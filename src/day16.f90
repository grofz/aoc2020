!
! DAY 16 - TICKET TRANSLATION
!
  module day16_mod
    use parse_mod, only : chop_string
    implicit none
    private
    public field_t, ticket_t, read_from_file
    public make_maybe_pos, eliminate_unknown_fields

    integer, parameter :: MAXLEN_NAME = 25, NO_VALID_INTERVALS = 2
    integer, parameter :: NF = 20 !3

    type ticket_t
      integer :: aa(NF)=-1
    contains
      procedure :: get_invalid => ticket_get_invalid
    end type ticket_t
    interface ticket_t
      module procedure ticket_new
    end interface ticket_t

    type field_t
      character(len=MAXLEN_NAME) :: label='?'
      integer :: lim0(NO_VALID_INTERVALS)=-1
      integer :: lim1(NO_VALID_INTERVALS)=-1
      integer :: pos=-1
      integer, allocatable :: maybe_pos(:)
    contains
      procedure :: isvalid => field_isvalid
    end type field_t
    interface field_t
      module procedure field_new
    end interface field_t

  contains

    integer function ticket_get_invalid(this, rules) result(inv_item)
      class(ticket_t), intent(in) :: this
      class(field_t), intent(in) :: rules(:)
!
! Return invalid item in the ticket or "-1" if all items are valid for at least
! one of the rules
!
      logical :: isvalid
      integer :: i_item, i_rule

      ! check that all ticket items are valid for at least on field
      inv_item = -1
      do i_item=1, size(this%aa)
        isvalid = .false.
        do i_rule=1,size(rules)
          if (rules(i_rule)%isvalid(this%aa(i_item))) then
            isvalid = .true.
            exit
          end if
        end do
        if (isvalid) cycle
        if (inv_item /=-1) error stop 'more than one invalid fields in one ticket'
        inv_item = this%aa(i_item)
      end do
    end function ticket_get_invalid


    subroutine read_from_file(file, fields, myticket, tickets)
      character(len=*), intent(in) :: file
      type(field_t), allocatable :: fields(:)
      type(ticket_t), intent(out) :: myticket
      type(ticket_t), intent(out), allocatable :: tickets(:)
      type(ticket_t) :: newticket
      integer :: fid, ios
      character(len=200) :: line
      type(field_t) :: newfield

      open(newunit=fid, file=file, status='old')
      allocate(fields(0))
      do
        read(fid,'(a)',iostat=ios) line
        if (ios /= 0) exit
        if (len_trim(line)==0) exit
        newfield = field_t(trim(line))
        fields = [fields, newfield]
      end do

      ! now read my ticket
      read(fid,'(a)') line
      if (trim(line) /= 'your ticket:') error stop 'your ticket expected'
      read(fid,'(a)') line
      myticket = ticket_t(trim(line), size(fields))

      ! read other tickets
      allocate(tickets(0))
      read(fid,'(a)') line
      read(fid,'(a)') line
      if (trim(line) /= 'nearby tickets:') error stop 'nearby tickets expected'
      do
        read(fid,'(a)',iostat=ios) line
        if (ios /= 0) exit
        newticket = ticket_t(trim(line), size(fields))
        tickets = [tickets, newticket]
      end do
    end subroutine read_from_file


    function ticket_new(str, n) result(this)
      character(len=*), intent(in) :: str
      integer, intent(in) :: n
      type(ticket_t) :: this
      if (size(this%aa) /= n) error stop 'please adjust NF in day16_mod'
      read(str,*) this % aa
    end function ticket_new


    type(field_t) function field_new(str) result(this)
      character(len=*), intent(in) :: str
      character(len=200) :: a1, a2

      ! get field label
      a1 = adjustl(str)
      call chop_string(a1, ':', a2)
      this % label = trim(adjustl(a1))

      ! get the ranges
      a1 = adjustl(a2)
      call chop_string(a1,'-',a2)
      read(a1,*) this % lim0(1)
      a1 = adjustl(a2)
      call chop_string(a1,' ',a2)
      read(a1,*) this % lim1(1)
      a1 = adjustl(a2)
      call chop_string(a1,' ',a2)
      if (trim(adjustl(a1)) /= 'or') error stop 'field_new - OR expected '//trim(a1)
      a1 = adjustl(a2)
      call chop_string(a1,'-',a2)
      read(a1,*) this % lim0(2)
      read(a2,*) this % lim1(2)
!print '("<",a,"> ",i3," - ",i3," or ",i3," - ",i3)', this%label, this%lim0(1), this%lim1(1), this%lim0(2), this%lim1(2)
    end function field_new


    elemental logical function field_isvalid(this, item) result(isvalid)
      class(field_t), intent(in) :: this
      integer, intent(in) :: item
      integer :: i
      isvalid = .false.
      do i=1, NO_VALID_INTERVALS
        if (this%lim0(i) <= item .and. item <= this%lim1(i)) then
          isvalid = .true.
          exit
        end if
      end do
    end function field_isvalid


    subroutine make_maybe_pos(fields, tickets)
      class(field_t), intent(inout) :: fields(:)
      class(ticket_t), intent(in) :: tickets(:)

      integer :: ifield, iticket, ipos
      logical :: all_tickets_valid

      do ifield = 1, size(fields)
        associate(ff => fields(ifield))
          if (allocated(ff%maybe_pos)) deallocate(ff%maybe_pos)
          allocate(ff%maybe_pos(0))
          do ipos = 1, NF
            all_tickets_valid = .true.
            do iticket = 1, size(tickets)
              if (ff%isvalid(tickets(iticket) % aa(ipos))) cycle
              all_tickets_valid = .false.
              exit
            end do
            if (all_tickets_valid) ff%maybe_pos = [ff%maybe_pos, ipos]
          end do
        end associate
      end do
    end subroutine make_maybe_pos


    subroutine eliminate_unknown_fields(fields)
      type(field_t), intent(inout) :: fields(:)
      integer :: i, i1, left_to_eliminate, ipos_known

      MAINLOOP: do
        !call sort_fields(fields,1)
        left_to_eliminate = count(fields % pos == -1)
        if (left_to_eliminate == 0) exit MAINLOOP

        ! Find first field with single item in "maybe_pos"
        ! Skip fields whose position was already identified
        do i=1, size(fields)
          if (fields(i)%pos > 0) cycle
          if (size(fields(i)%maybe_pos)==1) exit
        end do
        if (i==size(fields)+1) error stop 'eliminate - algorithm failed'

        ! Remove this positioin from other fields
        ! Assert no other field was identified with same position
        i1 = i
        ipos_known = fields(i1)%maybe_pos(1)
        fields(i1)%pos = ipos_known
        do i=1, size(fields)
          if (i==i1) cycle ! ignore itself
          if (fields(i)%pos > 0) then
            if (fields(i)%pos == ipos_known) error stop 'eliminate - 2 fields with same position'
          else
            call remove_item_from_array(fields(i)%maybe_pos, ipos_known)
          end if
        end do

        ! Continue until the positions of all fields identified
      end do MAINLOOP
      !call sort_fields(fields,2)
    end subroutine eliminate_unknown_fields


    subroutine remove_item_from_array(aa, item)
      integer, allocatable, intent(inout) :: aa(:)
      integer, intent(in) :: item
!
! Remove item from array. Do nothing if item is not in array.
!
      integer, allocatable :: new(:)
      if (count(aa == item)==0) return
      new = pack(aa, aa /= item)
      call move_alloc(new, aa)
      if (size(aa)<1) print *, 'remove_item_from_array WARNING - last item removed'
    end subroutine remove_item_from_array


    subroutine sort_fields(fields,order)
      type(field_t), intent(inout) :: fields(:)
      integer, intent(in) :: order
!
! Sort fields - not needed. Was used just for debugging
!
      integer :: i, j, iwrk
      type(field_t) :: tmp

      do i=2, size(fields)
        tmp = fields(i)
        j = i-1
        do
          if (j==0) exit
          select case (order)
          case(1)
            if (size(fields(j)%maybe_pos)<=size(tmp%maybe_pos)) exit
          case default
            if (fields(j)%pos<=tmp%pos) exit
          end select
          fields(j+1) = fields(j)
          j = j-1
        end do
        fields(j+1)=tmp
      end do
    end subroutine sort_fields


  end module day16_mod
