  module day23_mod
    implicit none

    integer, parameter :: NO_OF_CUPS=9
    type cup_t
      logical :: is_on = .true.
      integer :: id = -1
      type(cup_t), pointer :: next => null()
    end type cup_t
    type cup_ptr
      type(cup_t), pointer :: ptr
    end type cup_ptr

    type circle_t
      type(cup_t), pointer :: current => null()
      type(cup_ptr) :: cups(NO_OF_CUPS)
    contains
      procedure :: print => circle_print
      procedure :: destination => circle_destination
      procedure :: move => circle_move
      !procedure :: isvalid => circle_isvalid
      final :: circle_finalize
    end type circle_t
    interface circle_t
      module procedure circle_new
    end interface circle_t

  contains

    subroutine circle_move(this)
      class(circle_t), intent(inout) :: this
      type(cup_t), pointer :: removed
      integer :: id_dest
      integer, parameter :: CUPS_REMOVED = 3

      call remove(this%current, CUPS_REMOVED, removed)
      id_dest = this % destination()
      call insert(this%cups(id_dest)%ptr, removed)
      this % current => this % current % next
    end subroutine circle_move


    subroutine remove(from, cnt, removed)
      type(cup_t), pointer, intent(in) :: from
      integer,  intent(in) :: cnt
      type(cup_t), pointer, intent(out) :: removed
!
! "cnt" cups behind the "from" cup are removed from the chain
!
      type(cup_t), pointer :: cur, prev
      integer :: i

      if (.not. associated(from%next)) error stop 'remove - no successor cups exist'
      removed => from % next
      cur => removed
      prev => null()
      do i=1, cnt
        if (.not. cur%is_on) error stop 'remove - removing cup already switched off'
        cur % is_on = .false.
        prev => cur
        cur => cur % next
        if (.not. associated(cur)) error stop 'remove - chain broken'
      end do

      ! break link behind last removed cup
      prev % next => null()
      ! move pointer behind removed cups
      from % next => cur
    end subroutine remove


    subroutine insert(behind, what)
      type(cup_t), pointer, intent(in) :: behind, what
!
! Insert chain "what" behind cup "behind"
!
      type(cup_t), pointer :: cur, before

      if (.not. associated(behind) .or. .not. associated(what)) error stop 'insert - null pointer'
      before => behind % next
      behind % next => what
      cur => what
      do
        if (cur%is_on) error stop 'insert - inserting cup that is on'
        cur % is_on = .true.
        if (associated(cur % next)) then
          cur => cur % next
        else
          cur % next => before
          exit
        end if
      end do
    end subroutine insert


    integer function circle_destination(this) result(cup_id)
      class(circle_t), intent(in) :: this
      if (.not. associated(this%current)) error stop 'circle_destination - no current'
      cup_id = this % current % id
      do
        cup_id = ind_prev(cup_id)
        if (this%cups(cup_id)%ptr%is_on) exit
      end do
    end function circle_destination


    type(circle_t) function circle_new(str) result(this)
      character(len=*), intent(in) :: str
      integer :: i, cups_id(NO_OF_CUPS), cup0, cup1

      do i = 1, NO_OF_CUPS
        allocate(this % cups(i)%ptr)
        this % cups(i)%ptr % id = i
        read(str(i:i),*) cups_id(i)
      end do
      this % current => this % cups(cups_id(1))%ptr

      ! make a chain of cups
      do i=1, NO_OF_CUPS
        cup1 = cups_id(i)
        cup0 = cups_id(ind_prev(i))
        this % cups(cup0)%ptr % next => this % cups(cup1)%ptr
      end do
    end function circle_new


    subroutine circle_finalize(this)
      type(circle_t), intent(inout) :: this
      integer :: i
      do i=1,NO_OF_CUPS
        if (associated(this%cups(i)%ptr)) deallocate(this%cups(i)%ptr)
      end do
      nullify(this%current)
    end subroutine circle_finalize


    subroutine circle_print(this)
      class(circle_t), intent(in) :: this
      integer :: i
      type(cup_t), pointer :: cup
      cup => this % current
      do i=1,NO_OF_CUPS
        if (.not. associated(cup)) exit
        !write(*,'(i1,l1,1x)',advance='no') cup % id, cup % is_on
        write(*,'(i1,1x)',advance='no') cup % id
        cup => cup % next
      end do
      if (i /= NO_OF_CUPS+1) then
        write(*,'(a)') " - and chain is incomplete"
      else
        write(*,*)
      end if
    end subroutine circle_print

!
! Helper functions to move between 1, 2, 3, 4, ..., 9, 1, 2 in the circle
!
    integer function ind_next(ind)
      integer, intent(in) :: ind
      ind_next = mod(ind, NO_OF_CUPS)+1
    end function ind_next

    integer function ind_prev(ind)
      integer, intent(in) :: ind
      ind_prev = NO_OF_CUPS - mod(NO_OF_CUPS-ind+1, NO_OF_CUPS)
    end function ind_prev

  end module day23_mod
