module day14_mod
  use tree_m, only : rbtr_t, tree_mold, DAT_KIND
  use abstract_container, only : ERR_CONT_OK, ERR_CONT_ISNOT
  implicit none
  private
  public instruction_t, state_t, I8, read_all_instructions

  integer, parameter :: I8 = selected_int_kind(18)
  integer, parameter :: MSK_SIZE = 36, MAX_ADR = 99999

  ! address_t and address_ptr are stored using TREE module
  type address_t
    integer(I8) :: adr
    integer(I8) :: val
  end type address_t

  type address_ptr
    type(address_t), pointer :: ptr
  end type address_ptr

  ! instructions read from file and a representation of the computer
  type instruction_t
    private
    character(len=MSK_SIZE) :: msk=''
    integer(I8) :: adr=-1, val=-1
  contains
    procedure :: set => instruction_set
  end type instruction_t

  type state_t
    private
    character(len=MSK_SIZE) :: msk
    type(rbtr_t) :: memtree
  contains
    procedure :: init => state_init
    procedure :: clear => state_clear
    procedure :: doinstruction => state_doinstruction
    procedure :: summemory => state_summemory
    final :: state_final
  end type state_t

contains

  subroutine state_init(this)
    class(state_t), intent(inout) :: this
!
! Initialize the TREE in state_t object
!
    this % memtree = rbtr_t(userfun)
  end subroutine state_init


  subroutine state_clear(this)
    class(state_t), intent(inout) :: this
!
! Remove all entries in TREE
!
    integer(DAT_KIND), allocatable :: handle(:)
    type(address_ptr) :: adat
    do
      if (this%memtree % count() == 0) exit
      call this%memtree % Firstnode(handle)
      adat = transfer(this%memtree % Read(handle), adat)
      call this%memtree % remove(this%memtree % Read(handle))
      deallocate(adat % ptr)
    enddo
  end subroutine state_clear



  subroutine state_final(this)
    type(state_t), intent(inout) :: this
    call this % clear()
  end subroutine state_final



  subroutine state_doinstruction(this, instruction, version)
    class(state_t), intent(inout) :: this
    type(instruction_t), intent(in) :: instruction
    integer, intent(in) :: version
!
! Make single instruction: either update mask, or address value
!
    if (instruction % adr /= -1) then
      select case(version)
      case(1)
        call update_val(this%memtree, instruction%adr, mask_number(instruction%val, this%msk))
      case(2)
        call version2
      case default
        error stop 'do_instruction - uknown version'
      end select
    else if (instruction % msk /= '') then
      this%msk = instruction%msk
    else
      error stop 'do_instruction - invalid instruction'
    end if
  contains
    subroutine version2
      integer :: i, n
      n = count_X(this%msk)
      do i=0, 2**n-1
        call update_val(this%memtree, mask_number2(instruction%adr,this%msk,i),instruction%val)
      end do
    end subroutine
  end subroutine state_doinstruction



  function state_summemory(this) result(res)
    class(state_t), intent(in) :: this
    integer(I8) :: res
!
! Sum over all memory entriees in the TREE
!
    integer(DAT_KIND), allocatable :: handle(:)
    type(address_ptr) :: adat
    integer :: ierr

    ! Iterate and sum all nodes
    res = 0
    if (.not. this%memtree % Isempty()) then
      call this%memtree % Resetcurrent(handle)
      do
        adat = transfer(this%memtree % NextRead(handle, ierr), adat)
        if (ierr /= 0) exit
        res = res + adat % ptr % val
      enddo
    endif
  end function state_summemory



  subroutine instruction_set(this, string)
    class(instruction_t), intent(out) :: this
    character(len=*), intent(in) :: string
!
! New instruction from the string
!
    character(len=50) a1, a2, a3
    integer :: i1, i2

    read(string,*) a1, a2, a3
    if (a2 /= '=') error stop 'instruction_set - invalid format, = expected'
    select case(a1(1:3))
    case('mem')
      i1 = scan(a1,'[')
      i2 = scan(a1,']')
      if (i1 == 0 .or. i2 == 0) error stop 'instruction_set - pair of [...] expected'
      read(a1(i1+1:i2-1),*) this % adr
      read(a3,*) this % val
    case('mas')
      this % msk = trim(a3)
    case default
      error stop 'instruction_set - invalid format, mem or mas expected'
    end select
    !print *, 'adr  =', this%adr
    !print *, 'val  =', this%val
    !print *, 'mask = ['//this%msk//']'
    !print *
    if (this%adr > MAX_ADR) error stop 'Increase address size'
  end subroutine instruction_set



  subroutine read_all_instructions(file, list)
    character(len=*), intent(in) :: file
    type(instruction_t), allocatable, intent(out) :: list(:)
    integer :: fid, ios, n, i
    character(len=2000) line

    n = 0
    open(newunit=fid, status='old', file=file)
    do
      read(fid,'(a)',iostat=ios) line
      if (ios /= 0) exit
      n = n + 1
    end do
    allocate(list(n))
    rewind(fid)
    do i=1,n
      read(fid,'(a)') line
      !print *, '<'//trim(line)//'>'
      call list(i) % set(trim(line))
    end do
  end subroutine read_all_instructions



  pure function mask_number(num, msk) result(numres)
    integer(I8), intent(in) :: num
    character(len=MSK_SIZE), intent(in) :: msk
    integer(I8) :: numres, num0, bit
    integer :: i, bp

    num0 = num
    numres = 0
    do i = 1, MSK_SIZE
      ! decode bit from num
      bp = MSK_SIZE-i
      bit = num0 / 2_I8**bp
      if (bit > 1) error stop 'mask_number - number too big'
      num0 = num0 - bit*2_I8**bp

      ! modify bit according to mask
      select case(msk(i:i))
      case('1')
        bit = 1_I8
      case('0')
        bit = 0_I8
      case('X')
        continue
      case default
        error stop 'mask_number - invalid character in mask'
      end select

      ! encode to numres
      numres = numres + bit*2_I8**bp
    end do
  end function mask_number



  pure function mask_number2(num, msk, order) result(numres)
    integer(I8), intent(in) :: num
    character(len=MSK_SIZE), intent(in) :: msk
    integer, intent(in) :: order

    integer(I8) :: numres, num0, bit
    integer :: i, bp, ix, nx, xbits(MSK_SIZE)

    num0 = num
    numres = 0
    nx = count_X(msk)
    xbits(1:nx) = number_to_bits(order, nx)
    ix = 0
    do i = 1, MSK_SIZE
      ! decode bit from num
      bp = MSK_SIZE-i
      bit = num0 / 2_I8**bp
      if (bit > 1) error stop 'mask_number2 - number too big'
      num0 = num0 - bit*2_I8**bp

      ! modify bit according to mask
      select case(msk(i:i))
      case('1')
        bit = 1_I8
      case('0')
        continue
      case('X')
        ix = ix + 1
        bit = int(xbits(ix), kind=I8)
      case default
        error stop 'mask_number - invalid character in mask'
      end select

      ! encode to numres
      numres = numres + bit*2_I8**bp
    end do
  end function mask_number2


  pure integer function count_X(msk)
    character(len=MSK_SIZE), intent(in) :: msk
    integer :: i
    count_X = 0
    do i=1, MSK_SIZE
      if (msk(i:i)=='X') count_X = count_X + 1
    end do
  end function count_X


  pure function number_to_bits(num, nbits) result(bits)
    integer, intent(in) :: num, nbits
    integer :: bits(nbits)
    integer :: i, num0, bp
    num0 = num
    do i=1,nbits
      bp = nbits-i
      bits(i) = num0 / 2**bp
      num0 = num0 - bits(i) * 2**bp
    end do
  end function number_to_bits



  subroutine update_val(tree, adr, val)
    type(rbtr_t), intent(inout) :: tree
    integer(I8), intent(in) :: adr, val
!
! Look for address entries in the tree: if address exists, it is updated,
! otherwise new address entry is added.
!
    type(address_ptr) :: adat
    integer(DAT_KIND), allocatable :: handle(:)
    integer :: ierr

    allocate(adat%ptr)
    adat % ptr % adr = adr
    call tree % Find(transfer(adat,tree_mold), handle, ierr)
    select case(ierr)
    case(ERR_CONT_OK)
      deallocate(adat%ptr)
      adat = transfer(tree % Read(handle), adat)
      adat % ptr % val = val
    case(ERR_CONT_ISNOT)
      adat % ptr % val = val
      call tree % Add(transfer(adat,tree_mold))
    case default
      error stop 'update_val: uknow exit code'
    end select
  end subroutine update_val



  pure function userfun(a, b)
      use tree_m, only : DAT_KIND
      integer(DAT_KIND), intent(in) :: a(:), b(:)
      integer :: userfun
!
! user function attached to TREE storing address_t
!
      type(address_ptr) :: adat, bdat

      adat = transfer(a, adat)
      bdat = transfer(b, adat)

      if (adat % ptr % adr == bdat % ptr % adr) then
        userfun = 0
      elseif (adat % ptr % adr < bdat % ptr % adr) then
        userfun = 1
      else
        userfun = -1
      endif
    end function userfun

end module day14_mod
