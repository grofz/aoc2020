module day14_mod
  implicit none

  integer, parameter :: I8 = selected_int_kind(18)
  integer, parameter :: MSK_SIZE = 36, MAX_ADR = 99999

  type instruction_t
    character(len=MSK_SIZE) :: msk=''
    integer(I8) :: adr=-1, val=-1
  contains
    procedure :: set => instruction_set
  end type instruction_t

  type state_t
    character(len=MSK_SIZE) :: msk
    integer(I8) :: mem(MAX_ADR) = 0
  contains
    procedure :: do_instruction
    procedure :: sum_memory
  end type state_t

contains

  subroutine do_instruction(this, instruction)
    class(state_t), intent(inout) :: this
    type(instruction_t), intent(in) :: instruction

    if (instruction % adr /= -1) then
      this%mem(instruction%adr) = mask_number(instruction%val, this%msk)
    else if (instruction % msk /= '') then
      this%msk = instruction%msk
    else
      error stop 'do_instruction - invalid instruction'
    end if
  end subroutine do_instruction

  function sum_memory(this) result(res)
    class(state_t), intent(in) :: this
    integer(I8) :: res
    res = sum(this%mem)
  end function sum_memory



  subroutine instruction_set(this, string)
    class(instruction_t), intent(out) :: this
    character(len=*), intent(in) :: string
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



  function mask_number(num, msk) result(numres)
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
end module day14_mod
