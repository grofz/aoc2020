module day18_mod
  use kinds_m, only : I8 => I8B
  use stack_mod, only : stack_t
  implicit none
  private
  public I8, LINE_LEN, read_input_lines, evaluate_string

  integer, parameter :: OP_ADD=1, OP_MUL=2, OP_NIL=0
  integer, parameter :: LINE_LEN=200
  
contains

  subroutine read_input_lines(file, lines)
    character(len=*), intent(in) :: file
    character(len=LINE_LEN), allocatable, intent(out) :: lines(:)

    integer :: fid, ios, n, i
    character(len=LINE_LEN) :: line

    open(newunit=fid, file=file, status='old')
    n = 0
    do
      read(fid,'(a)',iostat=ios) line
      if (ios /= 0) exit
      n = n+1
    end do
    allocate(lines(n))
    rewind(fid)
    do i=1,n
      read(fid,'(a)') lines(i)
    end do
    close(fid)
  end subroutine read_input_lines


  pure function evaluate_string(str, mode) result(val)
    character(len=*), intent(in) :: str
    integer, intent(in) :: mode
    integer(I8) :: val
    character(len=:), allocatable :: wrk_str
    logical :: is_add_priority
    select case(mode)
    case(1) ! Part One - no precedence of operators
      is_add_priority = .false.
    case(2) ! Part Two - addition is evaluated first
      is_add_priority = .true.
    case default
      error stop 'evaluate_string - mode must be 1 or 2'
    end select
    wrk_str = str
    call evaluate(wrk_str, val, is_add_priority)
  end function evaluate_string


  pure recursive subroutine evaluate(str, val, is_add_priority)
    character(len=*), intent(inout) :: str
    integer(I8), intent(out) :: val
    logical, intent(in) :: is_add_priority

    integer, parameter :: MAX_STACK = 100
    logical :: expecting_operator
    integer(I8) :: a, b
    integer :: op
    type(stack_t) :: stack

    expecting_operator = .false.
    !val = 0
    op = OP_NIL
    stack = stack_t(MAX_STACK)
    do
      if (len_trim(str)==0) exit
      str = adjustl(str)

      if (is_digit(str(1:1)) .and. .not. expecting_operator) then
        select case(op)
        case (OP_NIL)
          read(str(1:1),*) a
          expecting_operator = .true.
          call stack % push(a)
        case (OP_ADD)
          call stack % pop(a)
          read(str(1:1),*) b
          a = a + b
          call stack % push(a)
        case (OP_MUL)
          read(str(1:1),*) b
          if (is_add_priority) then
            call stack % push(b)
          else
            call stack % pop(a)
            a = a * b
            call stack % push(a)
          end if
        end select
        str = adjustl(str(2:))
        expecting_operator = .true.

      else if (str(1:1)=='(' .and. .not. expecting_operator) then
        str = adjustl(str(2:))
        select case(op)
        case (OP_NIL)
          call evaluate(str, a, is_add_priority)
          expecting_operator = .true.
          call stack % push(a)
        case (OP_ADD)
          call stack % pop(a)
          call evaluate(str, b, is_add_priority)
          a = a + b
          call stack % push(a)
        case (OP_MUL)
          call evaluate(str, b, is_add_priority)
          if (is_add_priority) then
            call stack % push(b)
          else
            call stack % pop(a)
            a = a * b
            call stack % push(a)
          end if
        end select
        expecting_operator = .true.

      else if (is_operator(str(1:1)) .and. expecting_operator) then
        select case(str(1:1))
        case('+')
          op = OP_ADD
        case('*')
          op = OP_MUL
        end select
        str = adjustl(str(2:))
        expecting_operator = .false.

      else if (str(1:1)==')' .and. expecting_operator) then
        str = adjustl(str(2:))
        exit

      else
        error stop 'evaluate - unexpected character: "'//trim(str)//'"'
      end if
    end do

    if (is_add_priority) then
      if (stack % isempty()) error stop 'unexpected empty stack'
      val = 1_I8
      do
        call stack % pop(a)
        val = val * a
        if (stack % isempty()) exit
      end do
    else
      if (stack % size() /= 1) error stop 'unexpected stack size'
      call stack % pop(val)
    end if
  end subroutine evaluate


  pure logical function is_digit(ch)
    character(len=1), intent(in) :: ch
    is_digit = iachar(ch)>=iachar('0') .and. iachar(ch)<=iachar('9')
  end function is_digit


  pure logical function is_operator(ch)
    character(len=1), intent(in) :: ch
    is_operator = ch=='+' .or. ch=='*'
  end function is_operator

end module day18_mod
