module day18_mod
  use kinds_m, only : I8 => I8B
  implicit none

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
  end subroutine read_input_lines


  pure function evaluate_string(str) result(val)
    character(len=*), intent(in) :: str
    integer(I8) :: val
    character(len=:), allocatable :: wrk_str
    wrk_str = str
    call evaluate(wrk_str, val)
  end function evaluate_string


  pure recursive subroutine evaluate(str, val)
    character(len=*), intent(inout) :: str
    integer(I8), intent(out) :: val

    logical :: expecting_operator
    integer(I8) :: a, b
    integer :: op

    expecting_operator = .false.
    val = 0
    op = OP_NIL
    do
      if (len_trim(str)==0) exit
      str = adjustl(str)

      if (is_digit(str(1:1)) .and. .not. expecting_operator) then
        select case(op)
        case (OP_NIL)
          read(str(1:1),*) a
          expecting_operator = .true.
          val = a
        case (OP_ADD)
          read(str(1:1),*) b
          val = val + b
        case (OP_MUL)
          read(str(1:1),*) b
          val = val * b
        end select
        str = adjustl(str(2:))
        expecting_operator = .true.

      else if (str(1:1)=='(' .and. .not. expecting_operator) then
        str = adjustl(str(2:))
        select case(op)
        case (OP_NIL)
          call evaluate(str, a)
          expecting_operator = .true.
          val = a
        case (OP_ADD)
          call evaluate(str, b)
          val = val + b
        case (OP_MUL)
          call evaluate(str, b)
          val = val * b
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
        return

      else
        error stop 'evaluate - unexpected character: "'//trim(str)//'"'
      end if
    end do
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
