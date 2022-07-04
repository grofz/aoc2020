module day06_mod
  implicit none
  private
  public :: form_t, read_answers

  integer, parameter :: MAX_Q = 26
  integer, parameter :: I1 = selected_int_kind(2)

  type form_t
    logical(I1) :: ans(MAX_Q) = .false.
  contains
    procedure, private :: form_or, form_and
    generic :: operator(.for.) => form_or
    generic :: operator(.fand.) => form_and
  end type form_t

contains

  function form_new(string) result(new)
    type(form_t) :: new
    character(len=*), intent(in) :: string

    integer :: i, a

    do i=1, len_trim(string)
      a = iachar(string(i:i)) - iachar('a') + 1
      new % ans(a) = .true.
    end do
  end function form_new

  pure function form_or(a, b) result(res)
    class(form_t), intent(in) :: a, b
    type(form_t) :: res
    where (a % ans .or. b % ans) res % ans = .true.
  end function form_or

  pure function form_and(a, b) result(res)
    class(form_t), intent(in) :: a, b
    type(form_t) :: res
    where (a % ans .and. b % ans) res % ans = .true.
  end function form_and



  subroutine read_answers(file, forms, mode)
    character(len=*), intent(in) :: file
    type(form_t), allocatable, intent(out) :: forms(:)
    integer, intent(in) :: mode

    integer :: fid, ios, n, i, ig
    character(len=300) :: line

    open(newunit=fid, file=file, status='old')
    n = 1
    do
      read(fid,'(a)',iostat=ios) line
      if (ios /= 0) exit
      if (trim(line)=='') then
        n = n+1
      end if
    end do
    allocate(forms(n))
    rewind(fid)
    ig = 1
    i = 0
    do
      read(fid,'(a)',iostat=ios) line
      if (ios /= 0) exit
      if (trim(line)=='') then
        ig = ig + 1
        i = 0
        cycle
      end if
      if (i==0) then
        i = 1
        forms(ig) = form_new(trim(line))
      else
        select case(mode)
        case (1)
          forms(ig) = forms(ig) .for. form_new(trim(line))
        case (2)
          forms(ig) = forms(ig) .fand. form_new(trim(line))
        case default
          error stop 'read_answers: invalid mode'
        end select
      end if
    end do
    close(fid)
  end subroutine read_answers
end module day06_mod
