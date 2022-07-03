module day05_mod
  implicit none
  private
  public boarding_t, read_from_file, sort_bps

  type boarding_t
    integer :: row=-1, col=-1
    integer :: id=-1
  contains
    procedure :: new_from_string
  end type boarding_t
contains

  subroutine new_from_string(this, string)
    class(boarding_t), intent(out) :: this
    character(len=*), intent(in) :: string

    integer :: i

    this % row = 0
    do i = 1, 7
      select case(string(i:i))
      case ('F')
        continue
      case ('B')
        this % row = this % row + 2**(7-i)
      case default
        error stop 'new_from_string: F or B expected in string'
      end select
    end do

    this % col = 0
    do i = 1, 3
      select case(string(7+i:7+i))
      case('L')
        continue
      case ('R')
        this % col = this % col + 2**(3-i)
      case default
        error stop 'new_from_string: L or R expected in string'
      end select
    end do

    this % id = 8*this % row + this % col
  end subroutine new_from_string



  subroutine read_from_file(file, bps)
    character(len=*), intent(in) :: file
    type(boarding_t), allocatable, intent(out) :: bps(:)

    integer :: fid, ios, n, i
    character(len=10) :: line

    open(newunit=fid, file=file, status='old')
    n = 0
    do
      read(fid,'(a)',iostat=ios) line
      if (ios /= 0) exit
      n = n + 1
    end do
    allocate(bps(n))
    rewind(fid)
    do i=1, n
      read(fid,'(a)') line
      call bps(i) % new_from_string(line)
    end do
    close(fid)
  end subroutine read_from_file



  subroutine sort_bps(list)
    type(boarding_t), intent(inout) :: list(:)

    type(boarding_t) :: awrk
    integer :: i, j

    do i=2, size(list)
      j=i-1
      awrk = list(i)
      do
        if (j<1) exit
        if (list(j)%id <= awrk%id) exit
        list(j+1) = list(j)
        j = j-1
      end do
      list(j+1) = awrk
    end do
  end subroutine sort_bps

end module day05_mod
