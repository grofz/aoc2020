  module parse_mod
    implicit none
    private
    public read_pattern, chop_string, read_numbers, parse_array

  contains

    function read_numbers(file) result(a)
      character(len=*), intent(in) :: file
      integer, allocatable :: a(:)
!
! Read from file with a single integer on every line
!
      integer :: fid, n, i, ios

      ! count lines first...
      open(newunit=fid, file=file, status='old')
      n = 0
      do
        read(fid,*,iostat=ios) i
        if (ios /= 0) exit
        n = n + 1
      end do

      ! ...then re-read and store to "a"
      allocate(a(n))
      rewind(fid)
      do i=1, n
        read(fid,*) a(i)
      end do
      close(fid)
    end function



    function read_pattern(file) result(aa)
      character(len=*), intent(in) :: file
      character(len=1), allocatable :: aa(:,:)
!
! Read "character" 2D matrix from the file.
!
      integer :: fid, nrow, ncol, ios, i
      character(len=5000) :: line

      ! read no of rows, cols, and make sure all rows are of the same
      ! length
      open(newunit=fid, file=file, status='old')
      nrow = 0
      ncol = -1
      do
        read(fid,'(a)',iostat=ios) line
        if (ios /= 0) exit
        if (ncol==-1) ncol = len_trim(line)
        if (len_trim(line) /= ncol) &
          error stop 'read_pattern - not all lines have same length'
        nrow = nrow + 1
      end do

      allocate(aa(ncol,nrow))
      rewind(fid)
      do i=1,nrow
        read(fid,'(*(a))') aa(:,i)
      end do
      close(fid)
      aa = transpose(aa)
    end function read_pattern



    subroutine chop_string(left, reg, right)
      character(len=*), intent(inout) :: left
      character(len=*), intent(in) :: reg
      character(len=*), intent(out) :: right
!
! Chop string in two at "reg"-character: returns left and right part, "reg" character is discarded
!
      integer :: i, n
      i = scan(left, reg)
      n = len(left)
      if (i==0) then
        right = ''
      else
        right = left(i+1:)
        left = left(:i-1)
      end if
    end subroutine chop_string



    function parse_array(line, delim, nlen) result(arr)
      character(len=*), intent(in) :: line
      character(len=1), intent(in) :: delim
      integer, intent(in) :: nlen
      character(len=nlen), allocatable :: arr(:)
!
! ???
!
      character(len=len(line)) :: line0
      character(len=nlen), allocatable :: wrk(:)
      character(len=len(line)) :: a1, a2
      integer, parameter :: MAX_ITEMS=100
      integer :: nitems

      allocate(wrk(MAX_ITEMS))
      nitems=0
      line0 = line
      do
        call chop_string(line0, delim, a2)
        if (len_trim(line0) == 0) exit
        a1 = adjustl(line0)
        nitems = nitems+1
        wrk(nitems) = a1

        if(len_trim(a2)==0) exit
        line0 = adjustl(a2)
      end do

      allocate(arr(nitems))
      arr = wrk(1:nitems)
    end function parse_array

  end module parse_mod
