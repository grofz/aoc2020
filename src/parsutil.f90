  module parse_mod
    implicit none
    private
    public read_pattern, chop_string

  contains

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
      aa = transpose(aa)
    end function read_pattern


    subroutine chop_string(left, reg, right)
      character(len=*), intent(inout) :: left
      character(len=*), intent(in) :: reg
      character(len=*), intent(out) :: right

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

  end module parse_mod
