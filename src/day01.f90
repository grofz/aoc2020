  module day01_mod
    use quicksort_module, only : quicksort
    implicit none
    private
    public read_from_file, quicksort, find_entries, find_three_entries

  contains
    function read_from_file(file) result(a)
      character(len=*), intent(in) :: file
      integer, allocatable :: a(:)

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


    pure function find_entries(a, whatsum) result (entries)
      integer, intent(in) :: a(:)
      integer, intent(in) :: whatsum
      integer :: entries(2)
!
! Input array must be sorted.
!
      integer :: i, j

      i = 1
      j = size(a)
      do
        do
          if (a(j) <= whatsum-a(i)) exit
          j = j - 1
        end do
        if (a(i)+a(j)==whatsum) exit
        do
          if (a(i) >= whatsum-a(j)) exit
          i = i + 1
        end do
        if (a(i)+a(j)==whatsum) exit
        if (i > j) error stop 'find_entries - search failed'
      end do
      entries(1) = a(i)
      entries(2) = a(j)
    end function


    pure recursive function is_item(a, what) result(found)
      integer, intent(in) :: a(:)
      integer, intent(in) :: what
      logical :: found

      integer :: n

      n = size(a)
      if (n==1) then
        found = a(1)==what
      else if (what<a(1) .or. a(n)<what) then
        found = .false.
      else
        found = is_item(a(1:n/2),what) .or. is_item(a(n/2+1:n),what)
      end if
    end function


    pure function find_three_entries(a, whatsum) result(entries)
      integer, intent(in) :: a(:)
      integer, intent(in) :: whatsum
      integer :: entries(3)
!
! Input array must be sorted.
!
      integer :: i, j, n
      logical :: success

      success = .false.
      n = size(a)
      MAIN: do i = 1, n-2
        do j = i+1, n-1
          if (is_item(a(j+1:n), whatsum-a(i)-a(j))) then
            success = .true.
            exit MAIN
          end if
        end do
      end do MAIN
      if (.not. success) error stop 'find_three_entries - search failed'
      entries(1) = a(i)
      entries(2) = a(j)
      entries(3) = whatsum-a(i)-a(j)
    end function
  end module day01_mod
