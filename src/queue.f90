  module queue_mod
    implicit none
    private
    public queue_t

    integer, parameter :: DEFAULT_QUEUE_SIZE = 50

    type queue_t
      private
      integer, allocatable :: aa(:)
      integer :: ifront=-1 ! pointing at the first item
      integer :: irear=-1  ! pointing behind the rear item
      integer :: nmax=-1   ! allocated size of array
      integer :: n=-1      ! actual number of items
    contains
      procedure :: size => queue_size
      procedure :: isempty => queue_isempty
      procedure :: export => queue_export
      procedure :: insert => queue_insert
      procedure :: remove => queue_remove
    end type queue_t
    interface queue_t
      module procedure queue_new
    end interface queue_t

  contains


!
! Procedures that implement the simple queue
!
    type(queue_t) function queue_new(arr, maxsize) result(this)
      integer, intent(in) :: arr(:)
      integer, intent(in), optional :: maxsize

      this % n = size(arr)
      this % nmax = DEFAULT_QUEUE_SIZE
      if (present(maxsize)) this % nmax = maxsize
      allocate(this % aa(this%nmax))
      if (size(arr) > this%nmax) error stop 'queue_new - queue size exceeded'
      this % ifront = 1
      this % irear = index_next(size(arr), this%nmax)
      this % n = size(arr)
      if (this%n > 0) &
          this % aa(this%ifront : index_prev(this%irear,this%nmax)) = arr
    end function queue_new


    integer function queue_size(this)
      class(queue_t), intent(in) :: this
      queue_size = this % n
    end function queue_size


    logical function queue_isempty(this)
      class(queue_t), intent(in) :: this
      queue_isempty = this % n == 0
    end function queue_isempty


    function queue_export(this) result(arr)
      class(queue_t), intent(in) :: this
      integer, allocatable :: arr(:)
      integer :: gap

      allocate(arr(this%n))
      if (this%n == 0) return
      gap = this%nmax - this%ifront + 1
      if (gap >= this%n) then
        ! items form a single chunk
        if (this%irear <= this%ifront .and. this%irear/=1) &
            error stop 'queue export - defensive check failed'
        arr = this % aa(this%ifront : index_prev(this%irear,this%nmax))
      else
        if (this%irear > this%ifront .or. this%irear==1) &
            error stop 'queue export - defensive check failed B'
        arr(1:gap) = this%aa(this%ifront : this%nmax)
        arr(gap+1:) = this%aa(1 : this%irear-1)
      end if
    end function queue_export


    subroutine queue_insert(this, item)
      class(queue_t), intent(inout) :: this
      integer, intent(in) :: item
      if (this%n == this%nmax) error stop 'queue_insert - queue is full'
      this % aa(this%irear) = item
      this % irear = index_next(this%irear, this%nmax)
      this % n = this % n + 1
    end subroutine queue_insert


    subroutine queue_remove(this, item)
      class(queue_t), intent(inout) :: this
      integer, intent(out) :: item
      if (this%n == 0) error stop 'queue_remove - queue is empty'
      item = this % aa(this%ifront)
      this % ifront = index_next(this%ifront, this%nmax)
      this % n = this % n - 1
    end subroutine queue_remove

!
! Helper function to move index in the array of size "n"
!
    integer function index_next(ind, n)
      integer, intent(in) :: ind, n
      index_next = mod(ind, n) + 1
    end function index_next

    integer function index_prev(ind, n)
      integer, intent(in) :: ind, n
      index_prev = n - mod(n-ind+1, n)
    end function index_prev
  end module queue_mod
