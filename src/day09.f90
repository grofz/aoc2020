module day09_mod
  use kinds_m, only : I8B
  implicit none

  integer, parameter :: L_MARK=1, U_MARK=2

  type interval_t
    integer(I8B), allocatable :: arr(:)
    integer :: marks(2)=-1
    integer(I8B) :: sum
  contains
    procedure :: init => interval_init
    procedure :: move => interval_move
    procedure :: reset => interval_reset
    procedure :: verify => interval_verify
  end type interval_t

contains

  subroutine interval_init(this, file, marks)
    class(interval_t), intent(out) :: this
    character(len=*), intent(in) :: file
    integer, intent(in) :: marks(2)

    call read_numbers(file, this%arr)
    call this % reset(marks)
  end subroutine interval_init


  subroutine interval_reset(this, marks)
    class(interval_t), intent(inout) :: this
    integer, intent(in) :: marks(2)
    integer :: i

    if (any(marks > size(this%arr) .or. marks < 1) .or. marks(2)<marks(1)) &
        error stop 'interval_reset - wrong marks values'
    this % marks = marks
    this % sum = 0
    do i = marks(L_MARK), marks(U_MARK)
      this % sum = this % sum + this % arr(i)
    end do
  end subroutine interval_reset


  subroutine interval_move(this, id_mark, dir)
    class(interval_t), intent(inout) :: this
    integer, intent(in) :: id_mark, dir

    logical :: is_increasing, is_error

    ! verify input sanity
    if (id_mark < L_MARK .or. id_mark > U_MARK) error stop 'move - id_mark invalid'
    if (dir /= -1 .and. dir /= 1) error stop 'move - dir invalid'

    ! test marks limits
    is_error = .false.
    select case(id_mark)
    case(L_MARK)
      if (this%marks(L_MARK)+dir < 1 .or. &
          this%marks(L_MARK)+dir >= this%marks(U_MARK)) is_error = .true.
    case(U_MARK)
      if (this%marks(U_MARK)+dir > size(this%arr) .or. &
          this%marks(U_MARK)+dir <= this%marks(L_MARK)) is_error = .true.
    end select
    if (is_error) error stop 'interval_move - move invalid'

    ! increase or decrease the interval
    is_increasing = (dir == +1 .and. id_mark == U_MARK) .or. &
                    (dir == -1 .and. id_mark == L_MARK)

    if (is_increasing) then
      this%marks(id_mark) = this%marks(Id_mark) + dir
      this%sum = this%sum + this%arr(this%marks(id_mark))
    else
      this%sum = this%sum - this%arr(this%marks(id_mark))
      this%marks(id_mark) = this%marks(id_mark) + dir
    end if
  end subroutine interval_move


  logical function interval_verify(this) result(isvalid)
    class(interval_t), intent(in) :: this

    if (any(this%marks > size(this%arr) .or. this%marks < 1) .or. &
        this%marks(2)<this%marks(1)) then
      isvalid = .false.
      print *, 'Warning - marks are wrong'
      return
    end if
    isvalid = sum(this%arr(this%marks(L_MARK) : this%marks(U_MARK))) == this%sum
    if (.not. isvalid) print *, 'Warning - sum is wrong'
  end function interval_verify



  subroutine read_numbers(file, arr)
    character(len=*), intent(in) :: file
    integer(I8B), allocatable, intent(out) :: arr(:)

    integer :: fid, ios, i, n

    open(newunit=fid, file=file, status='old')
    n = 0
    do
      read(fid,*,iostat=ios) i
      if (ios /= 0) exit
      n = n + 1
    end do
    allocate(arr(n))
    rewind(fid)
    do i = 1, n
      read(fid,*) arr(i)
    end do
    close(fid)
  end subroutine read_numbers


  function is_num_valid(buffer, num) result(isvalid)
    integer(I8B), intent(in) :: buffer(:), num
    logical :: isvalid
!
! Test if there are two numbers in the buffer that add up to "num"
!
    integer :: i, j, n

    isvalid = .false.
    n = size(buffer)
    MAIN: do i=1,n-1
    do j=i+1,n
      if (buffer(i)+buffer(j) /= num) cycle
      isvalid = .true.
      exit MAIN
    end do
  end do MAIN
  end function is_num_valid

end module day09_mod
