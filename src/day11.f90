module day11_mod
  use parse_mod, only : read_pattern
  implicit none
  private
  public seatplan_t, OCCUPIED_SEAT

  integer, parameter :: I1B = selected_int_kind(2)
  character(len=1), parameter :: FREE_SEAT='L', OCCUPIED_SEAT='#'

  type seatplan_t
    character(len=1), allocatable :: plan(:,:)
    integer(I1B), allocatable :: ngb_occupied(:,:)
    integer :: mode=-1 ! "1" for Part 1 rules or "2" for Part 2 rules
  contains
    procedure :: print => seatplan_print
    procedure :: onestep => seatplan_onestep
  end type seatplan_t
  interface seatplan_t
    module procedure seatplan_new
  end interface seatplan_t

contains

  type(seatplan_t) function seatplan_new(file,mode) result(this)
    character(len=*), intent(in) :: file
    integer, intent(in) :: mode
    if (mode<1 .or. mode>2) error stop 'seatplan_new - mode must be 1 or 2'
    this%mode = mode
    this%plan = read_pattern(file)
    associate(rows=>size(this%plan,1), cols=>size(this%plan,2))
      allocate(this % ngb_occupied(rows,cols))
    end associate
    this % ngb_occupied = count_ngb(this % plan, this%mode)
    if (sum(this%ngb_occupied)/=0) error stop 'some seats occupied initialy'
  end function seatplan_new


  subroutine seatplan_print(this)
    class(seatplan_t), intent(in) :: this
    integer :: row, maxrow
    maxrow = size(this%plan, dim=1)
    do row = 1, maxrow
      print '(*(a1))', this%plan(row,:)
    end do
  end subroutine seatplan_print


  function count_ngb(plan, mode) result(ngb)
    character(len=1), intent(in) :: plan(:,:)
    integer, intent(in) :: mode
    integer(I1B) :: ngb(size(plan,1),size(plan,2))
!
! Count adjacent (mode=1) or visible (mode=2) occupied seats
!
    integer :: i, j, maxi, maxj, i0, j0, ori
    integer, parameter :: RAY(*,*) = reshape([0,1, 1,1, 1,0, 1,-1, 0,-1, -1,-1, -1,0, -1,1], [2,8])

    maxi = size(plan,1)
    maxj = size(plan,2)
    do i=1,maxi
    do j=1,maxj
      ngb(i,j) = 0
      select case(mode)
      case(1)
        do ori=1,8
          i0 = i + RAY(1,ori)
          j0 = j + RAY(2,ori)
          if (i0 < 1 .or. i0 > maxi .or. j0 < 1 .or. j0 > maxj) cycle
          if (plan(i0,j0)==OCCUPIED_SEAT) ngb(i,j) = ngb(i,j) + 1_I1B
        end do
      case(2)
        do ori=1,8
          i0=i; j0=j
          do
            i0 = i0 + RAY(1,ori)
            j0 = j0 + RAY(2,ori)
            if (i0 < 1 .or. i0 > maxi .or. j0 < 1 .or. j0 > maxj) exit
            if (plan(i0,j0) == FREE_SEAT) exit
            if (plan(i0,j0) /= OCCUPIED_SEAT) cycle
            ngb(i,j) = ngb(i,j) + 1_I1B ! count visible OCCUPIED_SEATS
            exit
          end do
        end do
      case default
        error stop 'count_ngb - uknown mode'
      end select
    end do
    end do
  end function count_ngb


  subroutine seatplan_onestep(this, nswaps)
    class(seatplan_t), intent(inout) :: this
    integer, intent(out) :: nswaps
    character(len=1), allocatable :: plan0(:,:)
    integer :: threshold

    plan0 = this % plan
    select case(this%mode)
    case(1)
      threshold = 4
    case(2)
      threshold = 5
    end select

    ! empty seat with no adjacent occupied seats becomes occupied
    where(plan0 == FREE_SEAT .and. this%ngb_occupied==0)
      this % plan = OCCUPIED_SEAT
    ! occupied seat with 4/5 or more adjacent occupid seats becomes empty
    else where(plan0 == OCCUPIED_SEAT .and. this%ngb_occupied>=threshold)
      this % plan = FREE_SEAT
    end where
    nswaps = count(plan0 /= this%plan)
    this%ngb_occupied = count_ngb(this%plan, this%mode)
  end subroutine seatplan_onestep

end module day11_mod
