module day12_mod
  implicit none
  private
  public :: ship_t, read_input_lines

  integer, parameter :: LINE_LEN=10
  integer, parameter :: WP_INIT(*) = [10, 1]

  integer, parameter :: R090_MAT(*,*) = reshape([ 0,-1,  1, 0], [2,2])
  integer, parameter :: R180_MAT(*,*) = reshape([-1, 0,  0,-1], [2,2])
  integer, parameter :: R270_MAT(*,*) = reshape([ 0, 1, -1, 0], [2,2])

  type ship_t
    integer :: heading = 90     ! facing East
    integer :: ship(2) = [0, 0] ! positive X => East, positibe Y => North
    integer :: wp(2) = WP_INIT  ! relative w.r.t to ship position
  contains
    procedure :: reset => ship_reset
    procedure :: autopilot => ship_autopilot
    procedure :: move => ship_move
    procedure :: rotate => ship_rotate
    procedure :: forward => ship_forward
    procedure :: move_to_wp => ship_move_to_wp
  end type ship_t

contains

  subroutine ship_reset(this)
    class(ship_t), intent(out) :: this
  end subroutine ship_reset


  subroutine ship_autopilot(this, instructions, mode)
    class(ship_t), intent(inout), target :: this
    character(len=*), intent(in) :: instructions(:)
    integer, intent(in) :: mode
    integer :: i
    if (mode /= 1 .and. mode /= 2) error stop 'ship_autopilot - mode must be 1 or 2'
    do i=1, size(instructions)
      select case(instructions(i)(1:1))
      case('N','E','W','S')
        call this % move(instructions(i), mode)
      case('L','R')
          call this % rotate(instructions(i), mode)
      case('F')
        if (mode==1) then
          call this % forward(instructions(i))
        else
          call this % move_to_wp(instructions(i))
        end if
      case default
        error stop 'ship_autopilot - unknown instruction'
      end select
      !print '(a)', trim(instructions(i))
      !print '("Ship position = [",i0,", ",i0,"]   waypoint = [",i0,", ",i0,"]")', &
      !  this%ship, this%wp
    end do
  end subroutine ship_autopilot


  subroutine ship_move(this, inst, mode)
    class(ship_t), intent(inout), target :: this
    character(len=*), intent(in) :: inst
    integer, intent(in) :: mode
!
! Move ship or its waypoint relative position
!
    integer :: val
    integer, pointer :: pos(:)

    if (mode==1) then
      pos => this % ship
    else
      pos => this % wp
    end if

    read(inst(2:),*) val
    select case (inst(1:1))
    case('N')
      pos(2) = pos(2) + val
    case('E')
      pos(1) = pos(1) + val
    case('W')
      pos(1) = pos(1) - val
    case('S')
      pos(2) = pos(2) - val
    case default
      error stop 'ship_move - invalid instruction'
    end select
  end subroutine ship_move


  subroutine ship_rotate(this, inst, mode)
    class(ship_t), intent(inout) :: this
    character(len=*), intent(in) :: inst
    integer, intent(in) :: mode
!
! Change ship's heading or rotate waypoint w.r.t. to the ship's position
!
    integer :: val

    read(inst(2:),*) val
    if (val /= 90 .and. val /= 180 .and. val /= 270) error stop 'ship_rotate - invalid turn value "'//inst//'"'
    select case (inst(1:1))
    case('R')
      continue
    case('L')
      val = 360 - val
    case default
      error stop 'ship_rotate - invalid instruction'
    end select

    if (mode == 1) then
      this % heading = mod(this % heading + val, 360)
    else
      select case(val)
      case(90)
        this%wp = matmul(R090_MAT, this%wp)
      case(180)
        this%wp = matmul(R180_MAT, this%wp)
      case(270)
        this%wp = matmul(R270_MAT, this%wp)
      end select
    end if
  end subroutine ship_rotate


  subroutine ship_forward(this, inst)
    class(ship_t), intent(inout) :: this
    character(len=*), intent(in) :: inst
!
! Move ship according to its actual heading
!
    character(len=LINE_LEN) :: inst0
    inst0 = inst
    select case (inst(1:1))
    case('F')
      select case(this % heading)
      case(0)
        inst0(1:1) = 'N'
      case(90)
        inst0(1:1) = 'E'
      case(180)
        inst0(1:1) = 'S'
      case(270)
        inst0(1:1) = 'W'
      case default
        error stop 'ship_forward - invalid heading'
      end select
    case default
      error stop 'ship_forward - invalid instruction'
    end select
    call this % move(trim(inst0),1)
  end subroutine ship_forward


  subroutine ship_move_to_wp(this, inst)
    class(ship_t), intent(inout) :: this
    character(len=*), intent(in) :: inst
!
! Move ship w.r.t it realtive waypoint position
!
    integer :: val
    if (inst(1:1) /= 'F') error stop 'ship_towp - expected F instruction'
    read(inst(2:),*) val
    this % ship = this % ship + val * this % wp
  end subroutine ship_move_to_wp


  subroutine read_input_lines(file, lines)
    character(len=*), intent(in) :: file
    character(len=:), intent(out), allocatable :: lines(:)
    integer :: fid, ios, n, i
    character(len=LINE_LEN) :: line

    open(newunit=fid, file=file, status='old')
    n = 0
    do
      read(fid,'(a)',iostat=ios) line
      if (ios /= 0) exit
      n = n + 1
    end do
    allocate(character(len=LINE_LEN) :: lines(n))
    rewind(fid)
    do i = 1, n
      read(fid,'(a)') lines(i)
    end do
    close(fid)
  end subroutine read_input_lines

end module day12_mod
