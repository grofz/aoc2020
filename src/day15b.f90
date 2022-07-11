!
! DAY15 - Faster implementation of the map. Uses just one big array
!

module day15b_mod
  implicit none
  private
  public game_t

  !integer, allocatable :: map(:)

  type game_t
    integer :: time
    integer :: prev_said
    integer, allocatable :: map(:)
  contains
    procedure :: init => game_init
    procedure :: one_round => game_next
  end type game_t

contains

  subroutine game_init(this, ainit, maxsize)
    class(game_t), intent(out) :: this
    integer, intent(in) :: ainit(:)
    integer, intent(in) :: maxsize
    integer :: istat, n0, i

    if (allocated(this%map)) deallocate(this%map)
    allocate(this%map(0:maxsize),stat=istat)
    if (istat /= 0) error stop 'game_init - allocation error'
    this%map = 0

    n0 = size(ainit)
    this %time = n0
    this%prev_said = ainit(n0)
    do i=1, n0-1
      if (this%map(ainit(i))/=0) error stop 'item repeats in the initialization list'
      this%map(ainit(i)) = i
    end do
  end subroutine game_init



  subroutine game_next(this)
    class(game_t), intent(inout) :: this
    integer :: val

    val = this%map(this%prev_said)
    this%map(this%prev_said) = this%time
    if (val == 0) then
      this%prev_said = 0
    else
      this%prev_said = this%time - val
    end if
    this%time = this%time + 1
  end subroutine game_next

end module day15b_mod
