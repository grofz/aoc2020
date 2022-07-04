module day15_mod
  implicit none

  integer, parameter :: MAX_NUM = 30000000  !2020
  type game_t
    integer :: turn = 0
    integer :: when_said(0:MAX_NUM) = 0
    integer :: last_said = -1
    !integer :: init_numbers = 0
  contains
    procedure say_init
    procedure say_next
  end type game_t
contains

  subroutine say_init(this, number)
    class(game_t), intent(inout) :: this
    integer, intent(in) :: number

    this % turn = this % turn + 1
    if (number < 0 .or. number > MAX_NUM) &
        error stop 'say_init: number out of epected range'
    if (this % last_said /= -1) &
      this % when_said(this % last_said) = this % turn - 1
    this % last_said = number
print *, this % turn, number
  end subroutine say_init


  subroutine say_next(this, number)
    class(game_t), intent(inout) :: this
    integer, intent(out) :: number

    this % turn = this % turn + 1
    associate(last => this % last_said)
      if (this % when_said(last)==0) then ! never said
        number = 0
      else
        number = this % turn-1 - this % when_said(last)
      end if
      this % when_said(last) = this % turn-1
      last = number
    end associate
!print *, this % turn, number
  end subroutine say_next

end module day15_mod
