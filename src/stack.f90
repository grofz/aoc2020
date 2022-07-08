module stack_mod
  use kinds_m, only : IK => I8B
  implicit none
  private
  public stack_t

  type stack_t
    private
    integer(IK), allocatable :: s(:)
    integer :: nmax=-1, n=-1
  contains
    procedure :: push => stack_push
    procedure :: pop => stack_pop
    procedure :: isempty => stack_isempty
    procedure :: isfull => stack_isfull
    procedure :: size => stack_size
  end type stack_t
  interface stack_t
    module procedure stack_new
  end interface

contains

  pure type(stack_t) function stack_new(nmax)
    integer, intent(in) :: nmax
    stack_new % nmax = nmax
    stack_new % n = 0
    allocate(stack_new%s(nmax))
  end function

  pure subroutine stack_push(this, item)
    class(stack_t), intent(inout) :: this
    integer(IK), intent(in) :: item
    if (this % nmax == -1) error stop 'stack_push - stack not initialized'
    if (this % n == this % nmax) error stop 'stack_push - stack is full'
    this % n = this % n + 1
    this % s(this%n) = item
  end subroutine

  pure subroutine stack_pop(this, item)
    class(stack_t), intent(inout) :: this
    integer(IK), intent(out) :: item
    if (this % nmax == -1) error stop 'stack_pop - stack not initialized'
    if (this % n == 0) error stop 'stack_pop - stack is empty'
    item = this % s(this%n)
    this % n = this % n - 1
  end subroutine

  pure logical function stack_isempty(this)
    class(stack_t), intent(in) :: this
    if (this % nmax == -1) error stop 'stack_isempty - stack not initialized'
    stack_isempty = this % n == 0
  end function

  pure logical function stack_isfull(this)
    class(stack_t), intent(in) :: this
    if (this % nmax == -1) error stop 'stack_isfull - stack not initialized'
    stack_isfull = this % n == this % nmax
  end function

  pure integer function stack_size(this)
    class(stack_t), intent(in) :: this
    stack_size = this % n
  end function stack_size

end module stack_mod
