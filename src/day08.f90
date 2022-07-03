module day08_mod
  implicit none
  private
  public computer_t

  type instruction_t
    character(len=3) :: cmd
    integer :: arg = 0
    logical :: was_called = .false.
  end type

  type computer_t
    private
    integer :: acc = 0, lab = 1
    type(instruction_t), allocatable :: list(:)
  contains
    procedure find_working_program
    procedure load_program
    procedure run_program
    procedure, private :: init_program
    procedure, private ::  step_program
  end type

contains

  subroutine find_working_program(this, final_acc)
    class(computer_t), intent(inout) :: this
    integer, intent(out) :: final_acc

    integer :: i, iexit

    do i=1, size(this%list)
      if (this%list(i)%cmd == 'acc') cycle
      call swap(this%list(i)%cmd)
      call this % run_program(iexit, final_acc)
      if (iexit > 0) exit
      call swap(this%list(i)%cmd)
    end do
    if (i>size(this%list)) then
      print '(a)', 'Correction failed'
    else
      print '("correction at line ",i0," [",a3,1x,i0,"]")', i, this%list(i)%cmd, this%list(i)%arg
    end if
  contains
    subroutine swap(cmd)
      character(len=*), intent(inout) :: cmd
      select case(cmd)
      case('nop')
        cmd = 'jmp'
      case('jmp')
        cmd = 'nop'
      end select
    end subroutine swap
  end subroutine find_working_program



  subroutine init_program(this)
    class(computer_t), intent(inout) :: this
    integer :: i
    this % acc = 0
    this % lab = 1
    do i=1,size(this % list)
      this % list(i) % was_called = .false.
    end do
  end subroutine init_program



  subroutine run_program(this, iexit, iacc)
    class(computer_t), intent(inout) :: this
    integer, intent(out) :: iexit, iacc

    call this % init_program()
    do
      call this % step_program(iexit)
      if (iexit /= 0) exit
    end do
    iacc = this % acc
  end subroutine run_program



  subroutine step_program(this, iexit)
    class(computer_t), intent(inout) :: this
    integer, intent(out) :: iexit

    if (this % lab == size(this % list)+1) then
      iexit = 1 ! normal exit
      return
    else if (this % lab < 1 .or. this % lab > size(this % list)+1) then
      error stop 'step: instruction invalid label'
    end if

    associate (lab=>this % lab, cmd=>this % list(this % lab))
      if (cmd % was_called) then
        iexit = -1 ! infinite loop
        return
      end if
      iexit = 0 ! still running
      select case(cmd % cmd)
      case('acc')
        this % acc = this % acc + cmd % arg
        lab = lab + 1
      case('jmp')
        lab = lab + cmd % arg
      case('nop')
        lab = lab + 1
      case default
        error stop 'step: invalid instruction'
      end select
      cmd % was_called = .true.
    end associate
  end subroutine step_program



  subroutine load_program(this, file)
    class(computer_t), intent(out) :: this
    character(len=*), intent(in) :: file

    character(len=3) :: cmd
    integer :: fid, ios, n, i, arg

    open(newunit=fid, file=file, status='old')
    n = 0
    do
      read(fid,*,iostat=ios) cmd, arg
      if (ios /= 0) exit
      n = n + 1
    end do
    allocate(this % list(n))
    rewind(fid)
    do i=1,n
      read(fid,*) this % list(i) % cmd, this % list(i) % arg
    end do
    close(fid)
  end subroutine load_program
end module day08_mod
