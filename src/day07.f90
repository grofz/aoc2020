module day07_mod
  implicit none
  private
  public color_t, rule_t, index_color, count_nbags, read_all_rules
  integer, parameter :: COLOR_LEN=20, UNK=-1

  type color_t
    character(len=COLOR_LEN) :: color1, color2
  contains
    procedure, private, non_overridable :: color_eq
    generic :: operator(==) => color_eq
    procedure :: set => color_set
  end type color_t

  type rule_item_t
    type(color_t) :: color
    integer :: n
    type(rule_item_t), pointer :: next => null()
  end type rule_item_t

  type rule_t
    type(color_t) :: base_color
    type(rule_item_t), pointer :: contained_bags => null()
    integer :: nbags = UNK
  contains
    procedure :: set => rule_set
    procedure :: findcolor => rule_findcolor
    final :: rule_finalize
  end type rule_t

contains

  pure logical function color_eq(a, b)
    class(color_t), intent(in) :: a, b
    if (a%color1 == b%color1 .and. a%color2 == b%color2) then
      color_eq = .true.
    else
      color_eq = .false.
    end if
  end function color_eq

  subroutine color_set(this, string)
    class(color_t), intent(out) :: this
    character(len=*), intent(in) :: string
    character(len=10) validation

    read(string,*) this % color1, this % color2, validation
    if (validation == 'bag' .or. validation == 'bags') then
      continue
    else
      error stop 'color_set - string not in correct format'
    end if
  end subroutine color_set



  subroutine rule_set(this, line)
    class(rule_t), intent(out) :: this
    character(len=*), intent(in) :: line
    integer :: i0, i1, i2, n
    character(len=:), allocatable :: wrk, bc_string, c_string
    type(rule_item_t), pointer :: new

    ! <color> contain [<n> <color>, ...] <n> <color>.
    ! <color> contain no other bags.

    ! disect base color
    i0 = index(line,' contain ')
    i1 = i0 + 9
    if (i0 == 0) error stop 'rule_set - contain keyword not found'
    bc_string = line(:i0-1)
    wrk = line(i1:)
    call this % base_color % set(bc_string)

    do
      ! disect color rule (separated by commas and dot)
      i0 = scan(wrk,',')
      i1 = scan(wrk,'.')
      if (i1 == 0) error stop 'rule_set - must end with dot'
      if (i0 == 0) then ! last item
        c_string = wrk(:i1-1)
      else
        c_string = wrk(:i0-1)
        wrk = trim(adjustl(wrk(i0+1:)))
      end if

      ! get number of bags
      if (c_string == 'no other bags') then
        n = 0
      else
        read(c_string,*) n
        i2 = scan(c_string,' ')
        c_string = trim(adjustl(c_string(i2+1:)))
      end if

      ! add contained bags to the rule
      if (n /= 0) then
        allocate(new)
        call new % color % set(c_string)
        new % n = n
        new % next => this % contained_bags
        this % contained_bags => new
        nullify(new)
      end if

      if (i0 == 0) exit
    end do
  end subroutine rule_set

  function rule_findcolor(this,color) result(n)
    class(rule_t), intent(in) :: this
    type(color_t), intent(in) :: color
    integer :: n
    type(rule_item_t), pointer :: curr

    n = 0
    curr => this % contained_bags
    do
      if (.not. associated(curr)) exit
      if (color == curr % color) then
        n = curr % n
        exit
      end if
      curr => curr % next
    end do
  end function rule_findcolor

  subroutine rule_finalize(this)
    type(rule_t), intent(inout) :: this
    type(rule_item_t), pointer :: curr, dele

    curr => this % contained_bags
    do
      if (.not. associated(curr)) exit
      dele => curr
      curr => curr % next
      deallocate(dele)
    end do
  end subroutine rule_finalize



  pure function index_color(list, color) result(ind)
    type(rule_t), intent(in) :: list(:)
    type(color_t), intent(in) :: color
    integer :: ind
    integer :: i
    ind = -1
    do i = 1, size(list)
      if (.not. (list(i)%base_color==color)) cycle
      ind = i
      exit
    end do
  end function index_color



  recursive subroutine count_nbags(what, list)
    integer, intent(in) :: what
    type(rule_t), intent(inout) :: list(:)
!
! To solve Part Two - count number of contained bags
!
    integer :: n, ind
    type(rule_item_t), pointer :: curr
    if (what<1 .or. what>size(list)) error stop 'count_nbags - out of range index'

    ! simple case - already counted (should not be called)
    if (list(what)%nbags /= UNK) return

   ! count trough the list of contained bags
    n = 0
    curr => list(what)%contained_bags
    do
      if (.not. associated(curr)) exit
      ind = index_color(list, curr%color)
      if (ind < 0) error stop 'count_nbags - color not found in the list'
      if (list(ind)%nbags == UNK) call count_nbags(ind, list)
      if (list(ind)%nbags == UNK) error stop 'count_nbags - something wrong'
      n = n + (1+list(ind)%nbags) * curr%n
      curr => curr%next
    end do
    list(what)%nbags = n
  end subroutine count_nbags



  subroutine read_all_rules(file, rules)
    character(len=*), intent(in) :: file
    type(rule_t), allocatable, intent(out) :: rules(:)
    integer :: fid, ios, n, i
    character(len=10000) :: line

    open(newunit=fid, file=file, status='old')
    ! count number of rules in the file
    n = 0
    do
      read(fid,'(a)',iostat=ios) line
      if (ios /= 0) exit
      n = n + 1
    end do

    ! read all rules
    rewind(fid)
    allocate(rules(n))
    do i = 1, n
      read(fid,'(a)') line
      call rules(i) % set(trim(line))
    end do
    close(fid)
  end subroutine read_all_rules

end module day07_mod
