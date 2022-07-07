module day19_mod
  use parse_mod, only : chop_string
  implicit none

  private
  public rule_t, is_valid_code, read_input, CODE_LEN

  integer, parameter :: OR_MAX=2, LIST_MAX=10, RULES_MAX = 500, CODE_LEN = 150
  integer, parameter :: TP_NULL=0, TP_CHAR=1, TP_OR=2
  type rule_t
    integer :: id=-1
    integer :: tp=TP_NULL
    integer :: ns(OR_MAX)=0
    integer :: s(OR_MAX,LIST_MAX)
    character(len=1) :: char='?'
  contains
    procedure :: set => rule_set
  end type rule_t

contains

  logical function is_valid_code(code, rules)
    character(len=*), intent(in) :: code
    type(rule_t), intent(in) :: rules(0:)

    character(len=:), allocatable :: wrk_code
    logical :: isok

    wrk_code = code
    call validate(0, wrk_code, rules, isok)
    is_valid_code = isok .and. len_trim(wrk_code)==0
  end function is_valid_code


  recursive subroutine validate(myid, code, rules, isok)
    integer, intent(in) :: myid
    character(len=*), intent(inout) :: code
    type(rule_t), intent(in) :: rules(0:)
    logical, intent(out) :: isok

    character(len=CODE_LEN) :: code0(OR_MAX)
    logical :: isok0(OR_MAX)
    integer :: i, j
    real :: rnd

    select case(rules(myid)%tp)
    case(TP_CHAR)
      ! character rule validation
      isok = .true.
      if (code(1:1) /= rules(myid)%char) isok = .false.
      code = code(2:)

    case(TP_OR)
      ! "|" rule validation
      isok0 = .false.
      do i=1, OR_MAX
        code0(i) = code
        do j=1, rules(myid)%ns(i)
          call validate(rules(myid)%s(i,j), code0(i), rules, isok0(i))
          if (.not. isok0(i)) exit
        end do
      end do
      isok = .false.
      do i=1, OR_MAX
        if (.not. isok0(i)) cycle
        isok = .true.
        code = code0(i)
        exit
      end do

      ! trick for part two
      ! if more than one solution, pick return value randomly
      ! wrong choice can result in false negatives only
      if (count(isok0)>1) then
        call random_number(rnd)
        i = ceiling(rnd*2.0)
        code = code0(i)
      end if

    case(TP_NULL)
      error stop 'validate - null rule'
    case default
      error stop 'validate - rule type uknown'
    end select
  end subroutine validate


  subroutine read_input(file, rules, codes)
    character(len=*), intent(in) :: file
    type(rule_t), allocatable, intent(out) :: rules(:)
    character(len=CODE_LEN), allocatable :: codes(:)

    integer :: fid, ios, idmax, n
    character(len=100) :: line
    type(rule_t) :: tmp, rules0(0:RULES_MAX)
    character(len=CODE_LEN), allocatable :: codes0(:)

    allocate(codes0(RULES_MAX))
    open(newunit=fid, status='old', file=file)
    idmax = 0
    do
      ! read rules...
      read(fid,'(a)',iostat=ios) line
      if (ios /= 0) exit
      if (len_trim(line)==0) exit
      call tmp%set(trim(line))
      if (tmp%id < 0 .or. tmp%id > RULES_MAX) error stop 'rule number out of range'
      rules0(tmp%id) = tmp
      if (tmp%id > idmax) idmax = tmp%id
    end do
    allocate(rules(0:idmax))
    rules = rules0(0:idmax)
print '("Rules = ",i0)', idmax

    ! ...now read codes
    n = 0
    do
      read(fid,'(a)',iostat=ios) line
      if (ios /= 0) exit
      n = n + 1
      if (n>RULES_MAX) error stop 'number of codes out of range'
      codes0(n) = line
    end do
    allocate(codes(n))
    codes = codes0(1:n)
print '("Codes = ",i0)', n
    close(fid)
  end subroutine read_input


  subroutine rule_set(this, string)
    class(rule_t), intent(out) :: this
    character(len=*), intent(in) :: string

    integer, parameter :: SLEN=50
    character(len=SLEN) :: a1, a2, a3, aa(OR_MAX)
    integer :: i, j
    ! 124: 12 13 | 109 7
    ! 124: "a"

    ! get rule number
    a1 = trim(adjustl(string))
    call chop_string(a1, ':', a2)
    read(a1,*) this % id

    ! get operands of "|"
    aa = ''
    do i = 1, OR_MAX
      call chop_string(a2, "|", a3)
      aa(i) = adjustl(a2)
      if (len_trim(a3)==0) exit
      a2 = adjustl(a3)
    end do

    ! get character rule
    if (aa(1)(1:1)=='"') then
      if (len_trim(aa(2))/=0) error stop 'rule_set - mixed rule'
      if (aa(1)(3:3)/='"') error stop 'rule_set - not one character'
      this % tp = TP_CHAR
      this % char = aa(1)(2:2)
      return
    end if

    ! get the lists
    this % tp = TP_OR
    do i=1, OR_MAX
      if (len_trim(aa(i))==0) exit
      a1 = aa(i)
      do j=1, LIST_MAX
        call chop_string(a1,' ',a2)
        read(a1,*) this % s(i,j)
        if (len_trim(a2)==0) exit
        a1 = adjustl(a2)
      end do
      this % ns(i) = j
    end do
  end subroutine rule_set

end module day19_mod
