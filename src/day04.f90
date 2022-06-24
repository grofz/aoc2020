  module day04_mod
    implicit none
    private
    public passport_t, read_to_line

    integer, parameter :: CHLEN = 20, NFS = 8
    character(len=3), parameter :: &
      FIELDS(*) = ['byr','iyr','eyr','hgt','hcl','ecl','pid','cid']

    type passport_t
      character(len=CHLEN) :: f(NFS)=''
    contains
      procedure :: read_from_line
      procedure :: is_passport, is_valid
    end type


  contains

    logical function is_passport(this)
      class(passport_t), intent(in) :: this
      integer :: i

      is_passport = .true.
      ! invalidate if any field is missing (except of "cid" which is optional)
      do i=1,NFS
        if (this % f(i) ==' ' .and. FIELDS(i) /= 'cid') then
          is_passport = .false.
          exit
        end if
      end do
    end function


    logical function is_valid(this,err)
      class(passport_t), intent(in) :: this
      character(len=:), allocatable, intent(out), optional :: err
      integer :: i
      character(len=:), allocatable :: err0

      is_valid = this % is_passport()
      if (.not. is_valid) then
        err0 = 'field is missing'
      else
        do i=1,NFS
          is_valid = valid(FIELDS(i), trim(this % f(i)), err0)
          if (.not. is_valid) exit
        end do
      end if
      if (present(err)) err = err0
    end function


    logical function valid(id, val, err)
      character(len=*), intent(in) :: id
      character(len=*), intent(in) :: val
      character(len=:), allocatable, intent(out) :: err

      integer :: n
      n = len(val)
      err = 'ok'
      select case(id)
      case('byr')
        valid = isdigit(val) >= 1920 .and. isdigit(val) <= 2002
        if (.not. valid) err ='byr out of range'
      case('iyr')
        valid = isdigit(val) >= 2010 .and. isdigit(val) <= 2020
        if (.not. valid) err ='iyr out of range'
      case('eyr')
        valid = isdigit(val) >= 2020 .and. isdigit(val) <= 2030
        if (.not. valid) err ='eyr out of range'
      case('hgt')
        if (val(n-1:n) == 'cm') then
          valid = hgt(val(:n-2)) >= 150 .and. hgt(val(:n-2)) <= 193
        else if (val(n-1:n) == 'in') then
          valid = hgt(val(:n-2)) >= 59 .and. hgt(val(:n-2)) <= 76
        else
          valid = .false.
        end if
        if (.not. valid) err ='hgt invalid'
      case('hcl')
        if (val(1:1) /= '#') then
          valid = .false.
        else if (n /= 7) then
          valid = .false.
        else if (verify(val(2:),'0123456789abcdef') /= 0) then
          valid = .false.
        else
          valid = .true.
        end if
        if (.not. valid) err ='hcl invalid'
      case('ecl')
        valid = val=='amb' .or. val=='blu' .or. val=='brn' .or. val=='gry' .or. val=='grn' .or. val=='hzl' .or.  val=='oth'
        if (.not. valid) err ='ecl invalid'
      case('pid')
        if (n /= 9) then
          valid = .false.
        else if (verify(val,'0123456789') /= 0) then
          valid = .false.
        else
          valid = .true.
        end if
        if (.not. valid) err ='pid invalid'
      case('cid')
        continue
      case default
        error stop 'uknown id'
      end select

    contains
      integer function isdigit(ch)
        character(len=*), intent(in) :: ch
        if (len(ch) /= 4) then
          isdigit = 0
        elseif (verify(ch,'1234567890') /= 0) then
          isdigit = 0
        else
          read(ch,*) isdigit
        end if
      end function

      integer function hgt(ch)
        character(len=*), intent(in) :: ch
        if (verify(ch,'1234567890') /= 0) then
          hgt = 0
        else
          read(ch,*) hgt
        end if
      end function
    end function valid



    subroutine read_from_line(this, line)
      class(passport_t), intent(out) :: this
      character(len=*), intent(in) :: line

      integer :: i, ito
      character(len=:), allocatable :: wrk, val
      character(len=3) :: id

      wrk = trim(line)
      do
        i=scan(wrk,':')
        if (i==0 .or. i<4) exit
        id = wrk(i-3:i-1)
        wrk = wrk(i+1:)
        i=scan(wrk,' ')
        if (i==0) then
          val=trim(wrk)
          wrk=''
        else
          val=trim(wrk(:i))
          wrk = wrk(i+1:)
        end if

        do i=1, size(FIELDS)
          if (FIELDS(i)==id) then
            this % f(i) = val
            exit
          end if
          if (i==size(FIELDS)) error stop 'read_from_line - invalid field id'
        end do
      end do
    end subroutine read_from_line


    subroutine read_to_line(fid, line)
      integer, intent(in) :: fid
      character(len=*), intent(out) :: line

      integer :: ios
      character(len=2000) :: oneline

      line = ''
      do
        read(fid,'(a)',iostat=ios) oneline
        if (ios /= 0 .or. oneline==' ') exit
        line = trim(line)//' '//trim(oneline)
      end do
    end subroutine

  end module day04_mod
