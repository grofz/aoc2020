  module day02_mod
    implicit none
    private
    public :: feed_t, password_t

    type feed_t
      integer :: fid = 0
      logical :: eof = .true.
      character(len=2000) :: line ! pre-read line from the file
    contains
      procedure :: open => feed_open
      procedure :: iseof => feed_iseof
      procedure :: get => feed_get
    end type

    type password_t
      integer             :: i0, i1
      character(len=1)    :: ch
      character(len=1000) :: ps
    contains
      procedure :: set => password_set
      procedure :: isvalid => password_isvalid
      procedure :: isvalid_b => password_isvalid_b
    end type

  contains

    subroutine password_set(this, line)
      class(password_t), intent(out) :: this
      character(len=*), intent(in) :: line

      character(len=500) :: d1, d2

      read(line,*) this%i0, d1, this%i1, d2, this%ps
      this%ch = d2(1:1)
    end subroutine


    pure logical function password_isvalid(this) result(isvalid)
      class(password_t), intent(in) :: this

      integer :: i, n, nf

      n = len(trim(this%ps))
      nf = 0
      do i=1,n
        if (this%ps(i:i)==this%ch) nf = nf+1
      end do
      if (nf >= this%i0 .and. nf <= this%i1) then
        isvalid = .true.
      else
        isvalid = .false.
      end if
    end function


    pure logical function password_isvalid_b(this) result(isvalid)
      class(password_t), intent(in) :: this
      logical :: l1, l2

      associate (i=>this%i0, j=>this%i1, ps=>this%ps, ch=>this%ch)
        l1 = ps(i:i) == ch
        l2 = ps(j:j) == ch
      end associate         
      isvalid = (l1 .and. .not. l2) .or. (.not. l1 .and. l2)
    end function


    subroutine feed_open(this, filename)
      class(feed_t), intent(out) :: this
      character(len=*), intent(in) :: filename

      open(newunit=this % fid, file=filename, status='old')
      call try_read(this)
    end subroutine


    subroutine feed_get(this, line) 
      class(feed_t), intent(inout) :: this
      character(len=:), allocatable :: line

      call isolate_dash(this % line)
      line = trim(this % line)
      call try_read(this)
    end subroutine


    subroutine try_read(this)
      class(feed_t), intent(inout) :: this
      integer :: ios
      read(this % fid,'(a)',iostat=ios) this % line
      if (ios /= 0) then
        this % eof = .true.
        close(this % fid)
      else
        this % eof = .false.
      end if
    end subroutine try_read


    logical function feed_iseof(this)
      class(feed_t), intent(in) :: this
      feed_iseof = this % eof
    end function


    subroutine isolate_dash(str)
      character(len=*) :: str
!
! Transforms "123-456" to "123 - 456" so list directed formatting read can be later used
!
      integer :: i, n
      i = scan(str,'-')
      n = len(str)
      if (i == 0) return
      str(i+2:n) = ' '//str(i+1:n-1)
      str(i:i) = ' '
      str(i+1:i+1) = '-'
    end subroutine

  end module day02_mod

