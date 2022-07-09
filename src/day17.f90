  module day17_mod
    use parse_mod, only : read_pattern
    implicit none
    private
    public conway_t

    character(len=1), parameter :: ACTIVE_CUBE='#', INACTIVE_CUBE='.'

    type conway_t
      logical, allocatable :: cube(:,:,:,:)
    contains
      procedure :: print => conway_print
      procedure :: cycle => conway_cycle
    end type conway_t
    interface conway_t
      module procedure conway_new
    end interface

  contains

    type(conway_t) function conway_new(file) result(this)
      character(len=*), intent(in) :: file
      character(len=1), allocatable :: init_pattern(:,:)

      init_pattern = read_pattern(file)
      init_pattern = transpose(init_pattern)
      if (count(init_pattern/=ACTIVE_CUBE .and. init_pattern/=INACTIVE_CUBE) /= 0) &
          error stop 'conway_new - pattern contains uknown characters'
      allocate(this%cube(size(init_pattern,1),size(init_pattern,2),1,1))
      where (init_pattern==ACTIVE_CUBE)
        this%cube(:,:,1,1)=.true.
      else where
        this%cube(:,:,1,1)=.false.
      end where
    end function conway_new


    subroutine conway_print(this)
      class(conway_t), intent(in) :: this
      integer :: imax, jmax, kmax, wmax, j, k, i, w
      imax = size(this%cube,1)
      jmax = size(this%cube,2)
      kmax = size(this%cube,3)
      wmax = size(this%cube,4)
      do w=1,wmax
      do k=1,kmax
        print '("z-slice ",i0,"  w-slice ",i0)', k, w
        do j=1,jmax
          print '(*(a1))', (dsp(this%cube(i,j,k,w)), i=1,imax)
        end do
      end do
      end do
      print *
    contains
      character(len=1) function dsp(val)
        logical, intent(in) :: val
        dsp = INACTIVE_CUBE
        if (val) dsp = ACTIVE_CUBE
      end function
    end subroutine conway_print


    function count_active_ngb(arr) result(ngb)
      logical, intent(in) :: arr(:,:,:,:)
      integer :: ngb(size(arr,1),size(arr,2),size(arr,3),size(arr,4))

      integer :: i, j, k, w, imax, jmax, kmax, wmax, i0, j0, k0, w0
      imax = size(arr,1)
      jmax = size(arr,2)
      kmax = size(arr,3)
      wmax = size(arr,4)
      ngb = 0
      do w=1,wmax
      do k=1,kmax
      do j=1,jmax
      do i=1,imax
        do i0 = max(1,i-1), min(i+1,imax)
        do j0 = max(1,j-1), min(j+1,jmax)
        do k0 = max(1,k-1), min(k+1,kmax)
        do w0 = max(1,w-1), min(w+1,wmax)
          if (i0==i .and. j0==j .and. k0==k .and. w0==w) cycle
          if (arr(i0,j0,k0,w0)) ngb(i,j,k,w) = ngb(i,j,k,w) + 1
        end do
        end do
        end do
        end do
      end do
      end do
      end do
      end do
    end function count_active_ngb


    subroutine conway_cycle(this,mode)
      class(conway_t), intent(inout) :: this
      integer, intent(in) :: mode
      logical, allocatable :: cube0(:,:,:,:)
      integer, allocatable :: active_ngb(:,:,:,:)
      integer :: d(4), ed(4)

      select case(mode)
      case(1)
        ed = [1, 1, 1, 0]
      case(2)
        ed = [1, 1, 1, 1]
      case default
        error stop 'invalid mode'
      end select

      d = shape(this%cube)
      allocate(cube0(d(1)+2*ed(1), d(2)+2*ed(2), d(3)+2*ed(3), d(4)+2*ed(4)))
      cube0 = .false.
      where (this%cube)
        cube0(ed(1)+1:ed(1)+d(1), ed(2)+1:ed(2)+d(2), &
              ed(3)+1:ed(3)+d(3), ed(4)+1:ed(4)+d(4)) = .true.
      end where

      active_ngb = count_active_ngb(cube0)
      this%cube = cube0

      ! active cube: If exactly 2 or 3 active ngb then cube remains active.
      ! otherwise, the cube becomes inactive.
      where(cube0 .and. .not. (active_ngb==2 .or. active_ngb==3))
        this % cube = .false.
      end where

      ! inactive cube: If exactly 3 active ngb then cube becomes active.
      ! Otherwise, the cube remains inactive.
      where(.not. cube0 .and. active_ngb==3)
        this % cube = .true.
      end where
    end subroutine conway_cycle

  end module day17_mod
