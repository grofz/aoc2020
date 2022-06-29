  module day24_mod
    implicit none
    private
    public tile_t, floor_t
    public read_from_file

                                      ! w  nw  ne  e   se  sw
    integer, parameter :: MOVE_X(*) = [-2, -1, +1, +2, +1, -1]
    integer, parameter :: MOVE_Y(*) = [ 0, +1, +1,  0, -1, -1]
    integer, parameter :: WEST=1, NWEST=2, NEAST=3, EAST=4, SEAST=5, SWEST=6
    integer, parameter :: WHITE_COLOR=0, BLACK_COLOR=1

    type tile_t
      integer :: x(2)=0
      integer :: color=WHITE_COLOR
    contains
      procedure :: isvalid => tile_isvalid
      procedure :: move => tile_move
      procedure :: blackngb => tile_count_black_ngb
    end type tile_t

    interface tile_t
      module procedure tile_from_string
    end interface


    type floor_t
      type(tile_t), allocatable :: arr(:,:)
    contains
      procedure :: init => floor_init
      procedure :: expand => floor_expand
      procedure :: blacks => floor_count_blacks
      procedure :: print => floor_print
    end type floor_t

  contains

    subroutine init_index(arr)
      type(tile_t), intent(inout), allocatable :: arr(:,:)
      integer :: i, j

      do i=lbound(arr, dim=1), ubound(arr, dim=1)
      do j=lbound(arr, dim=2), ubound(arr, dim=2)
        arr(i,j)%x(1) = i
        arr(i,j)%x(2) = j
      end do
      end do
    end subroutine



    subroutine floor_init(this, tiles)
      class(floor_t), intent(out) :: this
      class(tile_t), intent(in) :: tiles(:)

      integer :: x0, x1, y0, y1, i, j

      x0 = 0
      x1 = 0
      y0 = 0
      y1 = 0
      do i=1,size(tiles)
        if (tiles(i)%x(1) < x0) x0 = tiles(i)%x(1)
        if (tiles(i)%x(1) > x1) x1 = tiles(i)%x(1)
        if (tiles(i)%x(2) < y0) y0 = tiles(i)%x(2)
        if (tiles(i)%x(2) > y1) y1 = tiles(i)%x(2)
      end do
      allocate(this%arr(x0-2:x1+2,y0-1:y1+1))
      call init_index(this%arr)

      ! flip tiles that are listed in "tiles"
      do i=1,size(tiles)
        associate(x=>tiles(i)%x(1), y=>tiles(i)%x(2))
          associate(color=>this%arr(x,y)%color)  
            color = mod(color+1,2)
          end associate
        end associate
      end do
    end subroutine floor_init



    subroutine floor_expand(this)
      class(floor_t), intent(inout) :: this

      type(tile_t), allocatable :: arrcopy(:,:)
      integer :: x0, x1, y0, y1, ex, ey
      integer :: i, j, blacks

      x0 = lbound(this%arr,dim=1)
      y0 = lbound(this%arr,dim=2)
      x1 = ubound(this%arr,dim=1)
      y1 = ubound(this%arr,dim=2)

      ! test, if array size must be increased
      ex = 0
      ey = 0
      if (test_border()) then
        ex = 2
        ey = 1
      end if

      ! copy floor to "arrcopy" and reallocate/resize floor
      allocate(arrcopy(x0-ex:x1+ex,y0-ey:y1+ey))
      call init_index(arrcopy)
      arrcopy(x0:x1,y0:y1) = this%arr
      deallocate(this%arr)
      allocate(this%arr(x0-ex:x1+ex,y0-ey:y1+ey))
      call init_index(this%arr)

      ! count number of black neighbours, mirror or flip
      do i=x0,x1
      do j=y0,y1
        if (.not. arrcopy(i,j) % isvalid()) cycle
        blacks = arrcopy(i,j) % blackngb(arrcopy)
        if (arrcopy(i,j) % color == BLACK_COLOR) then
          if (blacks==0 .or. blacks>2) then
            this%arr(i,j)%color = WHITE_COLOR
          else
            this%arr(i,j)%color = BLACK_COLOR
          end if
        else ! color == WHITE_COLOR
          if (blacks==2) then
            this%arr(i,j)%color = BLACK_COLOR
          else
            this%arr(i,j)%color = WHITE_COLOR
          end if
        end if
      end do
      end do
    contains
      logical function test_border()
        test_border = .true.
        do i=x0,x1
        do j=y0,y1
          if (.not. this%arr(i,j) % isvalid()) cycle
          if (.not. (i==x0 .or. i==x1 .or. j==y0 .or. j==y1)) cycle
          ! [i,j] is at the border
          blacks = this%arr(i,j) % blackngb(this%arr)
          if (blacks==2) return
        end do
        end do
        test_border = .false.
      end function
    end subroutine floor_expand



    function floor_count_blacks(this) result(blacks)
      class(floor_t), intent(in) :: this
      integer :: blacks

      integer :: i, j
      blacks = 0
      do i = lbound(this%arr, dim=1), ubound(this%arr, dim=1)
      do j = lbound(this%arr, dim=2), ubound(this%arr, dim=2)
        if (.not. this%arr(i,j) % isvalid()) cycle
        if (this%arr(i,j)%color == BLACK_COLOR) blacks = blacks+1
      end do
      end do
    end function floor_count_blacks



    function tile_count_black_ngb(this, arr) result(blacks)
      integer :: blacks
      class(tile_t), intent(in) :: this
      type(tile_t), intent(in), allocatable :: arr(:,:)

      integer :: i
      type(tile_t) :: ngb

      if (.not. this % isvalid()) &
          error stop 'count_black_ngb - with invalid tile'
      blacks = 0
      do i=1,6
        ngb = tile_ngb(this, i, arr)
        if (ngb % color == BLACK_COLOR) blacks = blacks+1
      end do
    end function tile_count_black_ngb



    function tile_ngb(this, direction, arr) result(ngb)
      type(tile_t) :: ngb
      class(tile_t), intent(in) :: this
      integer, intent(in) :: direction
      type(tile_t), intent(in), allocatable :: arr(:,:)

      integer :: x0, x1, y0, y1
      type(tile_t) :: tile, out_of_border

      if (direction < 1 .or. direction > 6) &
          error stop 'tile_move - direction is invalid'
      x0 = lbound(arr,dim=1)
      y0 = lbound(arr,dim=2)
      x1 = ubound(arr,dim=1)
      y1 = ubound(arr,dim=2)

      tile = this
      call tile % move(direction)
      associate(x=>tile%x(1), y=>tile%x(2))
        if (x < x0 .or. x > x1 .or. y < y0 .or. y > y1) then
          ngb = out_of_border     
        else
          ngb = arr(x,y)
        end if
      end associate
    end function tile_ngb



    subroutine tile_move(this, direction)
      class(tile_t), intent(inout) :: this
      integer, intent(in) :: direction
      if (direction < 1 .or. direction > 6) &
          error stop 'tile_move - direction is invalid'
      this % x(1) = this % x(1) + MOVE_X(direction)
      this % x(2) = this % x(2) + MOVE_Y(direction)
    end subroutine tile_move



    logical function tile_isvalid(this)
      class(tile_t), intent(in) :: this
      associate(x=>this%x(1), y=>this%x(2))
        if (mod(abs(y), 2)==0) then ! even rows
          tile_isvalid = mod(abs(x), 2)==0
        else ! odd rows
          tile_isvalid = mod(abs(x), 2)==1
        end if
      end associate
    end function



    function tile_from_string(str) result(this)
      type(tile_t) :: this
      character(len=*), intent(in) :: str

      character(len=:), allocatable :: wrk
      if (verify(str,'nswe') /= 0) &
          error stop 'tile_from_string: invalid character in string'
      wrk = trim(str)
      this % x = 0

      do
        if (len(wrk)==0) exit
        if (len(wrk)==1) then
          select case (wrk(1:1))
          case('w')
            call this % move(WEST)
            wrk = wrk(2:)
          case('e')
            call this % move(EAST)
            wrk = wrk(2:)
          case default
            error stop 'tile_from_string: one char invalid'
          end select
        else
          select case (wrk(1:2))
          case('nw')
            call this % move(NWEST)
            wrk = wrk(3:)
          case('ne')
            call this % move(NEAST)
            wrk = wrk(3:)
          case('se')
            call this % move(SEAST)
            wrk = wrk(3:)
          case('sw')
            call this % move(SWEST)
            wrk = wrk(3:)
          case default
            select case (wrk(1:1))
            case('w')
              call this % move(WEST)
              wrk = wrk(2:)
            case('e')
              call this % move(EAST)
              wrk = wrk(2:)
            case default
              error stop 'tile_from_string: one char invalid'
            end select
          end select
        end if
      end do
    end function tile_from_string



    subroutine read_from_file(file, tiles)
      character(len=*), intent(in) :: file
      type(tile_t), intent(out), allocatable :: tiles(:)

      integer :: fid, ios, n, i
      character(len=2000) :: line

      open(newunit=fid, file=file, status='old')
      n = 0
      do
        read(fid,'(a)',iostat=ios) line
        if (ios /= 0) exit
        n = n + 1
      end do

      allocate(tiles(n))
      rewind(fid)
      do i=1,n
        read(fid,'(a)') line
        tiles(i) = tile_t(trim(line))
      end do
      close(fid)
    end subroutine read_from_file



    subroutine floor_print(this)
      class(floor_t), intent(in) :: this
      integer :: i, j

      do i=lbound(this%arr, 1), ubound(this%arr, 2)
        do j=lbound(this%arr, 2), ubound(this%arr, 2)
          if (.not. this%arr(i,j)%isvalid()) then
            write(*,'(a1)',advance='no') ' '
          elseif (this%arr(i,j)%color==WHITE_COLOR) then
            write(*,'(a1)',advance='no') '.'
          else
            write(*,'(a1)',advance='no') '#'
          end if
        end do
        write(*,*)
      end do
      write(*,*)
    end subroutine floor_print

  end module day24_mod
