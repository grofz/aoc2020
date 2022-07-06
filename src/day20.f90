  module day20_mod
    implicit none

    integer, parameter :: TILE_SIZE = 10, MONSTER_X = 20, MONSTER_Y = 3
    integer, parameter :: HSH=1, REP=2, NGI=3, NGO=4

    type tile_t
      integer :: id, num
      character(len=1) :: pattern(TILE_SIZE, TILE_SIZE)
      integer :: b(4,8) = -1 ! HSH, REP, NGI, NGO
      integer :: tx = -1, ty = -1, ori = 1
    contains
      procedure :: print => tile_print
      procedure :: hashify => tile_hashify
      procedure :: border_sides => tile_border_sides
      procedure :: rotate_r => tile_rotate
      procedure :: mirror_y => tile_mirror
    end type tile_t

    type puzzle_t
      type(tile_t), allocatable :: tiles(:)
      character(len=1), allocatable :: bpic(:,:)
      character(len=1) :: monster(MONSTER_X, MONSTER_Y)="?"
      integer :: na=-1
    contains
      procedure :: read => puzzle_read
      procedure :: corner_tiles => puzzle_corner_tiles
      procedure :: solve => puzzle_solve
      procedure :: print => puzzle_print
    end type puzzle_t

  contains

    subroutine puzzle_read(this, file)
      class(puzzle_t), intent(out) :: this
      character(len=*), intent(in) :: file

      integer :: i, j, n, ij
      integer, allocatable :: arr(:), freq(:)

      call read_all_tiles(file, this%tiles)
      n = size(this%tiles)

      ! Array with all hash values
      allocate(arr(8*n))
      ij = 0
      do i=1,n
        call this % tiles(i) % hashify()
        do j=1,8
          ij = ij + 1
          arr(ij) = this % tiles(i) % b(HSH,j)
        end do
      end do

      ! Frequency of hash values
      call calc_freq(arr, freq)
      do i=1,n
        do j=1,8
          associate(hash=>this%tiles(i) % b(HSH,j), rep=>this%tiles(i) % b(REP,j))
            if (hash < lbound(freq,1) .or. hash > ubound(freq,1)) &
                error stop 'puzzle_read - frequency calculation went wrong'
            rep = freq(hash)
          end associate
        end do
      end do

      ! Each external side gives two single valued hashes
      ! Each internal side gives two pairs of hashes
      if (count(freq==1)+2*count(freq==2) /= 8*n ) then
        print *, 'WARNING - non-unique tile placement detected'
        print *, 'algorithm is not prepared for this situation'
        print *, count(freq==0), count(freq==1), count(freq==2)
        print *, count(freq<0 .or. freq>2)
        error stop
      else
        ! seems a simple case
        associate(c => count(freq==1))
          print '("Tiles per side ",i0,"  Is valid? ",l1)', c/8, mod(c,8)==0
          this % na = c/8
          call find_ngb(this%tiles, c/8)
        end associate
      end if
    end subroutine puzzle_read



    subroutine read_all_tiles(file, tiles)
      character(len=*), intent(in) :: file
      type(tile_t), allocatable, intent(out) :: tiles(:)
!
! Read input tiles from the file into array of tiles
!
      integer :: fid, ios, i, n, j, i0
      character(len=1000) line
      character(len=:), allocatable :: tile_id

      ! Count number of tiles and verify the format
      open(newunit=fid, file=file, status='old')
      n = 0
      do
        read(fid, '(a)', iostat=ios) line
        if (ios /= 0) exit
        if (line(1:5)=="Tile ") then
          n = n + 1
        else
          error stop 'read_all_tiles - Tile xxxx: expected'
        end if
        j = 0
        do
          read(fid,'(a)',iostat=ios) line
          if (ios /= 0) exit
          if (len_trim(line)==0) exit
          if (len_trim(line) /= TILE_SIZE) error stop 'read_all_tiles - Line length not TILE_SIZE'
          j = j + 1
        end do
        if (j /= TILE_SIZE) error stop 'read_all_tiles - Number of lines not TILE_SIZE'
      end do
      print '("Number of tiles ",i0)', n

      ! Read all the tiles
      allocate(tiles(n))
      rewind(fid)
      do i=1,n
        read(fid, '(a)') line
        i0 = scan(line,':')
        tile_id = line(6:i0-1)
        read(tile_id,*) tiles(i) % id
        tiles(i) % num = i
        do j=1, TILE_SIZE
          read(fid,'(*(a1))') tiles(i)%pattern(:,j)
        end do
        if (i /= n) read(fid,*) ! skip empty line
      end do
      close(fid)
    end subroutine read_all_tiles



    subroutine tile_print(this)
      class(tile_t), intent(in) :: this

      integer :: i
      print '("Id = ",i0)', this % id
      print 100, this%b(HSH,:)
      print 100, this%b(NGI,:)
      print 100, this%b(NGO,:)
      print '("Orientation ",i0,"   Position ",i0," x ",i0)',this%ori, this%tx, this%ty
      do i=1, size(this%pattern,dim=2)
        print '(*(a1))', this%pattern(:,i)
      end do
      print *
      100 format(8(i5,1x))
    end subroutine tile_print



    subroutine puzzle_print(this)
      class(puzzle_t), intent(in) :: this

      integer :: i
      if (.not. allocated(this%bpic)) error stop 'puzzle_print - image not ready'
      do i=1, size(this%bpic,dim=2)
        print '(*(a1))', this%bpic(:,i)
      end do
      print *
    end subroutine puzzle_print



    subroutine tile_hashify(this)
      class(tile_t), intent(inout) :: this
      associate (aa => this%pattern, n => TILE_SIZE)
        this % b(HSH,1) = calc_hash(aa(1,:))
        this % b(HSH,2) = calc_hash(aa(:,n))
        this % b(HSH,3) = calc_hash(aa(n,:))
        this % b(HSH,4) = calc_hash(aa(:,1))
        this % b(HSH,5) = calc_hash(aa(1,n:1:-1))
        this % b(HSH,6) = calc_hash(aa(n:1:-1,n))
        this % b(HSH,7) = calc_hash(aa(n,n:1:-1))
        this % b(HSH,8) = calc_hash(aa(n:1:-1,1))
      end associate
    end subroutine tile_hashify

    pure function calc_hash(string) result(hash)
      integer :: hash
      character(len=1), intent(in) :: string(:)
      integer :: i, bit

      if (size(string) /= TILE_SIZE) error stop 'calc_hash - wrong arr length'
      hash = 0
      do i=1, TILE_SIZE
        select case(string(i))
        case('.')
          bit = 0
        case('#')
          bit = 1
        case default
          error stop 'calc_hash - uknonwn pattern character'
        end select
        hash = hash + bit * 2**(TILE_SIZE-i)
      end do
    end function calc_hash



    subroutine tile_rotate(this)
      class(tile_t), intent(inout) :: this
!
! Rotaate tile clock-wise (right)
!
      integer :: y0, x0, y, x
      character(len=1) :: pattern0(TILE_SIZE, TILE_SIZE)
      integer :: b0(4,8)

      ! y' = x, x' = -y
      pattern0 = this % pattern
      do y=1, TILE_SIZE
        do x=1, TILE_SIZE
          x0 = y
          y0 = TILE_SIZE-x+1
          this % pattern(y,x) = pattern0(y0,x0)
        end do
      end do

      b0 = this % b
      this % b(:,1) = b0(:,8)
      this % b(:,2) = b0(:,1)
      this % b(:,3) = b0(:,6)
      this % b(:,4) = b0(:,3)
      this % b(:,5) = b0(:,4)
      this % b(:,6) = b0(:,5)
      this % b(:,7) = b0(:,2)
      this % b(:,8) = b0(:,7)

      this % ori = mod(this % ori, 8) + 1
    end subroutine tile_rotate



    subroutine tile_mirror(this)
      class(tile_t), intent(inout) :: this
!
! Mirror tile according to y-axis
!
      integer :: y0, x0, y, x
      character(len=1) :: pattern0(TILE_SIZE, TILE_SIZE)
      integer :: b0(4,8)

      ! y' = y, x' = -x
      pattern0 = this % pattern
      do y=1, TILE_SIZE
      do x=1, TILE_SIZE
        x0 = TILE_SIZE-x+1
        y0 = y
        this % pattern(y,x) = pattern0(y0,x0)
      end do
      end do
      b0 = this % b
      this % b(:,1) = b0(:,5)
      this % b(:,2) = b0(:,4)
      this % b(:,3) = b0(:,7)
      this % b(:,4) = b0(:,2)
      this % b(:,5) = b0(:,1)
      this % b(:,6) = b0(:,8)
      this % b(:,7) = b0(:,3)
      this % b(:,8) = b0(:,6)
    end subroutine tile_mirror



    subroutine rotate_r(aa)
      character(len=1), intent(inout) :: aa(:,:)
      character(len=1) :: aa0(size(aa,1),size(aa,2))
      integer :: y0, x0, y, x, nmax
      nmax = size(aa,1)
      if (nmax /= size(aa,2)) error stop 'rotate_r - array must be square'
      aa0 = aa
      do y=1, nmax
      do x=1, nmax
        ! y' = x, x' = -y
        x0 = y
        y0 = nmax-x+1
        aa(y,x) = aa0(y0,x0)
      end do
      end do
    end subroutine rotate_r

    subroutine mirror_y(aa)
      character(len=1), intent(inout) :: aa(:,:)
      character(len=1) :: aa0(size(aa,1),size(aa,2))
      integer :: y0, x0, y, x, xmax, ymax
      ymax = size(aa,1)
      xmax = size(aa,2)
      aa0 = aa
      do y=1, ymax
      do x=1, xmax
        ! y' = y, x' = -x
        x0 = xmax-x+1
        y0 = y
        aa(y,x) = aa0(y0,x0)
      end do
      end do
    end subroutine mirror_y


    subroutine sort_array(arr, key_col)
      integer, intent(inout) :: arr(:,:)
      integer, intent(in) :: key_col

      integer :: row_wrk(size(arr,dim=2))
      integer :: nrow, i, j

      nrow = size(arr,dim=1)
      do i=2, nrow
        row_wrk = arr(i,:)
        j = i-1
        do
          if (j==0) exit
          if (arr(j,key_col) > row_wrk(key_col)) then
            arr(j+1,:) = arr(j,:)
          else
            exit
          end if
          j = j-1
        end do
        arr(j+1,:) = row_wrk
      end do
    end subroutine sort_array



    subroutine calc_freq(arr, freq)
      integer, intent(in) :: arr(:)
      integer, allocatable, intent(out) :: freq(:)

      integer :: i0, i1, j
      i0 = minval(arr,dim=1)
      i1 = maxval(arr,dim=1)
      allocate(freq(i0:i1))
      freq = 0
      do j=1,size(arr)
        associate(x => freq(arr(j)))
          x = x + 1
        end associate
      end do
    end subroutine calc_freq



    function tile_border_sides(this) result(res)
      class(tile_t), intent(in) :: this
      integer :: res

      select case(count(this%b(REP,:)==1))
      case(4)
        res = 2 ! tile is a corner tile
      case(2)
        res = 1 ! tile is at the border
      case(0)
        res = 0 ! tile is not at the border
      case default
        print *, 'WARNING - tile_border_sides undetected', this % id
        error stop 'tile_border_sides - detection error'
      end select
    end function tile_border_sides



    subroutine puzzle_corner_tiles(this, c_tiles, c_tiles_num)
      class(puzzle_t), intent(in) :: this
      integer, intent(out) :: c_tiles(4)
      integer, intent(out), optional :: c_tiles_num(4)

      integer :: i, ifound
      ifound = 0
      c_tiles = -1
      do i=1, size(this%tiles)
        if (this%tiles(i)%border_sides()==2) then
          ifound = ifound + 1
          c_tiles(ifound) = this%tiles(i)%id
          if (present(c_tiles_num)) c_tiles_num(ifound) = this%tiles(i)%num
        end if
      end do
    end subroutine puzzle_corner_tiles



    subroutine find_ngb(tiles, na)
      class(tile_t), intent(inout) :: tiles(:)
      integer, intent(in) :: na ! puzzle is "na x na" tiles

      integer, allocatable :: arr(:,:)
      integer :: ij, i, j

      if (na * na /= size(tiles)) error stop 'find_ngb - algorithm fails'
      allocate(arr(4*2*na*(na-1), 3))

      ! Array of all hash values with frequency of 2
      ij = 0
      do i=1, size(tiles)
        do j=1, 8
          if (tiles(i) % b(REP,j) /= 2) cycle
          ij = ij+1
          if (ij > size(arr,dim=1)) error stop 'find_ngb - arr too short'
          arr(ij,1) = tiles(i) % b(HSH,j) ! 1 - hash
          arr(ij,2) = i                   ! 2 - tile number
          arr(ij,3) = j                   ! 3 - tile side id
        end do
      end do
      call sort_array(arr, 1)

      do i=1, size(arr,dim=1)-1, 2
        if (arr(i,1) /= arr(i+1,1)) error stop 'find_ngb - arr not paired'
        associate(t1 => tiles(arr(i,2)), t2 => tiles(arr(i+1,2)))
          t1 % b(NGI,arr(i,3)) = arr(i+1,2)
          t1 % b(NGO,arr(i,3)) = arr(i+1,3)
          t2 % b(NGI,arr(i+1,3)) = arr(i,2)
          t2 % b(NGO,arr(i+1,3)) = arr(i,3)
        end associate
      end do
    end subroutine find_ngb



    subroutine puzzle_solve(this, ans)
      class(puzzle_t), intent(inout) :: this
      integer, intent(out) :: ans
!
! Solves part two of the puzzle.
!
      integer :: num0, num1, num, i, cnt

      ! Connect the tiles
      num0 = -1
      do
        if (num0==-1) then
          call puzzle_place_ltcorner(this, num0)
        else
          call puzzle_place_down(this, num0, num)
          if (num == -1) exit
          num0 = num
        end if
        num1 = num0
        do
          call puzzle_place_right(this, num1, num)
          if (num==-1) exit
          num1 = num
        end do
      end do

      ! Verify that all tiles were placed
      cnt = 0
      do i = 1, size(this%tiles)
        if (this%tiles(i)%tx==-1 .or. this%tiles(i)%ty==-1) cnt=cnt+1
      end do
      print '("Puzzle solved. All tiles placed? ",l1,1x,i1)',cnt==0,cnt

      ! Generate compound image (crop borders)
      !call puzzle_bpic(this%tiles, this%na, 0, this%bpic)
      call puzzle_bpic(this%tiles, this%na, 1, this%bpic)

      ! Search for monsters. If nothing found then reorient and try again.
      ! Do not test if there are multiple solutions
      call puzzle_read_monster(this, 'inp/20/monster.txt')
      do i=1,8
        call monster_search(this, cnt)
        if (cnt /= 0) exit
        call rotate_r(this%bpic)
        if (i==4) call mirror_y(this%bpic)
      end do
      if (i==9) error stop 'puzzle_solve - zero monsters found'
      ans = count(this%bpic=='#')
    end subroutine



    subroutine puzzle_place_ltcorner(this, numlt)
      class(puzzle_t), intent(inout) :: this
      integer, intent(out) :: numlt

      integer :: ct(4), ct_num(4), ior

      call this % corner_tiles(ct, ct_num)
      if (ct(1)==-1) error stop "place_ltcorner - corner tile not found"
      numlt = ct_num(1)

      ! change tile orientation until left and top sides are external
      associate(t => this%tiles(numlt))
        do ior = 1,8
          if (t % b(REP,1)==1 .and. t % b(REP,4)==1) exit
          call t % rotate_r()
          if (ior==4) call t % mirror_y()
        end do
        if (ior==9) error stop 'place_ltcorner - could not orient tile'
        t % tx = 1
        t % ty = 1
      end associate
    end subroutine puzzle_place_ltcorner


    subroutine puzzle_place_right(this, num, numr)
      class(puzzle_t), intent(inout) :: this
      integer, intent(in) :: num
      integer, intent(out) :: numr

      integer :: hash, ior
      numr = this%tiles(num) % b(NGI,2)
      hash = this%tiles(num) % b(HSH,2)
      if (numr == -1) return ! no more tiles to the right

      ! change tile orientation until its left side hash match "hash"
      associate(t => this%tiles(numr))
        do ior = 1,8
          if (t % b(HSH,4)==hash) exit
          call t % rotate_r()
          if (ior==4) call t % mirror_y()
        end do
        if (ior==9) error stop 'place_right - could not orient tile'
        t % tx = this%tiles(num) % tx + 1
        t % ty = this%tiles(num) % ty
      end associate
    end subroutine puzzle_place_right


    subroutine puzzle_place_down(this, num, numd)
      class(puzzle_t), intent(inout) :: this
      integer, intent(in) :: num
      integer, intent(out) :: numd

      integer :: hash, ior
      numd = this%tiles(num) % b(NGI,3)
      hash = this%tiles(num) % b(HSH,3)
      if (numd == -1) return ! no more tiles to down

      associate(t => this%tiles(numd))
        do ior = 1,8
          if (t % b(HSH,1)==hash) exit
          call t % rotate_r()
          if (ior==4) call t % mirror_y()
        end do
        if (ior==9) error stop 'place_down - could not orient tile'
        t % tx = this%tiles(num) % tx
        t % ty = this%tiles(num) % ty + 1
      end associate
    end subroutine puzzle_place_down


    subroutine puzzle_bpic(tiles, na, nb, bpic)
      type(tile_t), intent(in) :: tiles(:)
      integer, intent(in) :: na, nb
      character(len=1), intent(out), allocatable :: bpic(:,:)
!
! Full picture from sorted tiles.
! "na" is number of tiles at each side
! "nb" marks how many pixels crop from tiles (0 or 1)
!
      integer :: i, x0, x1, y0, y1, n1, nn

      n1 = TILE_SIZE - 2*nb
      nn = na * n1
      allocate(bpic(nn,nn))
      bpic = '?'
      do i=1, size(tiles)
        associate(loc => tiles(i)%pattern)
          x0 = (tiles(i)%tx-1)*n1+1
          y0 = (tiles(i)%ty-1)*n1+1
          x1 = x0+n1-1
          y1 = y0+n1-1
          bpic(y0:y1,x0:x1) = loc(1+nb:TILE_SIZE-nb,1+nb:TILE_SIZE-nb)
        end associate
      end do
    end subroutine puzzle_bpic



    subroutine puzzle_read_monster(this, file)
      class(puzzle_t), intent(inout) :: this
      character(len=*), intent(in) :: file

      integer :: fid, i

      open(newunit=fid, file=file, status='old')
      do i=1, MONSTER_Y
        read(fid,'(*(a1))') this%monster(:,i)
      end do
      close(fid)
    end subroutine puzzle_read_monster



    subroutine monster_match(aa,mm,is_match)
      character(len=1), intent(inout) :: aa(:,:)
      character(len=1), intent(in)    :: mm(:,:)
      logical :: is_match

      logical :: mask(size(aa,1),size(aa,2))

      if (size(aa,1) /= size(mm,1) .or. size(aa,2) /= size(mm,2)) &
          error stop 'monster_match - arrays does not match'
      mask = .false.
      where (mm=='#')
        mask = aa /= '#'
      end where

      if (any(mask)) then
        is_match = .false.
      else
        is_match = .true.
        where (mm=="#" .and. aa=="#") aa = 'O'
      end if
    end subroutine monster_match


    subroutine monster_search(this, nfound)
      class(puzzle_t), intent(inout) :: this
      integer, intent(out) :: nfound

      integer :: ax, ay, mx, my, i, j
      logical :: is_match

      nfound = 0
      associate(pic=>this%bpic, mon=>this%monster)
        ax = size(pic,1)
        ay = size(pic,2)
        mx = size(mon,1)
        my = size(mon,2)
        do i=1, ax-mx+1
        do j=1, ay-my+1
          call monster_match(pic(i:i+mx-1, j:j+my-1), mon, is_match)
          if (is_match) nfound = nfound + 1
        end do
        end do
        print '("Monsters found ",i0)', nfound
      end associate
    end subroutine monster_search

  end module day20_mod
