  program main
    implicit none

    !call day01('inp/01/input.txt')
    !call day01('inp/01/sample.txt')

    !call day02('inp/02/input.txt')
    !call day02('inp/02/sample.txt')

    !call day03('inp/03/input.txt')
    !call day03('inp/03/sample.txt')

    !call day04('inp/04/sample.txt')
    !call day04('inp/04/sample3.txt')
    !call day04('inp/04/input.txt')




    !call day24('inp/24/test.txt')
    call day24('inp/24/input.txt')

    !call day25('inp/25/test.txt')
    !call day25('inp/25/input.txt')
  end program main



  subroutine day01(file)
    use day01_mod
    implicit none
    character(len=*), intent(in) :: file

    integer, parameter :: SUM_TO_FIND = 2020
    integer, allocatable :: a(:)
    integer :: enta(2), entb(3), ans
    real :: t0, t1

    call cpu_time(t0)
    a = read_from_file(file)
    call quicksort(a)
    enta = find_entries(a, SUM_TO_FIND)
    ans = enta(1)*enta(2)
    print '(i0,1x,i0)', enta
    print '("Answer day01 part 1 = ",i0,1x,l1)', ans, ans==514579 .or. ans==744475

    entb = find_three_entries(a, SUM_TO_FIND)
    ans = entb(1)*entb(2)*entb(3)
    print '(i0,1x,i0,1x,i0)', entb
    print '("Answer day01 part 2 = ",i0,1x,l1)', ans, ans==241861950 .or. ans==70276940
    call cpu_time(t1)
    print '("Time spent = ",f12.4," seconds")', t1-t0
  end subroutine day01



  subroutine day02(file)
    use day02_mod
    implicit none
    character(len=*), intent(in) :: file
    type(feed_t) :: feed
    type(password_t) :: pas
    character(len=:), allocatable :: line
    integer :: valid, valid_b

    valid = 0
    valid_b = 0
    call feed % open(file)
    do
      if (feed % iseof()) exit
      call feed % get(line)
      call pas % set(line)
      if (pas % isvalid()) valid = valid + 1
      if (pas % isvalid_b()) valid_b = valid_b + 1
    end do

    print '("Answer day02 part 1 = ",i0,1x,l1)', valid, valid==2 .or. valid==600
    print '("Answer day02 part 2 = ",i0,1x,l1)', valid_b, valid_b==1 .or. valid_b==245
  end subroutine day02



  subroutine day03(file)
    use day03_mod
    use parse_mod, only : read_pattern
    implicit none
    character(len=*), intent(in) :: file
    
    
    integer, parameter :: I8 = selected_int_kind(18)
    character(len=1), allocatable :: aa(:,:)
    integer :: i, j, cnt(5)
    integer(I8) :: mlt
    integer, parameter :: SLOPES(*,*) = reshape([1,1, 1,3, 1,5, 1,7, 2,1], [2,5])

    aa = read_pattern(file)
    do i=1,size(aa,dim=1)
    do j=1,size(aa,dim=2)
      write(*,'(a)',advance='no') aa(i,j)
    end do
    write(*,*)
    end do

    mlt = 1_I8
    do i = 1, size(SLOPES, dim=2)
      cnt(i) = count_trees(aa,SLOPES(:,i))
      print '("Slope = [",i0,",",i0,"]  Trees = ",i0)', SLOPES(:,i), cnt(i)
      mlt = mlt * int(cnt(i), kind=I8)
    end do

    print '("Answer day02 part 1 = ",i0,1x,l1)', cnt(2), cnt(2)==7 .or. cnt(2)==173
    print '("Answer day02 part 2 = ",i0,1x,l1)', mlt, mlt==336 .or. mlt==4385176320_I8
  end subroutine day03



  subroutine day04(file)
    use day04_mod
    implicit none
    character(len=*), intent(in) :: file

    type(passport_t) :: pas
    integer :: fid, nvalid, nvalid2
    character(len=500) :: ln
    character(len=:), allocatable :: err

    open(newunit=fid, file=file, status='old')
    nvalid = 0
    nvalid2 = 0
    do
      call read_to_line(fid,ln)
      print '(a)', trim(ln)
      if (ln==' ') exit
      call pas % read_from_line(ln)
 print *, pas%is_passport(), pas%is_valid(err)
 if (err /= 'ok') print *, err
      if (pas % is_passport()) nvalid = nvalid + 1
      if (pas % is_valid()) nvalid2 = nvalid2 + 1
 print *
    end do
    print '("Answer day04 part 1 = ",i0,1x,l1)', nvalid, nvalid==2 .or. nvalid==219
    print '("Answer day04 part 2 = ",i0,1x,l1)', nvalid2, nvalid2==127
  end subroutine day04



  subroutine day24(file)
    use day24_mod
    implicit none
    character(len=*), intent(in) :: file

    type(tile_t), allocatable :: tiles(:)
    type(floor_t) :: flr
    integer :: i, ans

    call read_from_file(file, tiles)
    !call tile_sort(tiles)
    !call tile_remove_dupl(tiles)
    !ans = size(tiles)
    !print *, 'Answer to part 1 is :', ans, ans==360 .or. ans==10

    call flr % init(tiles)
    ans = flr % blacks()
    print *, 'Answer to part 1 is :', ans, ans==360 .or. ans==10
    call flr%print()
    do i=1,100
      call flr % expand()
    end do
    call flr%print()
    ans = flr%blacks()
    print *, 'Answer to part 2 is :', ans, ans==2208 .or. ans==3924
  end subroutine day24



  subroutine day25(file)
    use day25_mod, only : START_VAL, I8, brute_force_ls, handshake
    implicit none
    character(len=*), intent(in) :: file

    integer(I8) :: card_key, door_key, fid
    integer(I8) :: card_ls, door_ls, enc_key

    open(newunit=fid, file=file, status='old')
    read(fid,*) card_key
    read(fid,*) door_key
    print '("keys = ",2(i8,1x))', card_key, door_key
    close(fid)

    card_ls = brute_force_ls(START_VAL, card_key)
    door_ls = brute_force_ls(START_VAL, door_key)
    print '("ls   = ",2(i8,1x))', card_ls, door_ls
    enc_key = handshake(card_ls, door_ls, card_key, door_key)
    if (enc_key < 0) error stop 'hanshake failed'
    print '("key  = ",i8,1x,l1)', enc_key, enc_key==5025281 .or. enc_key==14897079
  end subroutine day25

