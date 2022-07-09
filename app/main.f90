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

    !call day05('inp/05/input.txt')

    !call day06('inp/06/sample.txt')
    !call day06('inp/06/input.txt')

    !call day07('inp/07/sample.txt')
    !call day07('inp/07/input.txt')

    !call day08('inp/08/sample.txt')
    !call day08('inp/08/input.txt')

    !call day09('inp/09/input.txt', 25)
    !call day09('inp/09/sample.txt', 5)

    !call day11('inp/11/sample.txt',2)
    !call day11('inp/11/input.txt',1)
    !call day11('inp/11/input.txt',2)

    !call day12('inp/12/sample.txt')
    !call day12('inp/12/input.txt')

    !call day13('inp/13/test.txt')
    !call day13('inp/13/input.txt')

    !call day14('inp/14/sample.txt')
    !call day14('inp/14/sample2.txt')
    !call day14('inp/14/input.txt')

    !call day15()

    !call day17('inp/17/sample.txt',1)
    !call day17('inp/17/sample.txt',2)
    !call day17('inp/17/input.txt',1)
    !call day17('inp/17/input.txt',2)

    !call day18('inp/18/input.txt', 1)
    !call day18('inp/18/input.txt', 2)

    !call day19('inp/19/sample.txt')
    !call day19('inp/19/input.txt')
    !call day19('inp/19/sample2.txt')
    !call day19('inp/19/input2.txt')

    !call day20('inp/20/test.txt')
    !call day20('inp/20/input.txt')

    !call day22('inp/22/sample.txt')
    !call day22('inp/22/input.txt')

    call day23('389125467') ! test case
    call day23('318946572') ! live puzzle

    !call day24('inp/24/test.txt')
    !call day24('inp/24/input.txt')

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



  subroutine day05(file)
    use day05_mod
    implicit none
    character(len=*), intent(in) :: file
    type(boarding_t), allocatable :: bps(:)
    integer :: max_id, i, next_id, missing_id

    call read_from_file(file, bps)
    call sort_bps(bps)
    max_id = -1
    next_id = -1
    missing_id = -1
    do i=1, size(bps)
      if (bps(i) % id > max_id) max_id = bps(i) % id
      if (next_id /= bps(i)%id .and. next_id /= -1) then
        if (missing_id /= -1) error stop 'main: more than one missing...'
        missing_id = next_id
      end if
      next_id = bps(i)%id + 1
    end do
    print '("Answer to day 5/1 is ",i0,1x,l1)', max_id, max_id==820 .or. max_id==801
    print '("Answer to day 5/2 is ",i0,1x,l1)', missing_id, missing_id==597
  end subroutine day05



  subroutine day06(file)
    use day06_mod
    implicit none
    character(len=*), intent(in) :: file
    type(form_t), allocatable :: forms(:), forms2(:)
    integer :: i, ans1, ans2

    call read_answers(file, forms, 1)
    call read_answers(file, forms2, 2)
    ans1 = 0
    ans2 = 0
    do i=1, size(forms)
      ans1 = ans1 + count(forms(i)%ans)
      ans2 = ans2 + count(forms2(i)%ans)
print *, i, count(forms(i)%ans), count(forms2(i)%ans)
    end do
    print '("Answer day 06/1 is ",i0,1x,l1)', ans1, ans1==6532 .or. ans1==11
    print '("Answer day 06/2 is ",i0,1x,l1)', ans2, ans2==3427 .or. ans2==6

  end subroutine day06



  subroutine day07(file)
    use day07_mod
    implicit none
    character(len=*), intent(in) :: file

    type(rule_t), allocatable :: rules_list(:)
    type(color_t) :: mycolor
    type(color_t), allocatable :: color_list(:)
    integer :: ir, ic, nold, nfound, ans1, ans2
    logical, allocatable :: rule_used(:)

    call read_all_rules(file, rules_list)

    ! Part One
    ! - the list contains our bag color initially
    ! - copy a color from rules if the rule contains a color on the list
    ! - repeat until no additions to the list found
    allocate(rule_used(size(rules_list)))
    rule_used = .false.
    call mycolor % set('shiny gold bag')
    color_list = [mycolor]
    do
      nold = size(color_list)
      do ir=1,size(rules_list)
        if (rule_used(ir)) cycle
        nfound = 0
        do ic=1,size(color_list)
          nfound = nfound + rules_list(ir)%findcolor(color_list(ic))
        end do
        if (nfound == 0) cycle
        rule_used(ir) = .true.
        color_list = [color_list, rules_list(ir)%base_color]
      end do
      if (size(color_list)==nold) exit
    end do
    ans1 = size(color_list)-1 ! exclude our bag color from the list
    print *, 'Answer 7/1 is ', ans1, ans1==205 .or. ans1==4

    ! Part Two
    ir = index_color(rules_list, mycolor)
    call count_nbags(ir, rules_list)
    ans2 = rules_list(ir)%nbags
    print *, 'Answer 7/2 is ', ans2, ans2==32 .or. ans2==80902
  end subroutine day07



  subroutine day08(file)
    use day08_mod
    implicit none
    character(len=*), intent(in) :: file
    type(computer_t) :: zx
    integer :: iexit, ans1, ans2
    call zx % load_program(file)
    call zx % run_program(iexit, ans1)
    call zx % find_working_program(ans2)
    print '("Answer day 08/1 is ",i0,1x,l1)', ans1, ans1==5 .or. ans1==1446
    print '("Answer day 08/2 is ",i0,1x,l1)', ans2, ans2==8 .or. ans2==1403
  end subroutine day08



  subroutine day09(file, preamble)
    use day09_mod
    implicit none
    character(len=*), intent(in) :: file
    integer, intent(in) :: preamble

    integer(I8B), allocatable :: arr(:)
    integer(I8B) :: ans1, ans2, amin, amax
    integer :: i, j, n
    logical :: isvalid
    type(interval_t) :: interval
    real :: t0, t1

    call cpu_time(t0)
    call read_numbers(file, arr)
    print '("Numbers in file ",i0)', size(arr)
    print '("Buffer size ",i0)', preamble
    do i=preamble+1, size(arr)
      isvalid = is_num_valid(arr(i-preamble:i-1), arr(i))
      if (.not. isvalid) exit
    end do
    ans1 = arr(i)
    print '("Answer to 9/1 is ",i0,"   Valid? ",l1)', ans1, ans1==26796446 .or. ans1==127

    ! Part Two
    call interval % init(file, [1,1])
    n = size(interval % arr)
    OUT: do i=1, n-1
      call interval % reset([i, i])
      IN: do j=i+1, n
        call interval % move(U_MARK,1)
        if (interval % sum == ans1) exit OUT
        if (interval % sum > ans1) exit IN
      end do IN
    end do OUT

    if (interval%sum==ans1) then
      print '("Interval found {",i0," ",i0,"}   Sum ",i0)', interval%marks, interval%sum
      associate(set => interval%arr(interval%marks(L_MARK) : interval%marks(U_MARK)))
        amin = minval(set)
        amax = maxval(set)
      end associate
      print '("Min value ",i0," and max value ",i0)', amin, amax
    else
      error stop 'Search failed'
    end if
    ans2 = amin+amax
    print '("Answer to 9/2 is ",i0,"   Valid? ",l1)', ans2, ans2==62 .or. ans2==3353494
    call cpu_time(t1)
    print '("Time taken ",f8.3," seconds")', t1-t0
    print *
  end subroutine day09

! day10

  subroutine day11(file, mode)
    use day11_mod
    implicit none
    character(len=*), intent(in) :: file
    integer, intent(in) :: mode
    type(seatplan_t) :: sp
    integer :: nswaps, ans

    sp = seatplan_t(file, mode)
    do
      !call sp % print()
      call sp % onestep(nswaps)
      !print '("Swaps ",i0,"   Occupied seats ",i0)', nswaps, count(sp%plan==OCCUPIED_SEAT)
      if (nswaps==0) exit
    end do
    ans = count(sp%plan==OCCUPIED_SEAT)
    print '("Answer 11/",i0," is ",i0,1x,l1)', mode, ans, &
      (mode==1 .and. (ans==2494 .or. ans==37)) .or. (mode==2 .and. (ans==2306 .or. ans==26))
  end subroutine day11



  subroutine day12(file)
    use day12_mod
    implicit none
    character(len=*), intent(in) :: file
    character(len=:), allocatable :: instructions(:)
    type(ship_t), target :: ship
    integer :: mand

    call read_input_lines(file, instructions)
    print '("Number of instructions ",i0)', size(instructions)

    call ship % autopilot(instructions, 1)
    print '("Ship position = [",i0,", ",i0,"]   heading = ",i0)', &
      ship%ship, ship%heading
    mand = sum(abs(ship%ship))
    print '("Manhattan distance 1 = ",i0,1x,l1)', mand, mand==1603 .or. mand==25

    call ship % reset()
    call ship % autopilot(instructions, 2)
    print '("Ship position = [",i0,", ",i0,"]   waypoint = [",i0,", ",i0,"]")', &
      ship%ship, ship%wp
    mand = sum(abs(ship%ship))
    print '("Manhattan distance 2 = ",i0,1x,l1)', mand, mand==52866 .or. mand==286
    print *
  end subroutine day12



  subroutine day13(file)
    use day13_mod
    implicit none
    character(len=*), intent(in) :: file

    integer :: fid, i, idmin
    integer(I8) :: tnow, tmin
    integer(I8) :: time, mp
    character(len=10000) :: line
    integer(I8), allocatable :: arr(:)

    open(newunit=fid, file=file, status='old')
    read(fid,*) tnow
    read(fid,'(a)') line
    call disect_line(line, arr)
    close(fid)

    print '(i0)', tnow
    print '(*(i0,:,1x))', arr

    tmin = huge(tmin)
    do i=1, size(arr)
      if (arr(i) < 0) cycle
      time = tnext(tnow, arr(i))
      if (time < tmin) then
        tmin = time
        idmin = arr(i)
      end if
    end do
    print '("tnext= ",i0,"  by id ",i0,"  in ",i0)', tmin, idmin, tmin-tnow
    print *, 'Answer is ', idmin*(tmin-tnow), idmin*(tmin-tnow)==222

    time = 0
    mp = arr(1)
    print '(/,a4,1x,a4,1x,a18)', ' bus', 'wait', '              time'
    print '(i4,1x,i4,1x,i18)', arr(1), 1-1, time
    do i=2,size(arr)
      if (arr(i) < 0) cycle
      time = next_time(time, mp, arr(i), int(i-1, kind=I8))
      ! this works only because bus numbers are primes, otherwise
      ! a more complex way should be used to update "mp"
      mp = mp * arr(i)
      print '(i4,1x,i4,1x,i18)', arr(i), i-1, time
    end do
    print *
    print *, 'Answer is ', time, time==408270049879073_I8 .or. time==1068781_I8
  end subroutine day13



  subroutine day14(file)
    use day14_mod, only : instruction_t, state_t, read_all_instructions, I8
    implicit none
    character(len=*), intent(in) :: file
    type(instruction_t), allocatable :: list(:)
    type(state_t) :: zx
    integer :: i
    integer(I8) :: ans1, ans2

    call read_all_instructions(file, list)
    call zx % init()

    do i = 1, size(list)
      call zx % doinstruction(list(i),1)
    end do
    ans1 = zx % summemory()
    print *, 'Answer 14/1 is ', ans1, ans1==165 .or. ans1==14839536808842_I8

    call zx % clear()
    do i = 1, size(list)
      call zx % doinstruction(list(i),2)
    end do
    ans2 = zx % summemory()
    print *, 'Answer 14/2 is ', ans2, ans2==208 .or. ans2==4215284199669_I8
  end subroutine day14



    subroutine day15()
      use day15_mod
      implicit none
      type(game_t) :: game
      !integer, parameter :: START(*) = [3, 1, 2]
      integer, parameter :: START(*) = [0,5,4,1,10,14,7]
      integer :: i, number

      do i=1, MAX_NUM !2020
        if (i <= size(START)) then
          call game % say_init(START(i))
        else
          call game % say_next(number)
        end if
      end do
      print *, 'Answer is : ', game % last_said
    end subroutine day15

! day16

  subroutine day17(file, mode)
    use day17_mod
    implicit none
    character(len=*), intent(in) :: file
    integer, intent(in) :: mode
    type(conway_t) :: cw
    integer :: ans, i

    cw = conway_t(file)
    call cw % print()
    do i = 1, 6
      call cw % cycle(mode)
      !call cw % print()
    end do
    ans = count(cw % cube)
    print '("Active cubes ",i0,l1)', ans, &
      (mode==1 .and. (ans==112 .or. ans==348)) .or. &
      (mode==2 .and. (ans==848 .or. ans==2236))
    print *
  end subroutine day17



  subroutine day18(file, mode)
    use day18_mod
    implicit none
    character(len=*), intent(in) :: file
    integer, intent(in) :: mode
    character(len=LINE_LEN), allocatable :: lines(:)
    integer(I8), allocatable :: vals(:)
    integer(I8) :: ans
    integer :: i

    call read_input_lines(file, lines)
    allocate(vals(size(lines)))
    do i=1,size(lines)
      vals(i) = evaluate_string(lines(i), mode)
      print *, trim(lines(i))
      print '("Value = ",i0)', vals(i)
    end do
    ans = sum(vals)
    print '("Answer 18/",i0," is ",i0,1x,l1)', mode, ans, &
        (mode==1 .and. ans==31142189909908_I8) .or. &
        (mode==2 .and. ans==323912478287549_I8)
    print *
  end subroutine day18



  subroutine day19(file)
    use day19_mod
    implicit none
    character(len=*), intent(in) :: file

    integer, parameter :: REP = 500
    type(rule_t), allocatable :: rules(:)
    character(len=CODE_LEN), allocatable :: codes(:)
    integer :: i, cnt, j
    logical :: isok

    call read_input(file, rules, codes)
    cnt = 0
    do i=1, size(codes)
      do j=1,REP
        isok = is_valid_code(codes(i), rules)
        if (isok) exit
      end do
      if (isok) cnt = cnt + 1
      !print *, '{'//trim(codes(i))//'}', isok
    end do
    print '("Answer day 19: ___",i0,"___  Ok part 1? ",l1"  Ok part 2? ",l1)', &
        cnt, cnt==2 .or. cnt==111, cnt==343 .or. cnt==12
  end subroutine day19



  subroutine day20(file)
    use day20_mod
    use kinds_m, only : I8 => I8B
    implicit none
    character(len=*), intent(in) :: file
    type(puzzle_t) :: puzzle
    integer :: corner_ids(4), i, ans2
    integer(I8) :: ans1


    call puzzle % read(file)
    call puzzle % corner_tiles(corner_ids)
    print '(a,*(i0,:,1x))', 'Corners = ', corner_ids
    ans1 = 1_I8
    do i=1,4
      ans1 = ans1 * int(corner_ids(i), kind=I8)
    end do
    print *, 'Answer 20/1 is ', ans1, ans1==20899048083289_I8 .or. ans1==84116744709593_I8
    print *

    ! Part Two
    call puzzle % solve(ans2)
    call puzzle % print()
    print *, 'Answer 20/2 is ', ans2, ans2==273 .or. ans2==1957
    print *
    print *
  end subroutine day20

! day21

  subroutine day22(file)
    use day22_mod
    implicit none
    character(len=*), intent(in) :: file
    type(game_t) :: g
    integer :: game_winner, score
    real :: t0, t1

    ! Part One
    g = game_t(file)
    call cpu_time(t0)
    call g % play_game(game_winner, .false.)
    score = g%score(game_winner)
    call cpu_time(t1)
    print '("Winner is ",i0," with score ",i0)', game_winner, score
    print '(l1,a,f8.3)', score==32472 .or. score==306, "    Time taken ",t1-t0

    ! Part Two - Recursive combat
    g = game_t(file)
    call cpu_time(t0)
    call g % play_game(game_winner, .true.)
    score = g%score(game_winner)
    call cpu_time(t1)
    print '("Winner is ",i0," with score ",i0)', game_winner, score
    print '(l1,a,f8.3)', score==36463 .or. score==291,"    Time taken ",t1-t0
  end subroutine day22



  subroutine day23(input)
    use day23_mod
    implicit none
    character(len=*), intent(in) :: input
    type(circle_t) :: cups
    integer :: i

    cups = circle_t(input)
    do i=1,100
      call cups % move()
    end do
    print *, 'Final state'
    call cups % print()

  end subroutine day23



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
