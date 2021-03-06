  program main
    implicit none
    real :: t0, t1
    call cpu_time(t0)
!goto 15

01  call day01('inp/01/input.txt')
    !call day01('inp/01/sample.txt')

02  call day02('inp/02/input.txt')
    !call day02('inp/02/sample.txt')

03  call day03('inp/03/input.txt')
    !call day03('inp/03/sample.txt')

04  call day04('inp/04/input.txt')
    !call day04('inp/04/sample.txt')
    !call day04('inp/04/sample3.txt')

05  call day05('inp/05/input.txt')

06  call day06('inp/06/input.txt')
    !call day06('inp/06/sample.txt')

07  call day07('inp/07/input.txt')
    !call day07('inp/07/sample.txt')

08  call day08('inp/08/input.txt')
    !call day08('inp/08/sample.txt')

09  call day09('inp/09/input.txt', 25)
    !call day09('inp/09/sample.txt', 5)

10  call day10('inp/10/input.txt')
    !call day10('inp/10/sample.txt')

11  call day11('inp/11/input.txt',1)
    call day11('inp/11/input.txt',2)
    !call day11('inp/11/sample.txt',2)

12  call day12('inp/12/input.txt')
    !call day12('inp/12/sample.txt')

13  call day13('inp/13/input.txt')
    !call day13('inp/13/test.txt')

14  call day14('inp/14/input.txt')
    !call day14('inp/14/sample.txt')
    !call day14('inp/14/sample2.txt')

15  call day15()
!stop

16  call day16('inp/16/input.txt')
    !call day16('inp/16/sample2.txt')

17  call day17('inp/17/input.txt',1)
    call day17('inp/17/input.txt',2)
    !call day17('inp/17/sample.txt',1)
    !call day17('inp/17/sample.txt',2)

18  call day18('inp/18/input.txt', 1)
    call day18('inp/18/input.txt', 2)

19  call day19('inp/19/input.txt')
    call day19('inp/19/input2.txt')
    !call day19('inp/19/sample.txt')
    !call day19('inp/19/sample2.txt')

20  call day20('inp/20/input.txt')
    !call day20('inp/20/test.txt')

21  call day21('inp/21/input.txt')
    !call day21('inp/21/sample.txt')

22  call day22('inp/22/input.txt')
    !call day22('inp/22/sample.txt')

23  call day23('318946572') ! live puzzle
    !call day23('389125467') ! test case

24  call day24('inp/24/input.txt')
    !call day24('inp/24/test.txt')

25  call day25('inp/25/input.txt')
    !call day25('inp/25/test.txt')

    call cpu_time(t1)
    print *
    print '("ADVENT OF CODE 2020 COMPLETED!")'
    print '("Total time taken ",f8.3," seconds")', t1-t0
  end program main



  subroutine day01(file)
    use day01_mod
    use parse_mod, only : read_from_file => read_numbers
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
    print '("Answer 1/1 = ",i0,1x,l1)', ans, ans==514579 .or. ans==744475

    entb = find_three_entries(a, SUM_TO_FIND)
    ans = entb(1)*entb(2)*entb(3)
    print '(i0,1x,i0,1x,i0)', entb
    print '("Answer 1/2 = ",i0,1x,l1)', ans, ans==241861950 .or. ans==70276940
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

    print '("Answer 2/1 = ",i0,1x,l1)', valid, valid==2 .or. valid==600
    print '("Answer 2/2 = ",i0,1x,l1)', valid_b, valid_b==1 .or. valid_b==245
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

    print '("Answer 3/1 = ",i0,1x,l1)', cnt(2), cnt(2)==7 .or. cnt(2)==173
    print '("Answer 3/2 = ",i0,1x,l1)', mlt, mlt==336 .or. mlt==4385176320_I8
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
    print '("Answer 4/1 = ",i0,1x,l1)', nvalid, nvalid==2 .or. nvalid==219
    print '("Answer 4/2 = ",i0,1x,l1)', nvalid2, nvalid2==127
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
    print '("Answer 5/1 is ",i0,1x,l1)', max_id, max_id==820 .or. max_id==801
    print '("Answer 5/2 is ",i0,1x,l1)', missing_id, missing_id==597
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
    print '("Answer 6/1 is ",i0,1x,l1)', ans1, ans1==6532 .or. ans1==11
    print '("Answer 6/2 is ",i0,1x,l1)', ans2, ans2==3427 .or. ans2==6
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
    print '("Answer 8/1 is ",i0,1x,l1)', ans1, ans1==5 .or. ans1==1446
    print '("Answer 8/2 is ",i0,1x,l1)', ans2, ans2==8 .or. ans2==1403
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



  subroutine day10(file)
    use day10_mod
    use parse_mod, only : read_numbers
    use quicksort_module, only : quicksort
    use kinds_m, only : I8B
    implicit none
    character(len=*), intent(in) :: file
!
! DAY!) - ADAPTER ARRAY
!
    integer, allocatable :: aa(:), bb(:)
    logical, allocatable :: essential(:)
    integer, parameter :: NDIF = 2, DIF(NDIF) = [1, 3]
    integer :: cnt_dif(NDIF), ans1, ipos, nvalid
    integer(I8B) :: ans2

    aa = read_numbers(file)
    aa = [aa, 0, maxval(aa)+3]
    call quicksort(aa)
    cnt_dif = count_dif(aa, DIF)
    ans1 = cnt_dif(1)*cnt_dif(2)

    print '(10(i3,1x))', aa
    print '(10(i3,1x))', cnt_dif
    print '("Answer 10/1 is ",i0,1x,l1)', ans1, ans1==220 .or. ans1==1920

    ! Part Two
    essential = mark_essential(aa, DIF(2))
    ipos = 1
    ans2 = 1_I8B
    do
      call extract_chain(aa, essential, ipos, bb)
      if (size(bb)==0) exit
      nvalid = count_valid_combinations(bb, DIF(2))
      ans2 = ans2 * nvalid
      print '("Valid ",i2," out of ",i2," in chain of ",*(i3,1x))', &
          nvalid, 2**(size(bb)-2), bb
    end do
    print '("Answer 10/2 is ",i0,1x,l1)', ans2, ans2==19208 .or. ans2==1511207993344_I8B
  end subroutine day10



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
    print '("Day 12")'
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
    print *, 'Answer 13/1 is ', idmin*(tmin-tnow), idmin*(tmin-tnow)==222

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
    print *, 'Answer 13/2 is ', time, time==408270049879073_I8 .or. time==1068781_I8
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
    !use day15_mod
    use day15b_mod   ! faster implementation

    implicit none
    type(game_t) :: game
    !integer, parameter :: START(*) = [3, 1, 2]
    integer, parameter :: START(*) = [0,5,4,1,10,14,7]
    integer, parameter :: END1=2020, END2=30000000
    integer :: i, number
    character(len=*), parameter :: METER = &
"[                                                                            ]"
    real :: t0, t1

    call cpu_time(t0)
    call game % init(START, END2)
    do i=size(START)+1, END1
      call game % one_round()
    end do
    print '("Answer 15/1 is ",i0,l3)', game % prev_said, game%prev_said==203

    write(*,'(a)',advance='no') METER
    do i=1,len(METER)-1
      write(*,'(a)',advance='no') achar(8)
    enddo
    do i=END1+1, END2
      call game % one_round()
      !if (mod(i,1000000)==0) print '("Turn ",i0,"  Hash size ",i0)',game%time,game%hash%count()
      if (mod(i,END2/(len(METER)-2))==0) write(*,'(a)',advance='no') '.'
    end do
    write(*,*)
    print '("Answer 15/2 is ",i0,l3)', game % prev_said, game%prev_said==9007186
    call cpu_time(t1)
    print '("Time taken ",f8.3," seconds")', t1-t0
  end subroutine day15



  subroutine day16(file)
    use day16_mod
    use kinds_m, only : I8B
    implicit none
    character(len=*), intent(in) :: file
!
! DAY 16 - TICKET TRANSLATION
!
    type(field_t), allocatable :: fields(:)
    type(ticket_t) :: myticket
    type(ticket_t), allocatable :: all_tickets(:), val_tickets(:)
    integer :: i, ans1, val
    integer(I8B) :: ans2

    call read_from_file(file, fields, myticket, all_tickets)
    print '("No of fields = ",i0,"   No of tickets = ",i0)', size(fields), size(all_tickets)
    print '("My ",*(i3,1x))', myticket%aa

    ! Part 1
    ans1 = 0
    allocate(val_tickets(0))
    do i=1,size(all_tickets)
      !val = report_invalid_item(all_tickets(i), fields)
      val = all_tickets(i) % get_invalid(fields)
      if (val /= -1) ans1 = ans1 + val
      if (val == -1) val_tickets = [val_tickets, all_tickets(i)]
    end do
    print '("Answer 16/1 is ",i0,1x,l1)', ans1, 29759==ans1
    print '("No of valid tickets remaining ",i0)', size(val_tickets)

    ! Part 2
    val_tickets = [val_tickets, myticket] ! including own ticket is otional
    call make_maybe_pos(fields, val_tickets)
    call eliminate_unknown_fields(fields)
    ans2 = 1_I8B
    do i=1,size(fields)
      if (fields(i)%label(1:9)/='departure') cycle
      val = myticket%aa(fields(i)%pos)
      ans2 = ans2 * val
      print '(a20,1x,*(i3,1x))', fields(i)%label, fields(i)%pos, val
    end do
    print '("Answer 16/2 is ",i0,1x,l1)', ans2, 1307550234719_I8B==ans2
    print *
  end subroutine day16



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
    print '("Day17: Active cubes ",i0,l1)', ans, &
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



  subroutine day21(file)
    use day21_mod
    use list_mod, only : list_t
    implicit none
    character(len=*), intent(in) :: file
    type(food_t), allocatable :: foods(:)
    type(list_t) :: ingredients, allergens, unsafe
    integer :: i, j, ans1

    call read_from_file(file, foods)
    print '("No of foods ",i0)', size(foods)

    ! Identify which ingredient is associated with each allergen
    call make_lists(foods, ingredients, allergens, unsafe)

    print '("List of all ingredients")'
    print '("=======================")'
    print *, ingredients%print()
    print '("Total ingredients ",i0)', ingredients%size()
    print *
    print '("List of unsafe ingredients")'
    print '("==========================")'
    print *, unsafe%print()
    print '("Unsafe ingredients ",i0)', unsafe%size()
    print *
    print '("List of all allergens")'
    print '("=====================")'
    print *, allergens%print()
    print '("Total allergens ",i0)', allergens%size()
    print *

    ans1 = count_safe_ingredients(foods, unsafe)
    print '("Answer 21/1 is ",i0,l2)', ans1, ans1==5 .or. ans1==2428
    print *


    ! Part 2 - just sort the list
    call allergens % sort(unsafe)
    print '("List of all allergens/unsafe ingredients (Answer 21/2)")'
    print '("======================================================")'
    print '(a)', allergens%print()
    print '(a)', unsafe%print(',')
    print '(l1)', unsafe%print(',')=='bjq,jznhvh,klplr,dtvhzt,sbzd,tlgjzx,ctmbr,kqms'
    print *
  end subroutine day21



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
    print '("22/1: Winner is ",i0," with score ",i0)', game_winner, score
    print '(l1,a,f8.3)', score==32472 .or. score==306, "    Time taken ",t1-t0

    ! Part Two - Recursive combat
    g = game_t(file)
    call cpu_time(t0)
    call g % play_game(game_winner, .true.)
    score = g%score(game_winner)
    call cpu_time(t1)
    print '("22/2: Winner is ",i0," with score ",i0)', game_winner, score
    print '(l1,a,f8.3)', score==36463 .or. score==291,"    Time taken ",t1-t0
  end subroutine day22



  subroutine day23(input)
    use day23_mod
    use kinds_m, only : I8B
    implicit none
    character(len=*), intent(in) :: input
    type(circle_t) :: cups1, cups2
    integer :: i
    type(cup_t), pointer :: ca, cb
    integer(I8B) :: mult
    real :: t0, t1

    call cpu_time(t0)
    cups1 = circle_t(input,9)
    do i=1,100
      call cups1 % move()
    end do
    print '(a)', 'Final state - 23/1'
    call cups1 % print()

    ! Part 2
    cups2 = circle_t(input, 1000000)
    do i=1,10000000
      call cups2 % move()
    end do

    ca => cups2 % cups(1)%ptr%next
    cb => ca % next
    mult = int(ca%id,kind=I8B) * int(cb%id, kind=I8B)
    print '(/a)', 'Simulation complete for 23/2'
    print '("Numbers behind 1 are ",i0," * ",i0," = ",i0)',ca%id, cb%id, mult
    print '("Valid? ", l1)', 11591415792_I8B==mult .or. 149245887792_I8B==mult
    call cpu_time(t1)
    print '("Time taken ",f8.3," seconds.")', t1-t0
    print *

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
    print *, 'Answer to 24/1 is :', ans, ans==360 .or. ans==10
    call flr%print()
    do i=1,100
      call flr % expand()
    end do
    call flr%print()
    ans = flr%blacks()
    print *, 'Answer to 24/2 is :', ans, ans==2208 .or. ans==3924
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
    print '("Ans 25/1 key  = ",i8,1x,l1)', enc_key, enc_key==5025281 .or. enc_key==14897079
  end subroutine day25
