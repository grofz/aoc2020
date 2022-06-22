  program main
    implicit none

    !call day01('inp/01/input.txt')
    !call day01('inp/01/sample.txt')

    !call day02('inp/02/input.txt')
    !call day02('inp/02/sample.txt')

    call day03('inp/03/input.txt')
    !call day03('inp/03/sample.txt')
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
