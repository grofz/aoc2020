  program main
    implicit none

    !call day01('inp/01/input.txt')
    !call day01('inp/01/sample.txt')

    call day02('inp/02/input.txt')
    !call day02('inp/02/sample.txt')
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
