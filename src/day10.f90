!
! DAY!): Adapter array
!
  module day10_mod
    implicit none
    private
    public count_dif, mark_essential, extract_chain, count_valid_combinations
  contains

    pure function count_dif(aa, dif) result(cnt_dif)
      integer, intent(in) :: aa(:)
      integer, intent(in) :: dif(:)
      integer :: cnt_dif(size(dif))
!
! Count the number of "diff" diferences in the array
!
      integer :: idif, ia

      do idif = 1, size(dif)
        cnt_dif(idif) = 0
        do ia = 1, size(aa)-1
          if (aa(ia+1)-aa(ia) == dif(idif)) &
              cnt_dif(idif) = cnt_dif(idif) + 1
        end do
      end do
    end function count_dif


    pure function mark_essential(aa, maxd) result (is_essential)
      integer, intent(in) :: aa(:), maxd
      logical :: is_essential(size(aa))
!
! mark elements having gaps to neighbours larger than "maxd"
!
      integer :: i
      is_essential(1) = .true.
      is_essential(size(aa)) = .true.
      do i=2, size(aa)-1
        if (aa(i)-aa(i-1) >= maxd .or. aa(i+1)-aa(i) >= maxd) then
          is_essential(i) = .true.
        else
          is_essential(i) = .false.
        end if
      end do
    end function mark_essential


    subroutine extract_chain(aa, is_essential, ipos, chain)
      integer, intent(in) :: aa(:)
      logical, intent(in) :: is_essential(:)
      integer, intent(inout) :: ipos
      integer, allocatable, intent(out) :: chain(:)
!
! Find a chain of non-essential items from position "ipos" in input arrays
!
      integer :: na, nchain, istart

      na = size(aa)
      if (size(is_essential) /= na) error stop 'extract_chain - input arrays not same size'

      ! Find a first non-essential item.
      ! Return empty list if reached the end of array.
      do
        if (ipos > na) then
          allocate(chain(0))
          return
        end if
        if (.not. is_essential(ipos)) exit
        ipos = ipos + 1
      end do
      if (ipos==1 .or. ipos==na) error stop 'extract_chain - first and last items must be essential'

      ! Where the chain of non-essential items ends?
      istart = ipos - 1
      do
        if (is_essential(ipos)) exit
        ipos = ipos + 1
        if (ipos>na) error stop 'extract_chain - out of array bounds, last item must be essential'
      end do
      nchain = ipos - istart + 1

      ! Extract non-essential items (first and last items are essential)
      allocate(chain(nchain))
      chain = aa(istart:ipos)
    end subroutine extract_chain


    pure function count_valid_combinations(aa, maxd) result(nvalid)
      integer, intent(in) :: aa(:), maxd
      integer :: nvalid
!
! Test if switching off middle elements makes gaps no larger than "maxd"
!
      logical :: mask(size(aa)-2)
      integer, allocatable :: bb(:)
      integer :: icomb, ncomb, nbits

      nbits = size(aa)-2
      if (nbits<1) error stop 'count_valid_combinations - input too small'
      ncomb = 2**nbits
      nvalid = 0
      do icomb = 0, ncomb-1
        mask = num2bits(icomb, nbits)
        bb = [aa(1), pack(aa(2:size(aa)-1), mask), aa(size(aa))]
        if (is_valid_combination(bb, maxd)) nvalid = nvalid + 1
      end do
    end function count_valid_combinations


    pure logical function is_valid_combination(bb, maxd) result(res)
      integer, intent(in) :: bb(:), maxd
      integer :: i

      if (size(bb)<2) error stop 'is_valid_combination - minimum size is 2'
      ! jump over all gaps, invalidate if gap is larger than "maxd"
      res = .true.
      do i=2, size(bb)
        if (bb(i)-bb(i-1) <= maxd) cycle
        res = .false.
        exit
      end do
    end function is_valid_combination


    pure function num2bits(num, nbits) result(bits)
      integer, intent(in) :: num, nbits
      logical :: bits(nbits)
      integer :: i, num0
      num0 = num
      bits = .false.
      do i=1, nbits
        if (num0 / 2**(nbits-i)==1) bits(i) = .true.
        num0 = num0 - bool2int(bits(i)) * 2**(nbits-i)
      end do
    contains
      pure integer function bool2int(bool)
        logical, intent(in) :: bool
        bool2int = 0
        if (bool) bool2int = 1
      end function
    end function num2bits

  end module day10_mod
