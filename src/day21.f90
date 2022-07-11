!
! DAY21 - ALLERGEN ASSESMENT
!
  module day21_mod
    use parse_mod, only : chop_string, parse_array
    use list_mod, only : list_t, CHAR_LEN
    implicit none

    type food_t
      type(list_t) :: ingr
      type(list_t) :: aler
    end type food_t 
    interface food_t
      module procedure food_new
    end interface

  contains

    integer function count_safe_ingredients(foods, unsafe) result(cnt)
      type(food_t), intent(in) :: foods(:)
      type(list_t), intent(in) :: unsafe
      integer :: i, j
!
! Get answer for part 1
!
      cnt = 0
      do i=1,size(foods)
      do j=1,foods(i)%ingr%size()
        if (unsafe % find(foods(i) % ingr%get(j)) == 0) cnt = cnt+1
      end do
      end do
    end function 



    subroutine make_lists(foods, all_ingr, all_aler, unsafe_ingr)
      type(food_t), intent(in) :: foods(:)
      type(list_t), intent(out) :: all_ingr, all_aler, unsafe_ingr
!
! Identification of alergens. Return list of unsafe ingredients.
!
      integer :: i, j, key_aler
      character(len=CHAR_LEN) :: current_alergen, identified_ingredient
      type(list_t), allocatable :: ingr(:)
      logical, allocatable :: alergen_identified(:)

      ! Lists of all ingredients and alergens ("all_ingr" and "all_aler")
      all_ingr = list_t()
      all_aler = list_t()
      do i=1,size(foods)
        do j=1,foods(i) % ingr % size()
          call all_ingr % add(foods(i)%ingr%get(j))
        end do
        do j=1,foods(i) % aler % size()
          call all_aler % add(foods(i)%aler%get(j))
        end do
      end do

      ! For each alergen make intersections of ingredients list from all foods ("ingr")
      allocate(ingr(all_aler % size()))
      allocate(alergen_identified(all_aler % size()))
      do i=1, all_aler%size()
        ingr(i) = list_t() ! list must be initialized as empty
      end do
      alergen_identified = .false.

      do i=1, size(foods)
      do j=1, foods(i)%aler%size()
        current_alergen = foods(i)%aler%get(j)
        key_aler = all_aler % find(current_alergen)
        if (key_aler<1 .or. key_aler>size(ingr)) error stop 'make_lists - key_aler invalid'
        if (ingr(key_aler) % size() == 0) then
          ! first list of ingredients - make just copy
          ingr(key_aler) = list_t(foods(i)%ingr%export())
        else
          ! second and next list of ingredients - do intersection operation
          ingr(key_aler) = ingr(key_aler) .and. foods(i)%ingr
        end if
      end do
      end do

      ! If there is a list with just one entry: ingredient for this alergen has been identified
      ! and that ingredient can be removed from other lists. Repeat until possible
      do
        print '("Unidentified alergens ",i0)', count(.not. alergen_identified)
        do i=1,all_aler%size()
          if (ingr(i)%size()==0) error stop 'make lists - no possible ingredient'
          if (alergen_identified(i)) cycle
          if (ingr(i)%size()==1) exit
        end do
        if (i==all_aler%size()+1) exit ! no more identification possible

        alergen_identified(i) = .true.
        identified_ingredient = ingr(i)%get(1)
        do j=1,all_aler%size()
          if (alergen_identified(j)) cycle
          call ingr(j) % remove(identified_ingredient)
        end do
      end do

      ! Display what alergens were identified
      print '("Identification results:")'
      do i=1, all_aler%size()
        print '("Alergen: ",a,l2)', all_aler%get(i), alergen_identified(i)
        print '(a)', ingr(i)%print()
      end do
      print *

      ! Copy all ingredients left on the list to the list of unsafe ingredients
      unsafe_ingr = list_t()
      do i=1, all_aler%size()
        do j=1,ingr(i)%size()
          call unsafe_ingr % add(ingr(i)%get(j))
        end do
      end do
    end subroutine make_lists



    subroutine read_from_file(file, foods)
      character(len=*), intent(in) :: file
      type(food_t), intent(out), allocatable :: foods(:)
      integer :: fid, ios
      character(len=1000) :: line
      type(food_t) :: newfood

      open(newunit=fid, file=file, status='old')
      allocate(foods(0))
      do
        read(fid,'(a)', iostat=ios) line 
        if (ios /= 0) exit
        newfood = food_t(line)
        foods = [foods, newfood]
      end do
      close(fid)
    end subroutine read_from_file



    type(food_t) function food_new(line) result(this)
      character(len=*), intent(in) :: line

      character(len=len(line)) :: a1, a2, a3
      character(len=CHAR_LEN), allocatable :: ingredients(:), allergens(:)

      ! first half are ingredients separated by space
      a1 = line
      call chop_string(a1,'(',a2)
      ingredients = parse_array(a1,' ',CHAR_LEN)

      ! remove "contains ... )" from the second half
      a1 = adjustl(a2)
      call chop_string(a1,' ',a2)
      if (trim(a1) /= 'contains') error stop 'food_new - contains expected'
      a1 = adjustl(a2)
      call chop_string(a1,')',a2)
      if (len_trim(a2) /= 0) error stop 'food_new - something left behind ")"'
      allergens = parse_array(a1,',',CHAR_LEN)

      ! store both arrays into list
      this % ingr = list_t(ingredients)
      this % aler = list_t(allergens)
      !print '("Food with ",i0," ingredients and ",i0," alergens")', this%ingr%size(),this%aler%size() 
    end function food_new

  end module day21_mod
