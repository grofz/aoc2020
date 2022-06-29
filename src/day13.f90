  module day13_mod
    implicit none
    integer, parameter :: I8 = selected_int_kind(18)

  contains
    pure function tnext(tnow,id)
      integer(I8) :: tnext
      integer(I8), intent(in) :: tnow, id
      tnext = tnow - mod(tnow,id) + id
    end function

    subroutine disect_line(line,arr)
      character(len=*), intent(in) :: line
      integer(I8), allocatable, intent(out) :: arr(:)

      character(len=:), allocatable :: str, cut, newstr
      integer(I8) :: cutnum

      str = trim(line)
      allocate(arr(0))
      do
        call cut_from_string(str,cut,newstr)
        if (cut=='x') then
          cutnum = -1
        else
          read(cut,*) cutnum
        end if
        arr = [arr, cutnum]
        if (len(newstr)==0) exit
        str = newstr
      end do
    end subroutine

    subroutine cut_from_string(str,cut,newstr)
      character(len=*), intent(in) :: str
      character(len=:), allocatable, intent(out) :: cut, newstr

      integer :: i

      i = scan(str,',')
      if (i==0) then
        cut = trim(str)
        newstr = ''
      else
        cut = str(:i-1)
        newstr = str(i+1:)
      end if
    end subroutine

    pure function next_time(time, mp, id, wt)
      integer(I8) :: next_time
      integer(I8), intent(in) :: time, mp, id, wt

      next_time = time
      do
        if (mod(next_time+wt, id) == 0) return
        next_time = next_time + mp
      end do
    end function next_time

  end module day13_mod
