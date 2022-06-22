  module day03_mod
    implicit none
    private
    public count_trees

  contains

    function count_trees(aa, slope) result(cnt)
      integer :: cnt
      character(len=1), intent(in) :: aa(:,:)
      integer, intent(in) :: slope(2)
      character(len=*), parameter :: TREE='#'

      integer :: row, col, cstep, rstep, i, j

      rstep = slope(1)
      cstep = slope(2)
      row = size(aa,dim=1)
      col = size(aa,dim=2)
      cnt = 0
      j = 1-cstep
      do i=1,row,rstep
        j = mod(j+cstep-1, col)+1
        if (aa(i,j)==TREE) cnt = cnt + 1
      end do
    end function count_trees

  end module day03_mod
