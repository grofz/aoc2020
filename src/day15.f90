module day15_mod
  use tree_m, only : rbtr_t, tree_mold, DAT_KIND
  use abstract_container, only : ERR_CONT_OK, ERR_CONT_ISNOT, ERR_CONT_IS
  implicit none
  private
  public game_t

  type hash_t
    integer :: key
    integer :: val
  end type

  type game_t
    type(rbtr_t) :: hash
    integer :: time
    integer :: prev_said
  contains
    procedure :: init => game_init
    procedure :: one_round => game_next
  end type game_t

contains

  subroutine game_init(this, starting_arr)
    class(game_t), intent(out) :: this
    integer, intent(in) :: starting_arr(:)
    integer :: n0, i

    this%hash = rbtr_t(hash_compare) ! must be initialized
    n0 = size(starting_arr)
    this%time = n0
    this%prev_said = starting_arr(n0)
    do i=1, n0-1
      call hash_update(this%hash, starting_arr(i), i)
    end do
  end subroutine game_init



  subroutine game_next(this)
    class(game_t), intent(inout) :: this
    integer :: val

    val = hash_get(this%hash, this%prev_said)
    call hash_update(this%hash, this%prev_said, this%time)
    if (val == 0) then 
      this%prev_said = 0
    else
      this%prev_said = this%time - val
    end if
    this%time = this%time + 1
 !print '("Round =",i0," number said= ",i0,"  val was ",i0)',this%time,this%prev_said,val
  end subroutine game_next



  subroutine hash_update(hash, key, val)
    type(rbtr_t), intent(inout) :: hash
    integer, intent(in) :: key, val
!
! Update pair "key - val" in hash, or store new one
!
    type(hash_t) :: olddat, newdat
    integer :: ierr

    olddat % key = key
    newdat % key = key
    newdat % val = val
    if (hash % Isin(transfer(olddat,tree_mold))) then
      call hash % Update(transfer(olddat,tree_mold), transfer(newdat,tree_mold))
    else
      call hash % Add(transfer(newdat,tree_mold))
    end if
  end subroutine hash_update



  integer function hash_get(hash, key) result(val)
    type(rbtr_t), intent(in) :: hash
    integer, intent(in) :: key
!
! Ask, if "key" has been stored. Return "val" or "0" (no entry)
!
    integer(DAT_KIND), allocatable :: handle(:)
    integer :: ierr
    type(hash_t) :: dat

    dat % key = key
    call hash % Find(transfer(dat,tree_mold), handle, ierr)
    select case(ierr)
    case(ERR_CONT_OK)
      dat = transfer(hash % Read(handle), dat)
      if (dat % key /= key) error stop 'hash_get - wrong key returned'
      val = dat % val
    case(ERR_CONT_ISNOT)
      val = 0
    case default
      error stop 'hash_get - uknown exit code'
    end select
  end function hash_get



  pure integer function hash_compare(a, b) result(res)
    integer(DAT_KIND), intent(in) :: a(:), b(:)
!
! user function for the TREE hash
!
    type(hash_t) :: ahash, bhash
    ahash = transfer(a, ahash)
    bhash = transfer(b, bhash)

    if (ahash%key == bhash%key) then
      res = 0
    else if (ahash%key < bhash%key) then
      res = 1
    else
      res = -1
    end if
  end function hash_compare

end module day15_mod
