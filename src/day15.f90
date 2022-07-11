!
! DAY 15 - Recitation
!
!
! This implementation uses red-hlack tree as a map. It is much slower than
! version B which uses just large preallocated array.
!
module day15_mod
  use tree_m, only : rbtr_t, tree_mold, DAT_KIND
  use abstract_container, only : ERR_CONT_OK, ERR_CONT_ISNOT, ERR_CONT_IS
  implicit none
  private
  public game_t

  type hash_t
    integer :: key
    integer, pointer :: val ! to update its value directly
  end type

  type game_t
    type(rbtr_t) :: hash
    integer :: time
    integer :: prev_said
  contains
    procedure :: init => game_init
    procedure :: one_round => game_next
    final :: game_finalize
  end type game_t

contains

  subroutine game_finalize(this)
    type(game_t), intent(inout) :: this
    type(hash_t) :: dat
    integer(DAT_KIND), allocatable :: handle(:)
    integer :: ierr
    call this%hash % Resetcurrent(handle)
    do
      dat = transfer(this%hash % NextRead(handle,ierr), dat)
      if (ierr /= 0) exit
      deallocate(dat%val)
    end do
    call this%hash % Removeall()
  end subroutine



  subroutine game_init(this, starting_arr, idummy)
    class(game_t), intent(out) :: this
    integer, intent(in) :: starting_arr(:)
    integer, intent(in) :: idummy ! unused in TREE implementation
    integer :: n0, i

    this%hash = rbtr_t(hash_compare) ! must be initialized
    n0 = size(starting_arr)
    this%time = n0
    this%prev_said = starting_arr(n0)
    do i=1, n0-1
      call hash_update(this%hash, starting_arr(i), i)
      ! WARNING - elements in starting_arr must be unique
    end do
  end subroutine game_init



  subroutine game_next(this)
    class(game_t), intent(inout) :: this
    integer :: val
    integer(DAT_KIND), allocatable :: handle(:)

    val = hash_get(this%hash, this%prev_said, handle)
    if (val == 0) then
      call hash_update(this%hash, this%prev_said, this%time)
      this%prev_said = 0
    else
      call hash_update(this%hash, this%prev_said, this%time, handle)
      this%prev_said = this%time - val
    end if
    this%time = this%time + 1
 !print '("Round =",i0," number said= ",i0,"  val was ",i0)',this%time,this%prev_said,val
  end subroutine game_next



  subroutine hash_update(hash, key, val, handle)
    type(rbtr_t), intent(inout) :: hash
    integer, intent(in) :: key, val
    integer(DAT_KIND), allocatable, intent(in), optional :: handle(:)
!
! Update pair "key - val" in hash, or store new one.
! We made search before: "handle" present -> update the value
! "handle" not present -> add new node
!
    type(hash_t) :: olddat, newdat
    integer :: ierr

    olddat % key = key
    newdat % key = key

    if (present(handle)) then
      ierr = ERR_CONT_OK
    else
      !call hash % Find(transfer(olddat,tree_mold), handle, ierr)
      ierr = ERR_CONT_ISNOT
    end if

    select case(ierr)
    case(ERR_CONT_OK)
      !call hash % UpdateCurrent(handle, transfer(newdat,tree_mold))
      olddat = transfer(hash % Read(handle), olddat)
      olddat % val = val
    case(ERR_CONT_ISNOT)
      allocate(newdat%val)
      newdat % val = val
      call hash % Add(transfer(newdat,tree_mold))
    case default
      error stop 'hash_update - uknown exit code'
    end select
  end subroutine hash_update



  integer function hash_get(hash, key, handle) result(val)
    type(rbtr_t), intent(in) :: hash
    integer, intent(in) :: key
    integer(DAT_KIND), allocatable, intent(out) :: handle(:)
!
! Ask, if "key" has been stored. Return "val" or "0" (no entry)
!
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
