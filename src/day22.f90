  module day22_mod
    use queue_mod, only : queue_t
    use tree_m, only : rbtr_t, tree_mold, DAT_KIND
    use abstract_container, only : ERR_CONT_OK, ERR_CONT_ISNOT
    implicit none

    private
    public game_t

    ! game configuration is stored in a 1D array using tree
    integer, parameter :: STORED_CARDS = 50
    integer, parameter :: I1B = selected_int_kind(2)
    type configuration_t
      integer(I1B) :: aa(2*STORED_CARDS)=-1
    end type configuration_t
    type configuration_ptr
      type(configuration_t), pointer :: ptr
    end type configuration_ptr


    type game_t
      type(queue_t) :: players(2)
    contains
      procedure :: play_game => game_play_game
      procedure :: simple_round => game_simple_round
      procedure :: score => game_score
    end type game_t
    interface game_t
      module procedure game_new, game_subgame
    end interface game_t

  contains

    recursive subroutine game_play_game(this, game_winner, is_recursive_combat)
     class(game_t), intent(inout) :: this
      integer, intent(out) :: game_winner
      logical, intent(in) :: is_recursive_combat

      type(rbtr_t) :: T
      type(configuration_ptr) :: adat
      integer(DAT_KIND), allocatable :: handle(:)
      integer, allocatable :: tmp(:)

      T = rbtr_t(userfun)
      do
        ! check if there is current configuration saved and save current
        allocate(adat % ptr)
        tmp = this % players(1) % export()
        adat%ptr%aa(1:size(tmp))=tmp
        tmp = this % players(2) % export()
        adat%ptr%aa(STORED_CARDS+1:STORED_CARDS+size(tmp))=tmp
        if (T % isin(transfer(adat,tree_mold))) then
          deallocate(adat%ptr)
          game_winner = 1 ! instant winner according to rules
          exit
        endif
        call T % Add(transfer(adat,tree_mold))
        nullify(adat%ptr)

        ! play one round
        call this % simple_round(game_winner, is_recursive_combat)
        if (game_winner /= 0) exit
      end do

      ! clean tree (destructor)
      do
        if (T % count() == 0) exit
        call T % Firstnode(handle)
        adat = transfer(T % Read(handle), adat)
        call T % remove(T % Read(handle))
        deallocate(adat % ptr)
      enddo
    end subroutine game_play_game


    subroutine game_simple_round(this, game_winner, is_recursive_combat)
      class(game_t), intent(inout) :: this
      integer, intent(out) :: game_winner
      logical, intent(in) :: is_recursive_combat
      integer :: cards(2), winner, looser
      type(game_t) :: sub_game

      if (this % players(1) % size() == 0 .or. &
          this % players(2) % size() == 0) error stop 'simple round - already won'

      ! Both players deal a card
      call this % players(1) % remove(cards(1))
      call this % players(2) % remove(cards(2))

      ! Play a sub-game to determine winner of this round...
      if (this % players(1) % size() >= cards(1) .and. &
          this % players(2) % size() >= cards(2) .and. is_recursive_combat) then
        sub_game = game_t(this, cards(1), cards(2))
        call sub_game % play_game(winner, is_recursive_combat)

      ! ..or determine winner according to classic rules
      else
        if (cards(1) > cards(2)) then
          winner = 1
        else if (cards(2) > cards(1)) then
          winner = 2
        else
          error stop 'one_round - both cards same value'
        end if
      end if

      ! Winner retakes his own's card, then his oponent's card
      looser = 3-winner
      call this % players(winner) % insert(cards(winner))
      call this % players(winner) % insert(cards(looser))

      ! Was this the last round?
      if (this % players(1) % size() == 0) then
        game_winner = 2
      else if (this % players(2) % size() == 0) then
        game_winner = 1
      else
        game_winner = 0
      end if
    end subroutine game_simple_round


    integer function game_score(this, player) result(score)
      class(game_t), intent(in) :: this
      integer, intent(in) :: player
      score = calc_score(this % players(player) % export())
    end function game_score


    integer function calc_score(arr) result(score)
      integer, intent(in) :: arr(:)
      integer :: i
      score = 0
      do i=1,size(arr)
        score = score + (size(arr)-i+1)*arr(i)
      end do
    end function calc_score


    type(game_t) function game_new(file) result(this)
      character(len=*), intent(in) :: file
      integer :: fid
      integer, allocatable :: tmp(:)

      open(newunit=fid, file=file, status='old')
      call read_cards_from_file(fid, tmp)
      this % players(1) = queue_t(tmp)
      call read_cards_from_file(fid, tmp)
      this % players(2) = queue_t(tmp)
      close(fid)
    end function game_new


    type(game_t) function game_subgame(main_game, nc1, nc2) result(sub_game)
      class(game_t), intent(in) :: main_game
      integer, intent(in) :: nc1, nc2
      integer, allocatable :: arr(:)
      arr = main_game%players(1)%export()
      if (size(arr) < nc1) error stop 'game_subgame - not enough cards player 1'
      sub_game%players(1) = queue_t(arr(1:nc1))

      arr = main_game%players(2)%export()
      if (size(arr) < nc2) error stop 'game_subgame - not enough cards player 2'
      sub_game%players(2) = queue_t(arr(1:nc2))
    end function game_subgame


    subroutine read_cards_from_file(fid, arr)
      integer, intent(in) :: fid
      integer, allocatable, intent(out) :: arr(:)
      integer, parameter :: MAX_CARDS = 100
      integer :: tmp(MAX_CARDS), n, ios
      character(len=100) line

      line = '?'
      read(fid,'(a)',iostat=ios) line
      if (line(1:6)/='Player' .or. ios /= 0) error stop 'read_cards_from_file - expecting header'
      n=0
      do
        read(fid,'(a)',iostat=ios) line
        if (ios /= 0) exit
        if (line=='') exit

        n = n + 1
        read(line,*) tmp(n)
      end do
      allocate(arr(n))
      arr = tmp(1:n)
    end subroutine read_cards_from_file



    pure function userfun(a, b)
        use tree_m, only : DAT_KIND
        integer(DAT_KIND), intent(in) :: a(:), b(:)
        integer :: userfun
  !
  ! user function attached to TREE storing configuration
  !
        type(configuration_ptr) :: adat, bdat
        integer :: i

        adat = transfer(a, adat)
        bdat = transfer(b, adat)

        userfun = 0
        do i=1, size(adat%ptr%aa)
          if (adat%ptr%aa(i) == bdat%ptr%aa(i)) then
            cycle
          else if (adat%ptr%aa(i) < bdat%ptr%aa(i)) then
            userfun = 1
            exit
          else
            userfun = -1
            exit
          end if
        end do
      end function userfun
      
  end module day22_mod
