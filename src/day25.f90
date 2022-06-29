  module day25_mod
    implicit none
    private
    integer, parameter, public :: I8 = selected_int_kind(18)
    integer(I8), parameter :: DIVISOR = 20201227_I8
    integer(I8), parameter, public :: START_VAL = 7_I8

    public brute_force_ls, handshake

    contains

      pure function transform(subject_number, loop_size) result(val)
        integer(I8) :: val
        integer(I8), intent(in) :: subject_number, loop_size

        integer(I8) :: i

        val = 1
        do i = 1, loop_size
          val = val * subject_number
          val = mod(val, DIVISOR)
        end do
    end function



    function brute_force_ls(sn, key) result(ls)
      integer(I8), intent(in) :: sn, key
      integer(i8) :: ls
      integer(i8) :: val, i
      integer(I8), parameter :: MAXLS = 100000000_I8

      val = 1
      do i = 1, MAXLS
        val = val * sn
        val = mod(val,DIVISOR)
        if (val == key) exit
      end do
      if (i == MAXLS+1) error stop 'not able to break ls, increase MAXLS'
      ls = i
    end function brute_force_ls



    pure function handshake(card_ls, door_ls, card_key, door_key) result(key)
      integer(I8) :: key
      integer(I8), intent(in) :: card_ls, door_ls, card_key, door_key

      integer(I8) :: local_card_key, local_door_key, enc_door, enc_card

      local_card_key = transform(START_VAL, card_ls)
      local_door_key = transform(START_VAL, door_ls)
      if (local_card_key /= card_key) then
        key = -1
      else if (local_door_key /= door_key) then
        key = -2
      else
        enc_card = transform(local_door_key, card_ls)
        enc_door = transform(local_card_key, door_ls)
        if (enc_card == enc_door) then
          key = enc_card
        else
          key = -3
        end if
      end if
    end function handshake
  end module day25_mod

