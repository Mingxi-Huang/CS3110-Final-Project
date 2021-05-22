type vectorized_board_state = int array array array

type move = (int * int) * (int * int)

val translate_board : Board.t -> vectorized_board_state
