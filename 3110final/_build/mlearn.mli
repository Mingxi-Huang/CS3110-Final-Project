(** [vectorized_board_state] is a 3D int array that represent the board
    state: [\[0, 0, … , 1\] * 9]*9 len[0][0] = 14, representing 14
    unique pieces and is an one-hot representation of piece. *)
type vectorized_board_state = int array array array

(** [move] represent a move in a certain round. It has the form ((x1,
    y1),(x2, y2)), the first coordinate is the start of move and the
    second coordinate is the destiny. *)
type move = (int * int) * (int * int)

(** [populate_train filename train_data] is a representation of our data
    stored in the csv file [filename]*)
val populate_train : string -> string array array -> string array array

(** [vec_piece rank side] is the one hot representation of a piece of
    [rank] and from [side]*)
val vec_piece : Piece.rank -> Piece.side -> int list

(** [translate_lines board yaxis] takes a single axis [yaxis] of [board]
    and turn it into vectorized row. *)
val translate_lines : Board.t -> int -> int array array

val translate_board : Board.t -> vectorized_board_state

(** [get_start_coord rank board start_x] is the coordinate of the piece
    on column [start_x] of board [board] refered by string [rank]*)
val get_start_coord : string -> Board.t -> string -> int * int

(** [get_end_coord start state oper side end_x] is the ending coordinate
    of the piece starting at [start] after operation [oper] and [end_x]
    on board of [state] *)
val get_end_coord :
  int * int -> State.t -> string -> string -> int -> int * int

(** [translate_coord state_ref move_str] is the start and end coord for
    board in [state_ref] after translating [move_str]*)
val translate_coord : State.t ref -> string -> move

val simulate_round :
  string array array -> (vectorized_board_state * move) array
