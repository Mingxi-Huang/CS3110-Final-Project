(** Representation of dynamic state in Xiangqi.

    This module represents the state of a Xiangqi game as it is being
    played, including the player's current coordinates, and functions
    that cause the state to change. *)
open Board

type turn

type result

(** [init_state] is the side that starts the game. We initializes it to
    be the [Black] side*)
val init_state : turn

(** [go] evaluated if the input movement is legal. *)
val go : turn -> piece -> int * int -> Board.t -> result
