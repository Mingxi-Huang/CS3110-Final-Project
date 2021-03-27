(** Representation of dynamic state in Xiangqi.

    This module represents the state of a Xiangqi game as it is being
    played, including the player's current coordinates, and functions
    that cause the state to change. *)
type state

type point

val get_label : state -> string

val get_coord : state -> point

val get_occupy : state -> bool

val init_states : state list

val create_state : string -> point -> bool -> state
