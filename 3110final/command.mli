(**The type [piece_phrase] represents the piece phrase that can be
    part of a player command.*)

type select_phrase = string
type mov_phrase = string

type piece_phrase = {
    select_phrase: select_phrase;
    mov_phrase: mov_phrase;
}

type command =
    | Go of piece_phrase
    | Quit

exception Empty
exception Malformed

val parse : string -> command