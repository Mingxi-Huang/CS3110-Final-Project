type point = int * int

type state = {
  label : string;
  coord : point;
  occupied : bool;
}

let get_label state = state.label

let get_coord state = state.coord

let get_occupy state = state.occupied

let init_states = []

let create_state l c o = { label = l; coord = c; occupied = o }
