let state = ref 0

let next () =
  let id = !state in
  incr state;
  id

let reset () = state := 0
