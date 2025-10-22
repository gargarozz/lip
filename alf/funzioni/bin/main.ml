let () = print_endline "Hello, World!";;

type ritorno = Ok of int | Err;;

let fun1 a = a - 1;;

let fun2 = function
  x when x >= 0 -> Ok(x+1)
| _ -> Err;;

let fun3 = function
  x when x mod 2 = 0 -> Ok(x)
| _ -> Err;;

(* scrivere una funzione non iniettiva e non suriettiva N -> N *)

