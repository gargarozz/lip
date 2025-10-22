open BoolexprLib.Main

let test_eval expr exp_result =
  (expr |> parse |> eval) = exp_result

(* ### Unit tests for task 4 *)

let%test "test_eval1" = test_eval "false" false
let%test "test_eval2" = test_eval "true" true
let%test "test_eval3" = test_eval "if true then false else true" false
let%test "test_eval4" = test_eval "if false then false else true" true
let%test "test_eval5" = test_eval "if true then (if true then false else true) else (if true then true else false)" false
let%test "test_eval6" = test_eval "if (if false then false else false) then (if false then true else false) else (if true then false else true)" false
let%test "test_eval7" = test_eval "if (if (if false then false else false) then (if false then true else false) else (if true then false else true)) then (if false then true else false) else (if true then false else true)" false

(* ### Unit tests for task 5 *)

let%test "test_trace1_1" = failwith "TODO"
