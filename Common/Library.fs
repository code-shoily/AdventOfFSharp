namespace Common

module Helpers =
    let unreachable () = failwith "unreachable"
    let todo () = failwith "todo"

module Types =
    type Solution =
        | BothInt of int * int
        | StringInt of string * int
        | Todo

    type SolutionError =
        | FileNotFound
        | InvalidInput
        | NotDoneYet
