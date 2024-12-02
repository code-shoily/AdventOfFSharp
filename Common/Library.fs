namespace Common

module Types =
    type Solution =
        | BothInt of int * int
        | BothLong of int64 * int64
        | BothString of string * string
        | IntString of int * string
        | StringInt of string * int
        | Todo

    type SolutionError =
        | FileNotFound
        | InvalidInput
        | NotDoneYet
