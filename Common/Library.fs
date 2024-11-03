namespace Common

module Types =
    type Solution =
        | BothInt of int * int
        | BothString of string * string
        | IntString of int * string
        | Todo

    type SolutionError =
        | FileNotFound
        | InvalidInput
        | NotDoneYet
