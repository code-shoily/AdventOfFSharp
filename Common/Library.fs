namespace Common

module Types =
    type Solution =
        | BothInt of int * int
        | IntString of int * string
        | Todo

    type SolutionError =
        | FileNotFound
        | InvalidInput
        | NotDoneYet
