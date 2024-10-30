namespace Common

module Types =
    type Solution =
        | BothInt of int * int
        | StringInt of string * int
        | Todo

    type SolutionError =
        | FileNotFound
        | InvalidInput
        | NotDoneYet
