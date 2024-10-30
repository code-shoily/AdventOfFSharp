namespace Common

module Helpers =
    let unreachable () = failwith "unreachable"
    let todo () = failwith "todo"

    /// <summary>
    /// Returns the single line
    /// </summary>
    let oneLiner = Seq.exactlyOne

    /// <summary>
    /// Collects paragraphs (i.e. groups data separated by "" or "\n\n" in String form)
    /// </summary>
    /// <param name="input">Sequence of String as received from file</param>
    let paragraphs input =
        let rec construct paragraph result =
            function
            | [] -> List.rev (paragraph :: result)
            | "" :: rest -> construct [] (paragraph :: result) rest
            | head :: rest -> construct (head :: paragraph) result rest

        construct [] [] input

module Types =
    type Solution =
        | BothInt of int * int
        | StringInt of string * int
        | Todo

    type SolutionError =
        | FileNotFound
        | InvalidInput
        | NotDoneYet
