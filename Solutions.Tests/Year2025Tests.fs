module Year2025Tests

open Common.Helpers
open Common.Types
open Solutions.Tests
open Year2025

open Xunit

module TestYear2025Solutions =
    let getSolver day : (string seq -> Solution) =
        match day with
        | _ -> unreachable ()

    let getInput = Helpers.getInput 2025

    // Add tests here as days are implemented
    // let isExpectedFor = Helpers.isExpectedForUtil getSolver 2025
