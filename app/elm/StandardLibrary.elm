module StandardLibrary exposing (compiledFunctions)

import Compiler.Ast as Ast exposing (CompiledFunction)
import Compiler.Parser as Parser
import Dict
import Parser.Advanced as Parser


{-| `functions` contains the source code for the functions contained in
elm-logo’s standard library. As far as possible, UCBLogo’s implementation is
copied.

The first function, `combine`, was taken from UCBLogo’s [source code][combine]
and only slightly modified. `bury "combine` was removed because burying is not
yet implemented in elm-logo.

[combine]: https://github.com/jrincayc/ucblogo-code/blob/master/logolib/combine

-}
functions : String
functions =
    """to combine :this :those
if wordp :those [output word :this :those]
output fput :this :those
end
"""


compiledFunctions : List CompiledFunction
compiledFunctions =
    let
        parser =
            Parser.withExistingFunctions Dict.empty

        compiledProgram =
            functions
                |> Parser.run parser
                |> Result.map Ast.compileProgram
    in
    compiledProgram
        |> Result.map .compiledFunctions
        |> Result.withDefault []
