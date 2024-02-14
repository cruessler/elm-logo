module StandardLibrary exposing (compiledFunctions)

import Compiler.Ast as Ast exposing (CompiledFunction, Context(..))
import Compiler.Parser as Parser
import Dict
import Parser.Advanced as Parser


{-| `functions` contains the source code for the functions contained in
elm-logo’s standard library. As far as possible, UCBLogo’s implementation is
copied.

The only thing omitted so far are the calls to `bury` because burying is not yet
implemented in elm-logo.

[combine]: https://github.com/jrincayc/ucblogo-code/blob/master/logolib/combine
[queue]: https://github.com/jrincayc/ucblogo-code/blob/master/logolib/queue
[reverse]: https://github.com/jrincayc/ucblogo-code/blob/master/logolib/reverse

-}
functions : String
functions =
    """to combine :this :those
if wordp :those [output word :this :those]
output fput :this :those
end

to queue :the.queue.name :the.item.value
make :the.queue.name lput :the.item.value thing :the.queue.name
end

to reverse :in [:out ifelse listp :in [[]] ["]]
if emptyp :in [output :out]
output (reverse bf :in combine first :in :out)
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
                |> Result.map (Ast.compileProgram Statement)
    in
    compiledProgram
        |> Result.map .compiledFunctions
        |> Result.withDefault []
