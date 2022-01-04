module ModuleName exposing (..)

import Elm.Syntax.ModuleName exposing (ModuleName)


toString : ModuleName -> String
toString =
    String.join "."


qualifiedToString : ( ModuleName, String ) -> String
qualifiedToString ( moduleName, name ) =
    moduleName ++ [ name ] |> String.join "."
