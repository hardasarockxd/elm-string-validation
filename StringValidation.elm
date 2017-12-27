module StringValidation exposing (..)

-- STRING VALIDATION
-- "stringValidation" function
--  *FIRST ARGUENT SHOULD BE YOUR STRING
--  *SECOND ARGUMENT SHOULD BE LIST OF SYMBOLS WHICH NEED TO BE IN STRING
--  *THIRD ARGUMENT SHOULD BE MIN LENGTH OF STRING
--  *FOURTH ARGUMENT SHOULD BE MAX LENGTH OF STRING
-- EXAMPLE:
-- InputPassword password ->
--     { model | passwordResult = StringValidation.stringValidation password [ '@', '1', '2', '3' ] 2 10 }


stringValidation : String -> List Char -> Int -> Int -> ( String, Bool )
stringValidation string listOfSymbols startPoint endPoint =
    let
        stringLength =
            String.length string

        charValidation =
            List.map
                (\char ->
                    let
                        newChar =
                            toString char
                                |> String.dropLeft 1
                                |> String.dropRight 1
                    in
                    String.contains newChar string
                )
                listOfSymbols

        listBoolValidation =
            List.filter
                (\bool -> bool)
                charValidation
    in
    if stringLength >= startPoint && stringLength <= endPoint && List.length charValidation == List.length listBoolValidation then
        ( string, True )
    else
        ( string, False )



-- "stringValidationToString" function
--  *FIRST ARGUMENT SHOULD BE RESULT OF "stringValidation"
-- EXAMPLE:
-- InputPassword password ->
--     { model | passwordResult = StringValidation.stringValidationToString <| StringValidation.stringValidation password [ '@', '1', '2', '3' ] 2 10 }


stringValidationToString : ( String, Bool ) -> String
stringValidationToString ( string, bool ) =
    if Tuple.second ( string, bool ) then
        Tuple.first ( string, bool )
    else
        "VALIDATION ERROR"
