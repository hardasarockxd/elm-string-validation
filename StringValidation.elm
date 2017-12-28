module Helpers.StringValidation exposing (..)

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



-- "nputValidation" function
--  *FIRST ARGUMENT SHOULD BE NAME OF INPUT
--  *SECOND ARGUMENT SHOULD BE STRING TO VALIDATE
--  *THIRD ARGUMENT SHOULD BE  LIST OF SYMBOLS WHICH MUST BE IN STRING
--  *FOURTH ARGUMENT SHOULD BE LIST OF SYMBOLS WHICH MUST NOT BE IN STRING
--  *FIFTH  ARGUMENT SHOULD BE BOOLEAN(Should input contain any numbers?)
--  *SIXTH ARGUMENT SHOULD BE MIN LENGTH OF STRING
--  *SEVENTH ARGUMENT SHOULD BE MAX LENGTH OF STRING


inputValidation : String -> String -> List Char -> List Char -> Bool -> Int -> Int -> ( String, Bool, String )
inputValidation inputName string listOfSymbols listOfBanSymbols isWithNumbers startPoint endPoint =
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

        charBanValidation =
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
                listOfBanSymbols

        listBoolBanValidation =
            List.filter
                (\bool -> not bool)
                charBanValidation

        numbers =
            [ "1", "2", "3", "4", "5", "6", "7", "8", "9", "0" ]

        numbersCheck =
            if isWithNumbers then
                List.map
                    (\number ->
                        String.contains number string
                    )
                    numbers
                    |> List.filter
                        (\numbersListItem ->
                            if numbersListItem == True then
                                True
                            else
                                False
                        )
            else
                []

        resultOfNumbersCheck =
            if isWithNumbers then
                if List.length numbersCheck > 0 then
                    "T"
                else
                    "F"
            else
                "T"
    in
    if stringLength <= startPoint then
        ( string, False, "Text should be no less than " ++ toString startPoint ++ " symbols " )
    else if stringLength >= endPoint then
        ( string, False, "Text should be no bigger than " ++ toString endPoint ++ " symbols " )
    else if List.length charValidation /= List.length listBoolValidation then
        let
            listOfAllSymbols =
                List.map
                    (\char ->
                        toString char
                            |> String.dropLeft 1
                            |> String.dropRight 1
                    )
                    listOfSymbols

            stringWithSymbols =
                String.join ", " listOfAllSymbols
        in
        ( string, False, "Text should include " ++ stringWithSymbols )
    else if List.length charBanValidation /= List.length listBoolBanValidation then
        if String.length inputName == 0 then
            let
                listOfAllBanSymbols =
                    List.map
                        (\char ->
                            toString char
                                |> String.dropLeft 1
                                |> String.dropRight 1
                        )
                        listOfBanSymbols

                stringWithBanSymbols =
                    String.join ", " listOfAllBanSymbols
            in
            ( string, False, "Text should not contain:" ++ stringWithBanSymbols )
        else
            ( string, False, "Please, type your real " ++ inputName )
    else if resultOfNumbersCheck == "F" then
        ( string, False, "Should contain min 1 number" )
    else
        ( string, True, "" )

