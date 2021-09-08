module View.AST exposing (view)

-- exposing (Document, Label(..), Section)

import Camperdown.Config.Config as Config
import Camperdown.Loc as Loc exposing (Loc)
import Camperdown.Parse
import Camperdown.Parse.Syntax as Syntax
import Camperdown.Problem as Problem
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Html.Attributes
import List.Extra as List
import RandomColor exposing (RandomColor)



-- PARAMETERS


margins : Int
margins =
    3


panelWidth =
    width (px 500)



--- START: view


view : String -> Syntax.Document -> List (Element.Element msg)
view source { prelude, sections } =
    viewElements source prelude :: List.map (viewSection source) sections


viewSection : String -> Syntax.Section -> Element.Element msg
viewSection source { level, contents, label } =
    let
        title =
            case label of
                Syntax.Named ( _, s ) ->
                    s

                Syntax.Anonymous n ->
                    "(Passage beginning on line " ++ String.fromInt n ++ ")"

        attrs =
            case level of
                1 ->
                    [ Font.size 18, Font.bold ]

                2 ->
                    [ Font.size 17, Font.bold, Font.italic ]

                3 ->
                    [ Font.size 16, Font.bold, Font.italic, Font.color (rgb255 50 50 50) ]

                _ ->
                    [ Font.size 16, Font.bold, Font.italic, Font.color (rgb255 100 100 100) ]
    in
    column [ panelWidth ]
        [ paragraph (sans :: attrs) [ text title ], viewElements source contents ]


viewElements : String -> List Syntax.Element -> Element.Element msg
viewElements source elements =
    column [ panelWidth ] <| List.map (viewElement source) elements


viewDivert : String -> Syntax.Divert -> Element.Element msg
viewDivert source divert =
    case divert of
        Syntax.Nested elems ->
            row [ padding margins, panelWidth ]
                [ el [ Background.color (rgb255 245 245 245), panelWidth, padding margins ] (viewElements source elems)
                ]

        Syntax.Immediate elems ->
            row [ padding margins, panelWidth ]
                [ el [ Background.color (rgb255 245 245 245), panelWidth, padding margins ] (viewElements source elems)
                ]

        Syntax.Reference elems ->
            Element.text "VIEW DIVERT, REFERENCE"


viewElement : String -> Syntax.Element -> Element.Element msg
viewElement source elem =
    case elem of
        Syntax.Paragraph { contents } ->
            row [ panelWidth, paddingEach { top = 3, bottom = 8, left = 0, right = 0 } ]
                [ wrappedRow [ panelWidth, spacingXY 6 3 ]
                    (List.concat <| List.map (viewText RandomColor.third baseStyle) contents)
                ]

        Syntax.Preformatted { contents } ->
            row [ panelWidth ]
                [ el [ monospace, Font.size 15, Element.htmlAttribute (Html.Attributes.style "white-space" "pre") ]
                    (text <| contents)
                ]

        Syntax.Item { children } ->
            row [ panelWidth ]
                [ el [ height fill, width (px 1), Background.color (rgb255 50 50 50) ] none
                , column [ panelWidth, Background.color (rgb255 225 225 225) ]
                    [ el [ height (px 1), panelWidth, Background.color (rgb255 50 50 50) ] none
                    , row [ padding margins, panelWidth ]
                        [ el [ Background.color (rgb255 245 245 245), panelWidth ] (viewElements source children)
                        ]
                    ]
                ]

        Syntax.Command { command, child } ->
            let
                ( mLocStr, config ) =
                    command |> Debug.log "command"

                color =
                    rgb255 200 200 200

                -- TODO: finish this!
                --case command of
                --    (Syntax.Bang, _ ) ->
                --        rgb255 200 200 200
                --
                --    ( Syntax.Huh, _ ) ->
                --        rgb255 225 225 225
            in
            row [ panelWidth ]
                [ el [ height fill, width (px 1), Background.color (rgb255 50 50 50) ] none
                , column [ panelWidth, Background.color color ]
                    [ el [ height (px 1), panelWidth, Background.color (rgb255 50 50 50) ] none
                    , viewCommand command
                    , case child of
                        Nothing ->
                            none

                        Just divert ->
                            viewDivert source divert
                    ]
                ]

        Syntax.Problem { loc, problem, lines } ->
            let
                problemLines =
                    String.split "\n" source
                        |> List.take lines.end
                        |> List.drop (lines.start - 1)

                spacingToRemove =
                    problemLines
                        |> List.map String.toList
                        |> List.map (List.takeWhile (\ch -> ch == ' '))
                        |> List.map List.length
                        |> List.minimumBy identity
                        |> Maybe.withDefault 0

                problemSource =
                    problemLines
                        |> List.map (String.dropLeft spacingToRemove)
                        |> List.intersperse "\n"
                        |> String.concat
            in
            row [ panelWidth, paddingEach { top = 5, bottom = 5, left = 0, right = 0 } ]
                [ el [ height fill, width (px 1), Background.color (dark RandomColor.first) ] none
                , column [ panelWidth, Background.color (light RandomColor.first) ]
                    [ el [ panelWidth, height (px 1), Background.color (dark RandomColor.first) ] none
                    , paragraph [ Font.size 15, sans, paddingXY margins 4 ] [ text <| "Error line " ++ String.fromInt loc.start.line ++ ", position " ++ String.fromInt loc.start.column ]
                    , paragraph [ Font.size 15, sans, paddingXY margins 4 ] [ text <| problem ]
                    , el [ padding margins ]
                        (el [ Background.color (rgb255 255 255 255), Font.size 15, monospace, padding 5, Element.htmlAttribute (Html.Attributes.style "white-space" "pre") ]
                            (text <| problemSource)
                        )
                    ]
                ]


viewCommand : Syntax.Command -> Element.Element msg
viewCommand ( maybeLocString, ( locValues, locParameters ) ) =
    let
        a : List (Loc Syntax.Value)
        a =
            locValues

        p : List (Loc Syntax.Parameter)
        p =
            locParameters

        commandName =
            case maybeLocString of
                Nothing ->
                    ""

                Just c ->
                    Loc.value c
    in
    column [ panelWidth, paddingXY 0 2 ]
        [ wrappedRow [ panelWidth, paddingEach { top = 2, bottom = 2, left = margins, right = margins }, spacingXY 6 0 ]
            (el [ Font.size 15, monospace, Font.bold ] (text <| commandName)
                :: List.map viewValue locValues
            )
        ]


viewParam : Loc Syntax.Parameter -> List (Element.Element msg)
viewParam param =
    let
        ( _, locValues ) =
            Loc.value param
    in
    List.map viewValue locValues


viewValue : Loc Syntax.Value -> Element.Element msg
viewValue val =
    case val of
        ( _, Syntax.Markup markup ) ->
            el [ paddingXY 0 2, panelWidth ] <| wrappedRow [ spacingXY 3 6, Background.color (rgb255 245 245 245), panelWidth, padding 3 ] (List.concat <| List.map (viewText RandomColor.third baseStyle) markup)

        _ ->
            el [ Font.size 15, monospace ] (text <| valueToString val)


valueToString : Loc Syntax.Value -> String
valueToString v =
    case Loc.value v of
        Syntax.Variable s ->
            s

        Syntax.String s ->
            "\"" ++ s ++ "\""

        Syntax.Int i ->
            String.fromInt i

        Syntax.Markup _ ->
            "[(markup)]"


viewText : RandomColor -> Style -> Syntax.Text -> List (Element.Element msg)
viewText color style elem =
    let
        meta : String -> Element msg
        meta str =
            row [ monospace, height fill, Font.size 15, Background.color (light color) ]
                [ el [ height fill, width (px 1), Background.color (dark color) ] none
                , el [ height fill, Font.color (dark color) ] (text str)
                , el [ height fill, width (px 1), Background.color (dark color) ] none
                ]
    in
    case elem of
        Syntax.Raw str ->
            List.map (\word -> el (styleAttributes style) (text word)) (String.split " " str |> List.filter (\s -> String.trim s /= ""))

        Syntax.Verbatim ch ( _, str ) ->
            let
                _ =
                    Debug.log "Verbatim" str
            in
            [ row [ monospace, height fill, Font.size 15, Background.color (light RandomColor.second) ]
                [ el [ height fill, width (px 1), Background.color (dark RandomColor.second) ] none
                , el [ height fill, Font.color (dark RandomColor.second) ] (text <| String.fromChar ch)
                , el [ height fill, paddingEach { top = 0, bottom = 0, left = 4, right = 4 } ] (text str)
                , el [ height fill, Font.color (dark RandomColor.second) ] (text <| String.fromChar ch)
                , el [ height fill, width (px 1), Background.color (dark RandomColor.second) ] none
                ]
            ]

        Syntax.Annotation prefix textList maybeSuffix maybeLocCommand ->
            meta (Loc.value prefix)
                :: (List.concat <|
                        List.map (viewText (RandomColor.next color) style) textList
                   )
                ++ (case maybeLocCommand of
                        Nothing ->
                            [ meta <| Maybe.withDefault "" (Maybe.map Loc.value maybeSuffix) ]

                        Just _ ->
                            [ meta <| Maybe.withDefault "" (Maybe.map Loc.value maybeSuffix) ++ "(..)" ]
                   )

        Syntax.InlineProblem problem ->
            text " " :: viewInlineProblem problem ++ [ text " " ]


viewInlineProblem : Problem.Inline -> List (Element.Element msg)
viewInlineProblem problem =
    [ row [ Font.size 15, Background.color (light RandomColor.first), height fill ]
        (el [ height fill, width (px 1), Background.color (dark RandomColor.first) ] none
            :: el [ height fill, width (px 4), Background.color (light RandomColor.first) ] none
            :: (case problem of
                    _ ->
                        let
                            ( loc, str ) =
                                Problem.inlineToString problem
                        in
                        [ text <| str ++ " (Line " ++ String.fromInt loc.start.line ++ ", position " ++ String.fromInt loc.start.column ++ ".)" ]
               )
            ++ [ el [ height fill, width (px 4), Background.color (light RandomColor.first) ] none
               , el [ height fill, width (px 1), Background.color (dark RandomColor.first) ] none
               ]
        )
    ]



--- HELPERS


monospace : Attribute msg
monospace =
    Font.family [ Font.typeface "Source Code Pro", Font.monospace ]


sans : Attribute msg
sans =
    Font.family [ Font.typeface "Soleil", Font.typeface "Arial", Font.sansSerif ]


dark : RandomColor -> Color
dark c =
    fromRgb (RandomColor.toRGB 0.99 0.6 c)


light : RandomColor -> Color
light c =
    fromRgb (RandomColor.toRGB 0.1 0.99 c)


valuesToString : List (Loc Syntax.Value) -> String
valuesToString vs =
    List.map valueToString vs
        |> List.intersperse " "
        |> String.concat


type alias Style =
    { bold : Bool
    , strike : Bool
    , italic : Bool
    , under : Bool
    , code : Bool
    }


baseStyle : Style
baseStyle =
    { bold = False
    , strike = False
    , italic = False
    , under = False
    , code = False
    }


styleAttributes : Style -> List (Element.Attribute msg)
styleAttributes { bold, strike, italic, under } =
    (if bold then
        [ Font.bold ]

     else
        []
    )
        ++ (if strike then
                [ Font.strike ]

            else
                []
           )
        ++ (if italic then
                [ Font.italic ]

            else
                []
           )
        ++ (if under then
                [ Font.underline ]

            else
                []
           )
        ++ [ sans, Font.size 15 ]
