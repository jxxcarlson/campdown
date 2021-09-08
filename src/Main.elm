module Main exposing (main)

{- This is a starter app which presents a text label, text field, and a button.
   What you enter in the text field is echoed in the label.  When you press the
   button, the text in the label is reverse.
   This version uses `mdgriffith/elm-ui` for the view functions.
-}

import ASTTools
import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav exposing (Key)
import Camperdown.Config.Config as Config
import Camperdown.Parse
import Camperdown.Parse.Syntax exposing (Document, Label(..), Section)
import Docs
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import File exposing (File)
import File.Select as Select
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode as Decode
import Json.Encode as Encode
import Markdown
import Task
import Url exposing (Url)
import View.AST
import View.Campdown


main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        }


type alias Model =
    { contents : String
    , document : Maybe Document
    , key : Key
    , url : Url
    , viewMode : ViewMode
    }


type Msg
    = NoOp
    | CodeChanged String
    | UrlClicked UrlRequest
    | UrlChanged Url
    | InputText String
    | ShowAST
    | ShowCampdown
    | About
    | Dummy String
    | FileRequested
    | FileSelected File
    | LoadFileContents String


type ViewMode
    = ViewCampdown
    | ViewAST
    | ViewAbout


type alias Flags =
    {}


init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        doc =
            Just (Camperdown.Parse.parse Config.config sourceText |> Debug.log "AST")
    in
    ( { key = key
      , url = url
      , contents = sourceText
      , document = doc
      , viewMode = ViewAST
      }
    , Cmd.none
    )


subscriptions model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        CodeChanged str ->
            ( loadContent model str, Cmd.none )

        InputText str ->
            ( { model
                | contents = str

                --, viewMode = ViewCampdown
                , document = Just (Camperdown.Parse.parse Config.config str)
              }
            , Cmd.none
            )

        UrlClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model, Cmd.none )

                External url ->
                    ( model
                    , Nav.load url
                    )

        UrlChanged _ ->
            ( model, Cmd.none )

        ShowAST ->
            ( { model | viewMode = ViewCampdown }, Cmd.none )

        ShowCampdown ->
            ( { model | viewMode = ViewAST }, Cmd.none )

        About ->
            ( { model | viewMode = ViewAbout }, Cmd.none )

        Dummy _ ->
            ( { model | viewMode = ViewAST }, Cmd.none )

        FileRequested ->
            ( model, Select.file [ "text/txt" ] FileSelected )

        FileSelected file ->
            ( model, Task.perform LoadFileContents (File.toString file) )

        LoadFileContents contents ->
            ( { model | contents = contents, document = Just (Camperdown.Parse.parse Config.config contents) }, Cmd.none )



-- HELPERS


requestHyperCard : Cmd Msg
requestHyperCard =
    Select.file [ "text/plain" ] FileSelected



--
-- VIEW
--


fontGray g =
    Font.color (Element.rgb g g g)


bgGray g =
    Background.color (Element.rgb g g g)


view : Model -> Browser.Document Msg
view model =
    { title = "Campdown Demo"
    , body = [ Element.layout [ bgGray 0.2, centerX, centerY ] (mainColumn model) ]
    }


mainColumn : Model -> Element Msg
mainColumn model =
    column mainColumnStyle
        [ column [ spacing 36, width (px 1200), height (px 800), scrollbarY ]
            [ row [ spacing 12, centerX, centerY ]
                [ el [ Font.size 24 ] (text "Campdown Demo")
                , toggleViewButton model.viewMode
                , requestFileButton
                , aboutButton model.viewMode
                ]
            , row [ spacing 12 ] [ editor model, viewDocument model ]
            ]
        ]


viewDocument model =
    case model.viewMode of
        ViewAST ->
            viewAST model

        ViewCampdown ->
            viewCampDown model

        ViewAbout ->
            text "Not implemented"


viewCampDown : Model -> Element msg
viewCampDown model =
    case model.document of
        Nothing ->
            Element.none

        Just doc ->
            column [ height (px 700), width (px 500), scrollbarY ]
                (View.Campdown.view ourFormat model.contents doc)


ourFormat =
    -- Units = pixels
    { imageHeight = 300
    , lineWidth = 500
    , leftPadding = 15
    , bottomPadding = 8
    , topPadding = 8
    }



-- |> column [ height (px 700), width (px 500), scrollbarY ]


viewAST : Model -> Element msg
viewAST model =
    case model.document of
        Nothing ->
            Element.none

        Just doc ->
            View.AST.view model.contents doc |> column [ height (px 700), width (px 500), scrollbarY ]


panelWidth model =
    width (px <| min (model.width // 2 - 100) maxPanelWidth)


maxPanelWidth =
    700


headerHeight =
    40


verticalSpreaderHeight =
    24



--
--panelHeight model =
--    height (px (model.height - headerHeight - verticalSpreaderHeight - 30))
--
--
--editor2 model =
--    el [ panelWidth model, panelHeight model ]
--        (html <|
--            Html.node "custom-editor"
--                [ Html.Attributes.property "editorContents" <|
--                    Encode.string model.contents
--                , Html.Events.on "editorChanged" <|
--                    Decode.map CodeChanged <|
--                        Decode.at [ "target", "editorContents" ] <|
--                            Decode.string
--                ]
--                []
--        )


loadContent : Model -> String -> Model
loadContent model text =
    let
        document =
            documentFromString text
    in
    { model
        | contents = text
        , document = Just document
    }


documentFromString str =
    Camperdown.Parse.parse Config.config str


editor model =
    -- TODO: finish this!
    -- column [ width fill, height fill, Background.color (rgb255 255 212 220), padding 40 ] [ text "EDITOR" ]
    Input.multiline [ width (px 500), height (px 700), Background.color (rgb255 255 212 220), padding 40, Font.size 14 ]
        { onChange = InputText
        , text = model.contents
        , placeholder = Nothing
        , label = Input.labelHidden "Editor"
        , spellcheck = False
        }


format =
    { imageHeight = 500
    , lineWidth = 600
    , leftPadding = 20
    , bottomPadding = 10
    , topPadding = 10
    }


title : String -> Element msg
title str =
    row [ centerX, Font.bold, fontGray 0.9 ] [ text str ]


inputText : Model -> Element Msg
inputText model =
    Input.text []
        { onChange = InputText
        , text = model.contents
        , placeholder = Nothing
        , label = Input.labelAbove [ fontGray 0.9 ] <| el [] (text "Input")
        }


toggleViewButton : ViewMode -> Element Msg
toggleViewButton viewMode =
    case viewMode of
        ViewAST ->
            row []
                [ Input.button buttonStyle
                    { onPress = Just ShowAST
                    , label = el [ centerX, centerY ] (text "AST    ")
                    }
                ]

        ViewCampdown ->
            row []
                [ Input.button buttonStyle
                    { onPress = Just ShowCampdown
                    , label = el [ centerX, centerY ] (text "Campdown")
                    }
                ]

        ViewAbout ->
            Element.none


requestFileButton : Element Msg
requestFileButton =
    row []
        [ Input.button buttonStyle
            { onPress = Just FileRequested
            , label = el [ centerX, centerY ] (text "Open file")
            }
        ]


aboutButton : ViewMode -> Element Msg
aboutButton viewMode =
    case viewMode of
        ViewAbout ->
            row []
                [ Input.button buttonStyle
                    { onPress = Just ShowCampdown
                    , label = el [ centerX, centerY ] (text "Campdown")
                    }
                ]

        _ ->
            row []
                [ Input.button buttonStyle
                    { onPress = Just About
                    , label = el [ centerX, centerY ] (text "About")
                    }
                ]



--
-- STYLE
--


mainColumnStyle =
    [ centerX
    , centerY
    , bgGray 1.0
    , paddingXY 20 0
    , width (px 1400)
    , height (px 800)
    ]


buttonStyle =
    [ Background.color (Element.rgb 0.5 0.5 0.5)
    , Font.color (rgb255 255 255 255)
    , Font.size 14
    , paddingXY 15 8
    ]



-- DATA


sourceText9 =
    """
Math: $\\LaTeX$"""


sourceText6 =
    """
The text of a lesson is pretty simple. You can use **bold** and _italic_ text. You can add inline math using $\\LaTeX$.
"""


sourceText7 =
    """
%%% You can add comments if you need them.
    Comments can span multiple lines if necessary. Just keep indenting
"""


sourceText8 =
    """
You can also add preformatted text blocks (i.e. code blocks)
using $```$, and you can add multi-line math with `$$$`.
They look like this:
"""


sourceText =
    """
! heading1  [Introduction]
"""


sourceText10 =
    """
This is **BOLD.**

This is _italic._

[NYT](link "http://nytimes.com")
"""


sourceText4 =
    """
You can use **bold**.
"""


sourceText3 =
    """
!heading1 [writing text]

The text of a lesson is pretty simple.
You can use **bold** and _italic_ text.
You can add inline math using $\\LaTeX$.
"""


sourceText1 =
    """go to [Fraance](link "france-entry")."""


sourceText2 =
    """

# The beginning

Go around Europe!

! image "https://europa.eu/european-union/sites/default/files/easy_to_read/european-map_en.jpg"

You start in Dover, and hop aboard the Chunnel
to go to [France](link "france-entry").

# france-entry

Welcome to France!

! image "https://upload.wikimedia.org/wikipedia/en/c/c3/Flag_of_France.svg"

From here you can
[eat a baguette](link "france-munch"), or you can get on
the train to [Italy](link "italy-entry" ), or you can
take a hot air balloon to [Germany](link "germany-hot-air").



# germany-hot-air

! image "https://artprojectsforkids.org/wp-content/uploads/2018/02/Hot-Air-Balloon-1.jpg"

You are entering Germany by hot air balloon!

Your only option now is to go to [Italy](link "italy-entry").

# france-munch

Mmm tasty baguette.

! image "https://images.food52.com/svbmUpu4t64ynw5tCXQ0b1aKW6I=/71f851b1-f927-4fd5-a0c9-c418e74f428c--13408906735_82b0d0499e_b.jpg"

From here you can get on
the train to [Italy](link "italy-entry" ), or you can
take a hot air balloon to [Germany](link "germany-hot-air").

# italy-entry

Welcome to Italy!

! image "https://foodwineclick.files.wordpress.com/2016/09/winestudio_amatriciana_umbria-20160830-65.jpg?w=640"

The wine, pasta, and conversation were too good ...  [time for a nap](link "naptime").

# naptime

! image "https://i1.wp.com/katzenworld.co.uk/wp-content/uploads/2020/03/close-up-photography-of-gray-tabby-cat-sleeping-on-yellow-1440918.jpg?fit=1020%2C680&ssl=1"

Italy is a good place to take a nap. We hope
you had a pleasant journey.
"""
