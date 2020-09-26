module Ui.Theme exposing
    ( backgroundColor
    , buttonStyle
    , color
    , globalStyle
    , monospaceFont
    , proportionalFont
    )

import Css
    exposing
        ( batch
        , borderBox
        , borderRadius
        , borderStyle
        , boxSizing
        , cursor
        , fontFamilies
        , fontSize
        , height
        , hex
        , hover
        , margin
        , none
        , padding2
        , pointer
        , px
        , rgb
        , vh
        , vw
        , width
        , zero
        )
import Css.Global exposing (body, everything, global)
import Css.Transitions as T exposing (transition)
import Html.Styled exposing (Html)


globalStyle : Html msg
globalStyle =
    global
        [ everything [ boxSizing borderBox ]
        , body
            [ width (vw 100)
            , height (vh 100)
            , margin zero
            ]
        ]


proportionalFont : Css.Style
proportionalFont =
    fontFamilies [ "Raleway", "-apple-system", "system", "sans-serif" ]


monospaceFont : Css.Style
monospaceFont =
    batch
        [ fontFamilies [ "Fira Code", "monospace" ]
        , fontSize (px 12)
        ]


color : Css.Color
color =
    rgb 248 248 242


backgroundColor : Css.Color
backgroundColor =
    rgb 37 36 36


buttonStyle : Css.Style
buttonStyle =
    batch
        [ Css.color (rgb 37 36 36)
        , Css.backgroundColor (hex "cfcfcf")
        , borderRadius (px 5)
        , borderStyle none
        , padding2 (px 5) (px 20)
        , cursor pointer
        , hover
            [ Css.backgroundColor (hex "efefef")
            ]
        , transition
            [ T.backgroundColor3 200 0 T.easeIn
            ]
        ]
