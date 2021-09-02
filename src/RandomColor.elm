module RandomColor exposing (RandomColor, first, next, second, third, toRGB)


type RandomColor
    = Color Float


goldenRationConjugate : Float
goldenRationConjugate =
    0.618033988


next : RandomColor -> RandomColor
next (Color i) =
    let
        j =
            i + goldenRationConjugate
    in
    if j > 1.0 then
        Color (j - 1.0)

    else
        Color j


first : RandomColor
first =
    Color 0


second : RandomColor
second =
    next first


third : RandomColor
third =
    next second


toRGB : Float -> Float -> RandomColor -> { red : Float, green : Float, blue : Float, alpha : Float }
toRGB saturation value (Color hue) =
    let
        c =
            value * saturation

        m =
            value - c

        lazyabs i =
            if i < 0 then
                0 - i

            else
                i

        lazymod2 i =
            if i > 2.0 then
                lazymod2 (lazyabs (i - 2.0))

            else
                i

        x =
            c * (1.0 - lazyabs (lazymod2 (hue * 6.0) - 1.0))
    in
    if hue < (1.0 / 6.0) then
        { red = c + m, green = x + m, blue = m, alpha = 1.0 }

    else if hue < (2.0 / 6.0) then
        { red = x + m, green = c + m, blue = m, alpha = 1.0 }

    else if hue < (3.0 / 6.0) then
        { red = m, green = c + m, blue = x + m, alpha = 1.0 }

    else if hue < (4.0 / 6.0) then
        { red = m, green = x + m, blue = c + m, alpha = 1.0 }

    else if hue < (5.0 / 6.0) then
        { red = x + m, green = m, blue = c + m, alpha = 1.0 }

    else
        { red = c + m, green = m, blue = x + m, alpha = 1.0 }
