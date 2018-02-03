module Main exposing (main, updateCompleted)

import AnimationFrame
import Html exposing (Html, text)
import Html.Attributes exposing (width, height, style)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Mouse
import Task exposing (Task)
import Time exposing (Time)
import WebGL exposing (Mesh, Shader, Entity)
import WebGL.Texture as Texture exposing (Texture, defaultOptions, Error)
import List.Extra


type alias Model =
    { texture : Maybe Texture
    , progress : Float
    , spindleRotation : Int
    , previousMousePos : Maybe Mouse.Position
    , requestedCode : List Int
    , codesCompleted : Int
    , gameState : GameState
    }


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , subscriptions = subscriptions
        , update = update
        }


type GameState
    = Playing
    | Won
    | Lost


type Msg
    = Tick Time
    | MouseDown Mouse.Position
    | MouseUp Mouse.Position
    | MouseMove Mouse.Position
    | TextureError Error
    | TextureLoaded Texture


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ AnimationFrame.diffs Tick
        , Mouse.downs MouseDown
        , Mouse.ups MouseUp
        , Mouse.moves MouseMove
        ]


init : ( Model, Cmd Msg )
init =
    ( { texture = Nothing
      , progress = 0
      , spindleRotation = 0
      , previousMousePos = Nothing
      , requestedCode = [ 38, 12, 58, 45 ]
      , codesCompleted = 0
      , gameState = Playing
      }
    , Cmd.batch
        [ fetchTexture
        ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        TextureError err ->
            ( model, Cmd.none )

        TextureLoaded texture ->
            ( { model | texture = Just texture }, Cmd.none )

        Tick dt ->
            let
                progress =
                    calcNewProgress dt model.progress

                gameState =
                    if progress == 0 then
                        Lost
                    else
                        model.gameState
            in
                ( { model
                    | progress =
                        if model.gameState == Playing then
                            progress
                        else
                            model.progress
                    , gameState = gameState
                  }
                , Cmd.none
                )

        MouseDown pos ->
            ( { model | previousMousePos = Just pos }, Cmd.none )

        MouseUp _ ->
            ( { model
                | previousMousePos = Nothing
                , gameState =
                    if model.codesCompleted == List.length model.requestedCode then
                        Won
                    else
                        Playing
              }
            , Cmd.none
            )

        MouseMove pos ->
            let
                newRotation =
                    Maybe.map
                        (\previousMousePos ->
                            calcNewSpindleRotation pos.x previousMousePos.x model.spindleRotation
                        )
                        model.previousMousePos
                        |> Maybe.withDefault model.spindleRotation

                newCompleted =
                    updateCompleted model.requestedCode newRotation model.spindleRotation model.codesCompleted
            in
                ( { model
                    | spindleRotation = newRotation
                    , previousMousePos =
                        Maybe.map
                            (\previousPos ->
                                if model.spindleRotation /= newRotation then
                                    pos
                                else
                                    previousPos
                            )
                            model.previousMousePos
                    , codesCompleted = newCompleted
                  }
                , Cmd.none
                )


updateCompleted : List Int -> Int -> Int -> Int -> Int
updateCompleted list rotation previousRotation completed =
    let
        currentNeeded : Int
        currentNeeded =
            List.Extra.getAt completed list
                |> Maybe.withDefault
                    99

        previousNeeded : Int
        previousNeeded =
            List.Extra.getAt (completed - 1) list
                |> Maybe.withDefault
                    99
    in
        case (currentNeeded == rotation) of
            True ->
                completed + 1

            False ->
                case rem completed 2 == 0 of
                    True ->
                        case rotation >= 0 && rotation < currentNeeded of
                            True ->
                                if rotation < previousRotation then
                                    0
                                else
                                    completed

                            False ->
                                if rotation > previousRotation then
                                    0
                                else
                                    completed

                    False ->
                        case rotation >= 0 && rotation < currentNeeded of
                            True ->
                                if rotation < previousRotation then
                                    0
                                else
                                    completed

                            False ->
                                if rotation > previousRotation then
                                    0
                                else
                                    completed


calcNewSpindleRotation : Int -> Int -> Int -> Int
calcNewSpindleRotation previousPos currentPos rotation =
    let
        newRotation =
            rotation + ((previousPos - currentPos) // 10)

        resetIfNeeded =
            if rotation > 60 then
                newRotation - 60
            else if newRotation < 0 then
                newRotation + 60
            else
                newRotation
    in
        resetIfNeeded


calcNewProgress : Float -> Float -> Float
calcNewProgress dt progress =
    let
        newProgress =
            progress + dt / 400

        resetIfNeeded =
            if newProgress > 40 then
                0
            else
                newProgress
    in
        resetIfNeeded


fetchTexture : Cmd Msg
fetchTexture =
    "texture/safe-side.jpg"
        |> Texture.loadWith
            { defaultOptions
                | magnify = Texture.nearest
                , minify = Texture.nearest
            }
        |> Task.attempt
            (\result ->
                case result of
                    Err error ->
                        TextureError error

                    Ok texture ->
                        TextureLoaded texture
            )



-- Meshes


type alias Vertex =
    { position : Vec3
    , coord : Vec2
    }


faceMesh : Mesh Vertex
faceMesh =
    WebGL.triangles square


sidesMesh : Mesh Vertex
sidesMesh =
    [ ( 90, 0 ), ( 180, 0 ), ( 270, 0 ), ( 0, 90 ), ( 0, 270 ) ]
        |> List.concatMap rotatedSquare
        |> WebGL.triangles


rotatedSquare : ( Float, Float ) -> List ( Vertex, Vertex, Vertex )
rotatedSquare ( angleXZ, angleYZ ) =
    let
        transformMat =
            Mat4.mul
                (Mat4.makeRotate (degrees angleXZ) Vec3.j)
                (Mat4.makeRotate (degrees angleYZ) Vec3.i)

        transform vertex =
            { vertex
                | position =
                    Mat4.transform transformMat vertex.position
            }

        transformTriangle ( a, b, c ) =
            ( transform a, transform b, transform c )
    in
        List.map transformTriangle square


square : List ( Vertex, Vertex, Vertex )
square =
    let
        topLeft =
            Vertex (vec3 -1 1 1) (vec2 0 1)

        topRight =
            Vertex (vec3 1 1 1) (vec2 1 1)

        bottomLeft =
            Vertex (vec3 -1 -1 1) (vec2 0 0)

        bottomRight =
            Vertex (vec3 1 -1 1) (vec2 1 0)
    in
        [ ( topLeft, topRight, bottomLeft )
        , ( bottomLeft, topRight, bottomRight )
        ]



-- VIEW


view : Model -> Html Msg
view model =
    case model.texture of
        Just safeTexture ->
            Html.div []
                [ WebGL.toHtml
                    [ width 1000
                    , height 1000
                    , style [ ( "display", "block" ) ]
                    ]
                    [ toEntity model.progress faceMesh safeTexture
                    , toEntity model.progress sidesMesh safeTexture
                    , spindleEntity model.spindleRotation
                    ]
                , Html.div [ Html.Attributes.class "spindleValue" ] [ text (toString model.spindleRotation) ]
                , viewRequestedCode model.requestedCode model.codesCompleted
                ]

        Nothing ->
            text "Loading textures..."


viewRequestedCode : List Int -> Int -> Html Msg
viewRequestedCode list completed =
    Html.div
        []
        [ Html.div
            [ Html.Attributes.class "requestedCode" ]
            ([ text "Safe code: " ]
                ++ (List.indexedMap
                        (\index code ->
                            Html.div
                                [ Html.Attributes.classList
                                    [ ( "done", index < completed )
                                    ]
                                ]
                                [ text (toString code) ]
                        )
                        list
                   )
            )
        ]


toEntity : Float -> Mesh Vertex -> Texture -> Entity
toEntity progress mesh texture =
    WebGL.entity
        vertexShader
        fragmentShader
        mesh
        { texture = texture
        , perspective = perspective
        , initialPos = vec3 0 38 0
        , progress = progress
        }


perspective : Mat4
perspective =
    let
        eye =
            vec3 0 -8 3
    in
        Mat4.mul
            (Mat4.makePerspective 45 1 0.01 100)
            (Mat4.makeLookAt eye (vec3 0 0 0) Vec3.j)



-- SHADERS


type alias Uniforms =
    { perspective : Mat4
    , texture : Texture
    , initialPos : Vec3
    , progress : Float
    }


vertexShader : Shader Vertex Uniforms { vcoord : Vec2 }
vertexShader =
    [glsl|

        attribute vec3 position;
        attribute vec2 coord;
        uniform mat4 perspective;
        uniform vec3 initialPos;
        uniform float progress;
        varying vec2 vcoord;

        void main () {
          gl_Position = perspective * vec4(position + initialPos - vec3(0, progress, 0) , 1.0);
          vcoord = coord.xy;
        }

    |]


fragmentShader : Shader {} Uniforms { vcoord : Vec2 }
fragmentShader =
    [glsl|

        precision mediump float;
        uniform sampler2D texture;
        varying vec2 vcoord;

        void main () {
          gl_FragColor = texture2D(texture, vcoord);
        }

    |]


spindleEntity : Int -> Entity
spindleEntity rotation =
    WebGL.entity
        spindleVertexShader
        spindleFragmentShader
        spindleMesh
        { perspective = spindlePerspective
        , rotation = Mat4.makeRotate ((toFloat rotation) / 10) (vec3 0 0 1)
        , offset =
            vec3 0 -1 0
        }


spindlePerspective : Mat4
spindlePerspective =
    Mat4.mul
        (Mat4.makePerspective 45 1 0.01 100)
        (Mat4.makeLookAt (vec3 0 0 1) (vec3 0 0 0) (vec3 0 1 0))


type alias SpindleVertex =
    { position : Vec3
    }


spindleMesh : Mesh SpindleVertex
spindleMesh =
    let
        scale =
            (Vec3.scale 0.2)
    in
        [ ( (vec3 -1 -1 0)
          , (vec3 1 -1 0)
          , (vec3 -1 1 0)
          )
        , ( (vec3 1 1 0)
          , (vec3 1 -1 0)
          , (vec3 -1 1 0)
          )
        ]
            |> List.map (\( a, b, c ) -> ( scale a, scale b, scale c ))
            |> List.map (\( a, b, c ) -> ( SpindleVertex a, SpindleVertex b, SpindleVertex c ))
            |> WebGL.triangles


type alias SpindleUniforms =
    { perspective : Mat4
    , offset : Vec3
    , rotation : Mat4
    }


spindleVertexShader : Shader SpindleVertex SpindleUniforms {}
spindleVertexShader =
    [glsl|
        attribute vec3 position;
        uniform vec3 offset;
        uniform mat4 rotation;
        uniform mat4 perspective;
        void main () {
            gl_Position = perspective * vec4(position, 1.0) * rotation + vec4(offset, 1.0) ;
        }
    |]


spindleFragmentShader : Shader {} SpindleUniforms {}
spindleFragmentShader =
    [glsl|
        precision mediump float;
        void main () {
            gl_FragColor = vec4(1, 0.5, 0.5, 1.0);
        }
    |]
