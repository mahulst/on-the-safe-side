module Main exposing (main)

import AnimationFrame
import Html exposing (Html, text)
import Html.Attributes exposing (width, height, style)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Task exposing (Task)
import Time exposing (Time)
import WebGL exposing (Mesh, Shader, Entity)
import WebGL.Texture as Texture exposing (Texture, defaultOptions, Error)


type alias Model =
    { texture : Maybe Texture
    , progress : Float
    }


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , subscriptions = subscriptions
        , update = update
        }


type Msg
    = Tick Time
    | TextureError Error
    | TextureLoaded Texture


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ (AnimationFrame.diffs Tick)
        ]


init : ( Model, Cmd Msg )
init =
    ( { texture = Nothing
      , progress = 0
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
            in
                ( { model | progress = model.progress }, Cmd.none )


calcNewProgress : Float -> Float -> Float
calcNewProgress dt progress =
    let
        newProgress =
            progress + dt / 100

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
view { texture, progress } =
    case texture of
        Just safeTexture ->
            WebGL.toHtml
                [ width 1000
                , height 1000
                , style [ ( "display", "block" ) ]
                ]
                [ toEntity progress faceMesh safeTexture
                , toEntity progress sidesMesh safeTexture
                ]

        Nothing ->
            text "Loading textures..."


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