module Main exposing (..)

import Simulator exposing (Simulation)

import Html
import Html.Events 
import Html.Attributes

import Browser 
import Browser.Events 

import Svg 
import Svg.Attributes 
import Svg.Events 

import Time 


-- Main 

main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- Model

type alias Model = 
    { run : Bool
    , ticks : Int
    , simulation : Simulation
    , width : Int
    , height : Int
    , leap : Float
    }

init : () -> (Model, Cmd Msg)
init _ =
    let
        w = 500
        h = 500
        sim = Simulator.init w h
    in
       ({ run = True
        , ticks = 0
        , simulation = sim
        , width = w
        , height = h
        , leap = 0.1
        }
        , Cmd.none)  

-- Messages 

type Msg =
    Tick Time.Posix
        | Click

-- Subscriptions 

subscriptions: Model -> Sub Msg
subscriptions model =
    if model.run then
        Browser.Events.onAnimationFrame Tick
    else
        Sub.none

-- Update            

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Tick _ -> 
            let 
                sim = Simulator.run model.simulation model.leap
            in
                ({model | ticks = model.ticks+1, simulation = sim}, Cmd.none)
        Click ->
            let 
                sim = Simulator.addParticle (100,200) (10,8) 10 "red" model.simulation 
            in
                ({model | simulation = sim}, Cmd.none)

-- View
    
view : Model -> Html.Html Msg
view model  =
    let
        particles = Simulator.particles model.simulation
    in
        Html.div []
            [ Html.h1 
                  [ Html.Attributes.style "text-align" "center"]
                  [ Html.text ("The Particle Simulator " ++ (String.fromInt model.ticks)) ]
            , Html.div 
                  [ Html.Attributes.style "top" "50%"
                  , Html.Attributes.style "left" "50%"
                  , Html.Attributes.style "width" "80%"
                  , Html.Attributes.style "margin" "auto"
                  , Html.Attributes.style "border-style" "solid"
                  , Html.Attributes.style "height" ((String.fromInt model.height) ++ "px")
                  , Html.Attributes.style "width" ((String.fromInt  model.width) ++ "px")
                  ]
                  [ Svg.svg
                        [ Svg.Attributes.width (String.fromInt model.width)
                        , Svg.Attributes.height (String.fromInt model.height)
                        , Svg.Events.onClick Click
                        ]
                        ( (List.map (\prt ->
                                         Svg.circle
                                         [ Svg.Attributes.cx (String.fromFloat prt.x)
                                         , Svg.Attributes.cy (String.fromFloat ( toFloat(model.height) - prt.y))
                                         , Svg.Attributes.r  (String.fromFloat prt.radius)
                                         , Svg.Attributes.fill prt.color
                                         ] []
                                    )
                               particles)
                        )
                  ]
            ]
            
