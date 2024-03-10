module Simulator exposing (..)

import Dict exposing (Dict)


type alias Simulation = 
    { particles : Dict Int Particle     -- all particles in a Dict ordered by their id
    , id : Int                          -- next id to use for particles
    , time : Float                      -- current simulation time
    , width : Float                     -- width, stays fixed during simulation
    , height : Float                    -- height, stays fixed during simulation
    }


-- Particles: each particle has a unique id, a local time, position etc.

type alias Particle =
        { id : Int          -- identifier
        , x: Float          --  x and y position
        , y : Float
        , vx : Float        -- speed in x and y direction
        , vy : Float
        , time : Float      -- local time of particle
        , mass : Float      -- mass of particl
        , radius : Float    -- radius
        , color : Color     -- color for rendering
        }

type alias Pos = (Float, Float)

type alias Vel = (Float, Float)    

type alias Color = String

      
-- The simulator : starts with one particle. 
    
init: Int -> Int -> Simulation
init w h =
    let
        sim = Simulation Dict.empty 0 0 (toFloat w) (toFloat h)
    in
        addParticle (100,200) (10,10) 10 "red" sim


addParticle: Pos -> Vel -> Float -> Color -> Simulation -> Simulation
addParticle pos vel rad col sim = 
    let
        id = sim.id
        p = newParticle id pos vel sim.time rad col
    in
        {sim | id = id+1, particles = Dict.insert id p sim.particles}


particles: Simulation -> (List Particle)
particles sim =
    Dict.values sim.particles

-- Running the simulation: for a given time

run: Simulation -> Float -> Simulation
run sim step  =
    let
        pause = sim.time + step
        updated = Dict.map  (\i -> \p -> moveParticle p pause) sim.particles
    in
        {sim | time = pause,  particles = updated}

    
-- Particles -------------------------------------------------------------------


newParticle: Int -> Pos -> Vel -> Float -> Float -> Color -> Particle
newParticle i (x, y) (vx, vy) t r color =
    { id = i
    , x = x
    , y = y
    , vx = vx
    , vy = vy
    , time = t
    , mass = 1
    , radius = r
    , color = color
    }

-- Moving a particle forward in time.

moveParticle: Particle -> Float -> Particle  
moveParticle prt ti =
    let
        dt = ti - prt.time
        xi = prt.x + (prt.vx * dt)
        yi = prt.y + (prt.vy  * dt)
    in
        {prt | x = xi, y = yi, time = ti}
    
