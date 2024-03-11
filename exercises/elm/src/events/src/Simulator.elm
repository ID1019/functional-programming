module Simulator exposing (..)

import Dict exposing (Dict)

import PriorityQueue as PQ exposing (PriorityQueue)

type alias Simulation = 
    { particles : Dict Int Particle     -- all particles in a Dict ordered by their id
    , queue : PriorityQueue Event       -- priority queue of all future events
    , id : Int                          -- next id to use for particles
    , time : Float                      -- current simulation time
    , width : Float                     -- width, stays fixed during simulation
    , height : Float                    -- height, stays fixed during simulation
    }

-- Three types of events: collision between two particles, particle hitting wall and pause of simulation.    

type Event
    = WALL Wall Int Float
    | PAUSE Float

type Wall       
    = FLOOR
    | ROOF 
    | LEFT 
    | RIGHT

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
    Simulation Dict.empty (PQ.new lessEvent) 0 0 (toFloat w) (toFloat h)

addParticle: Pos -> Vel -> Float -> Color -> Simulation -> Simulation
addParticle pos vel rad col sim = 
    let
        id = sim.id
        p = newParticle id pos vel sim.time rad col
    in
        {sim | id = id+1
             , particles = Dict.insert id p sim.particles
             , queue = predictPart p sim.particles sim.width sim.height sim.queue}

particles: Simulation -> (List Particle)
particles sim =
    Dict.values sim.particles

-- Running the simulation: for a given time, ie.e insert a pause event and go.

run: Simulation -> Float -> Simulation
run sim hold  =
    let
        queue = PQ.insert (pauseEvent (sim.time + hold)) sim.queue
        (prts, que, tm) = step sim.particles queue sim.time sim.width sim.height
    in
        {sim | time = tm, particles = prts, queue = que}

-- Each step will take an event from the queue and update the particle,
-- if asked to pause all particles are moved forward in time.            
            
step: (Dict Int Particle) -> PriorityQueue Event -> Float -> Float ->  Float -> (Dict Int Particle, PriorityQueue Event, Float)
step parts queue tm width height =
    case PQ.next queue of
        Nothing ->
            (parts, queue, tm)
        Just (evt, tail) ->
            case evt of
                WALL wall id time ->
                    let 
                        (updParts, updQueue) = case (Dict.get id parts) of
                                                   Nothing ->
                                                       (parts, tail)
                                                   Just prt ->
                                                       let 
                                                           upd = collWall wall prt time 
                                                           updP  = Dict.insert upd.id upd (Dict.remove id parts)
                                                           updQ = predictPart upd  width height tail
                                                       in
                                                           (updP, updQ)
                    in
                        step updParts updQueue time width height
                PAUSE time ->
                    let
                        updated = Dict.map  (\i -> \p -> moveParticle p time) parts
                    in
                        (updated, tail, time)

-- Predicting future collisions with walls, roof and floor.

predictPart: Particle -> Float -> Float -> PriorityQueue Event -> PriorityQueue Event
predictPart prt width height queue =
    let
        closest = closestWall prt width height 
    in
        case closest of
            Nothing ->
                queue
            Just (bw, bt) ->
                (PQ.insert (wallEvent bw prt bt) queue)       

                
closestWall: Particle -> Float -> Float -> Maybe (Wall, Float)
closestWall prt width height = 
    List.foldl (\w -> \m -> case predictWall w prt width height of
                                Nothing ->
                                   m
                                Just t ->
                                   case m of
                                       Nothing ->
                                          Just (w, t)
                                       Just (mw, mt) ->
                                          if (t < mt) then
                                              Just (w, t)
                                          else
                                              m
               )
               Nothing
               [LEFT, RIGHT, FLOOR, ROOF]

-- Events:  hitting a wall or pasue. We need to order them in time.

lessEvent: Event -> Event -> Bool
lessEvent a b =
    let
        ta = case a of
                 WALL _ _ time -> time
                 PAUSE time -> time                          
        tb = case b of
                 WALL _ _ time -> time
                 PAUSE time -> time                          
    in
        ta < tb

                   
wallEvent: Wall -> Particle -> Float -> Event
wallEvent wl prt t =
    WALL wl prt.id t
 
pauseEvent: Float -> Event
pauseEvent t =
    PAUSE t


    
-- Particles -------------------------------------------------------------------


newParticle: Int -> Pos -> Vel -> Float -> Float -> Color -> Particle
newParticle i (xi, yi) (vxi, vyi) ti ri colori =
    { id = i
    , x = xi
    , y = yi
    , vx = vxi
    , vy = vyi
    , time = ti
    , mass = 1
    , radius = ri
    , color = colori
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
    

                    
-- Prediction of collisions given a particle.

predictWall: Wall -> Particle -> Float -> Float -> Maybe Float
predictWall wall prt width height = 
    case wall of
        LEFT ->
            if (prt.vx >= 0) then
                Nothing
            else
                Just (prt.time + ((prt.radius - prt.x) / prt.vx))
        RIGHT ->
            if (prt.vx <= 0) then
                Nothing
            else
                Just (prt.time + ((width - prt.radius - prt.x) / prt.vx))
        FLOOR ->
            let
                distance = -(prt.y - prt.radius)
            in
                if (prt.vy < 0) then 
                    Just (prt.time + distance/prt.vy)
                else
                    Nothing
        ROOF ->
            if (prt.vy <= 0) then
                Nothing
            else
                let 
                    distance = (height - prt.y) - prt.radius
                in
                    Just (prt.time + (distance/prt.vy))


-- Outcome of collision

collWall: Wall -> Particle -> Float  -> Particle
collWall wall prt t =
    let 
        prti = moveParticle prt t
    in
        case wall of
            LEFT ->
                {prti | vx = -prti.vx }
            RIGHT ->
                {prti | vx = -prti.vx }            
            FLOOR ->
                {prti | vy = -prti.vy }
            ROOF ->
                {prti | vy = -prti.vy }            

        
