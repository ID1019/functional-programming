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
    = COLLISION (Int, Int) (Int, Int) Float
    | WALL Wall (Int, Int) Float
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
        , count : Int       -- number of collisions
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

-- Each step will take an event from the queue and update the particle, if asked to pause all particles are moved forard in time.            
            
step: (Dict Int Particle) -> PriorityQueue Event -> Float ->  Float -> Float -> (Dict Int Particle, PriorityQueue Event, Float)
step parts queue tm width height =
    case PQ.next queue of
        Nothing ->
            (parts, queue, tm)
        Just (evt, tail) ->
            case evt of
                COLLISION (id_a, cnt_a) (id_b, cnt_b) time ->
                    let
                        (updParts, updQueue) = case (Dict.get id_a parts, Dict.get id_b parts ) of
                                                   (Just prtA, Just prtB) ->
                                                       if ((cnt_a ==  prtA.count) && (cnt_b ==  prtB.count)) then
                                                           let 
                                                               (updA, updB) = collPart prtA prtB time
                                                               updP = List.foldl (\(i,p) -> \ps -> (Dict.insert i p ps)) 
                                                                             (List.foldl (\id -> \ps -> Dict.remove id ps) parts [prtA.id, prtB.id])
                                                                             [(updA.id, updA), (updB.id, updB)]
                                                               updQ = List.foldl (\p -> \q -> predictPart p updP width height q) tail [updA, updB]
                                                           in
                                                               (updP, updQ)
                                                       else
                                                           (parts, tail)
                                                   (_, _) ->
                                                       (parts, tail)
                    in
                        step updParts updQueue time width height                                                

                WALL wall (id,count) time ->
                        let 
                            (updParts, updQueue) = case (Dict.get id parts) of
                                                           Nothing ->
                                                               (parts, tail)
                                                           Just prt ->
                                                               if (prt.count == count) then
                                                                   let 
                                                                       upd = collWall wall prt time 
                                                                       updP  = Dict.insert upd.id upd (Dict.remove id parts)
                                                                       updQ = predictPart upd updP width height tail
                                                                   in
                                                                       (updP, updQ)
                                                               else
                                                                   (parts, tail)            
                        in
                            step updParts updQueue time width height
                PAUSE time ->
                        let
                            updated = Dict.map  (\i -> \p -> moveParticle p time) parts
                        in
                            (updated, tail, time)

-- Predicting future collisions with other particles, walls, roof and floor.

predictPart: Particle -> (Dict Int Particle) -> Float -> Float -> PriorityQueue Event -> PriorityQueue Event
predictPart prt prts width height queue =
    let
        closest = closestWall prt width height 
        updated = case closest of
                     Nothing ->
                         queue
                     Just (bw, bt) ->
                         (PQ.insert (wallEvent bw prt bt) queue)
    in
        Dict.foldl (\_ -> \p -> \q -> checkPart prt p (\_ -> True) q)
            updated 
            prts

                
checkPart: Particle -> Particle -> (Float -> Bool) -> PriorityQueue Event -> PriorityQueue Event
checkPart prt p check queue = 
    case (predictColl prt p) of
        Nothing ->
            queue
        Just t ->
            if ( check t) then
                PQ.insert (collEvent prt p t) queue
            else
                queue

                               
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

-- Events: collision, hitting a wall or pasue. We need to order them in time.

lessEvent: Event -> Event -> Bool
lessEvent a b =
    let
        ta = case a of
                 COLLISION _ _ time -> time
                 WALL _ _ time -> time
                 PAUSE time -> time                          
        tb = case b of
                 COLLISION _ _ time -> time
                 WALL _ _ time -> time
                 PAUSE time -> time            
    in
        ta < tb

                   
collEvent: Particle -> Particle -> Float -> Event
collEvent prtA prtB t =
    COLLISION (prtA.id,prtA.count) (prtB.id,prtB.count) t

wallEvent: Wall -> Particle -> Float -> Event
wallEvent wl prt t =
    WALL wl (prt.id, prt.count) t
 
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
    , count = 0
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
        yi = prt.y + (prt.vy * dt)
    in
    {prt | x = xi, y = yi,  time = ti}
    

                    
-- Prediction of collisions given a particle.

predictColl: Particle -> Particle -> Maybe Float
predictColl prtA prtB  =
    if (prtA.id == prtB.id) then
        Nothing
    else
        let
            -- move particles to same local time 
            (a, b, t) = if (prtA.time < prtB.time) then
                         (moveParticle prtA prtB.time,  prtB, prtB.time)
                     else
                         (prtA, moveParticle prtB prtA.time, prtA.time)

            -- the delta position and velocity
            dx = b.x - a.x
            dy = b.y - a.y
            dvx = b.vx - a.vx
            dvy = b.vy - a.vy  

            -- the absolute velocity squared
            k2 = (dvx^2 + dvy^2)
        in
            if ( k2 == 0 ) then
                Nothing
            else
                let 
                    p = ((2*dx*dvx)+(2*dy*dvy))/k2
                    q = (dx^2 + dy^2 - (a.radius+b.radius)^2)/k2
                    sq = (p/2)^2 - q
                in
                    if (sq < 0 ) then 
                        Nothing
                    else
                        let
                            ct = -(p/2) - (sqrt sq)
                        in
                            if (ct <= 0)  then
                                Nothing
                            else
                                Just (t + ct)


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

collPart: Particle -> Particle -> Float -> (Particle, Particle)
collPart prtA prtB t =
    let
        a = moveParticle prtA t 
        b = moveParticle prtB t                 

        dx  = b.x - a.x
        dy  = b.y - a.y
        dvx = b.vx - a.vx
        dvy = b.vy - a.vy
        dvdr = dx*dvx + dy*dvy       -- dv dot dr
        dist = a.radius + b.radius   -- distance between particle centers at collison

        -- initial momentum 

        mx1 = (b.vx * b.mass + a.vx * a.mass)
        my1 = (b.vy * b.mass + a.vy * a.mass)   
        
        -- magnitude of normal force
        magnitude = 2 * a.mass * b.mass * dvdr / ((a.mass + b.mass) * dist)

        -- normal force, and in x and y directions
        fx = magnitude * dx / dist
        fy = magnitude * dy / dist
    in
        -- update velocities according to normal force
        ( {a | vx = a.vx + fx / a.mass, vy = a.vy + fy / a.mass, count = a.count + 1}
        , {b | vx = b.vx - fx / b.mass, vy = b.vy - fy / b.mass, count = b.count + 1})
        

collWall: Wall -> Particle -> Float -> Particle
collWall wall prt t =
    let 
        prti = moveParticle prt t
    in
        case wall of
            LEFT ->
                {prti | vx = -prti.vx, count = prti.count+1}
            RIGHT ->
                {prti | vx = -prti.vx, count = prti.count+1}            
            FLOOR ->
                {prti | vy = -prti.vy, count = prti.count+1}
            ROOF ->
                {prti | vy = -prti.vy, count = prti.count+1}            

        
