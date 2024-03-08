module Simulator exposing (..)

import Dict exposing (Dict)

import PriorityQueue as PQ exposing (PriorityQueue)

type alias Simulator = 
    { particles : Dict Int Particle     -- all particles in a Dict ordered by their id
    , queue : PriorityQueue Event       -- priority queue of all future events
    , id : Int                          -- next id to use for particles
    , time : Float                      -- current simulation time
    , gravity : Float                   -- gravity 
    , width : Float                     -- width, stays fixed during simulation
    , height : Float                    -- height, stays fixed during simulation
    }

-- Three types of events: collision between two particles, particle hitting wall and pause of simulation.    

type Event
    = COLLISION {idA : Int, idB : Int, countA : Int, countB : Int, time : Float}
    | WALL {wall : Wall, id : Int, count : Int, time : Float}
    | PAUSE {time : Float} 

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
    
init: Int -> Int -> Simulator
init w h =
    let
        sim = Simulator Dict.empty (PQ.empty lessEvent) 0 0 1.0 (toFloat w) (toFloat h)
    in
        addParticle (100,200) (10,10) 10 "red" sim
            

clear: Simulator -> Simulator        
clear sim =
    {sim | particles = Dict.empty, queue = (PQ.empty lessEvent), id = 0, time = 0}


addParticle: Pos -> Vel -> Float -> Color -> Simulator -> Simulator
addParticle pos vel rad col sim = 
    let
        id = sim.id
        p = newParticle id pos vel sim.time rad col
    in
        {sim | id = id+1
             , particles = Dict.insert id p sim.particles
             , queue = predictPart p sim.particles sim.gravity sim.width sim.height sim.queue}

particles: Simulator -> (List Particle)
particles sim =
    Dict.values sim.particles

-- Running the simulation: for a given time, ie.e insert a pause event and go.

run: Simulator -> Float -> Simulator
run sim hold  =
    let
        queue = PQ.insert (pauseEvent (sim.time + hold)) sim.queue
        (prts, que, tm) = step sim.particles queue sim.time sim.gravity sim.width sim.height
    in
        {sim | time = tm, particles = prts, queue = que}

-- Each step will take an event from the queue and update the particle, if asked to pause all particles are moved forard in time.            
            
step: (Dict Int Particle) -> PriorityQueue Event -> Float -> Float ->  Float -> Float -> (Dict Int Particle, PriorityQueue Event, Float)
step parts queue tm gravity width height =
    case PQ.next queue of
        Nothing ->
            (parts, queue, tm)
        Just (evt, tail) ->
            case evt of
                COLLISION e ->
                    let
                        (updParts, updQueue) = case (Dict.get e.idA parts, Dict.get e.idB parts ) of
                                                   (Just prtA, Just prtB) ->
                                                       if ((e.countA ==  prtA.count) && (e.countB ==  prtB.count)) then
                                                           let 
                                                               (updA, updB) = collPart prtA prtB e.time gravity
                                                               updP = List.foldl (\(i,p) -> \ps -> (Dict.insert i p ps)) 
                                                                             (List.foldl (\id -> \ps -> Dict.remove id ps) parts [prtA.id, prtB.id])
                                                                             [(updA.id, updA), (updB.id, updB)]
                                                               updQ = List.foldl (\p -> \q -> predictPart p updP gravity width height q) tail [updA, updB]
                                                           in
                                                               (updP, updQ)
                                                       else
                                                           (parts, tail)
                                                   (_, _) ->
                                                       (parts, tail)
                    in
                        step updParts updQueue e.time gravity width height                                                

                WALL e ->
                        let 
                            (updParts, updQueue) = case (Dict.get e.id parts) of
                                                           Nothing ->
                                                               (parts, tail)
                                                           Just prt ->
                                                               if (prt.count == e.count) then
                                                                   let 
                                                                       upd = collWall e.wall prt e.time gravity
                                                                       updP  = Dict.insert upd.id upd (Dict.remove e.id parts)
                                                                       updQ = predictPart upd updP gravity width height tail
                                                                   in
                                                                       (updP, updQ)
                                                               else
                                                                   (parts, tail)            
                        in
                            step updParts updQueue e.time gravity width height
                PAUSE e ->
                        let
                            updated = Dict.map  (\i -> \p -> moveParticle p e.time gravity) parts
                        in
                            (updated, tail, e.time)

-- Predicting future collisions with other particles, walls, roof and floor.

predictPart: Particle -> (Dict Int Particle) -> Float -> Float -> Float -> PriorityQueue Event -> PriorityQueue Event
predictPart prt prts gravity width height queue =
    let
        closest = closestWall prt gravity width height 
        updated = case closest of
                     Nothing ->
                         queue
                     Just (bw, bt) ->
                         (PQ.insert (wallEvent bw prt bt) queue)
    in
        Dict.foldl (\_ -> \p -> \q -> checkPart prt p gravity (\_ -> True) q)
            updated 
            prts

                
checkPart: Particle -> Particle -> Float -> (Float -> Bool) -> PriorityQueue Event -> PriorityQueue Event
checkPart prt p gravity check queue = 
    case (predictColl prt p gravity) of
        Nothing ->
            queue
        Just t ->
            if ( check t) then
                PQ.insert (collEvent prt p t) queue
            else
                queue

                               
closestWall: Particle -> Float -> Float -> Float -> Maybe (Wall, Float)
closestWall prt gravity width height = 
    List.foldl (\w -> \m -> case predictWall w prt gravity width height of
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
                 COLLISION r -> r.time
                 WALL r -> r.time
                 PAUSE r -> r.time                          
        tb = case b of
                 COLLISION r -> r.time
                 WALL r -> r.time
                 PAUSE r -> r.time                          
    in
        ta < tb

                   
collEvent: Particle -> Particle -> Float -> Event
collEvent prtA prtB t =
    COLLISION {idA = prtA.id, idB = prtB.id, countA = prtA.count, countB = prtB.count, time = t}

wallEvent: Wall -> Particle -> Float -> Event
wallEvent wl prt t =
    WALL {wall = wl, id = prt.id, count = prt.count, time = t}
 
pauseEvent: Float -> Event
pauseEvent t =
    PAUSE {time = t}


    
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

moveParticle: Particle -> Float -> Float -> Particle  
moveParticle prt ti g =
    let
        dt = ti - prt.time
        xi = prt.x + (prt.vx * dt)
        yi = prt.y + ((prt.vy  - ((g * dt)/2)) * dt)
        vyi = prt.vy - (g * dt)
    in
    {prt | x = xi, y = yi,  vy = vyi, time = ti}
    

                    
-- Prediction of collisions given a particle.

predictColl: Particle -> Particle -> Float -> Maybe Float
predictColl prtA prtB gravity =
    if (prtA.id == prtB.id) then
        Nothing
    else
        let
            -- move particles to same local time 
            (a, b, t) = if (prtA.time < prtB.time) then
                         (moveParticle prtA prtB.time gravity,  prtB, prtB.time)
                     else
                         (prtA, moveParticle prtB prtA.time gravity, prtA.time)

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


predictWall: Wall -> Particle -> Float -> Float -> Float -> Maybe Float
predictWall wall prt gravity width height = 
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
                if (gravity == 0) then
                    if (prt.vy < 0) then 
                        Just (prt.time + distance/prt.vy)
                    else
                        Nothing
                else
                    let 
                        p = -(2 * prt.vy/gravity)
                        q = 2 * distance/gravity
                        sq = (p/2)^2 - q
                    in
                        Just (prt.time - (p/2)  + sqrt sq)
        ROOF ->
            if (prt.vy <= 0) then
                Nothing
            else
                let 
                    distance = (height - prt.y) - prt.radius
                in
                    if (gravity == 0) then
                        Just (prt.time + (distance/prt.vy))
                    else
                        let
                            p = -(2 * prt.vy/gravity)
                            q = 2 * distance/gravity
                            sq = (p/2)^2 - q
                        in
                            if (sq < 0) then 
                                Nothing
                            else
                                -- we will hit the roof (second hit is on the way down)
                                Just (prt.time - (p/2) - sqrt sq)


-- Outcome of collision

collPart: Particle -> Particle -> Float -> Float -> (Particle, Particle)
collPart prtA prtB t g =
    let
        a = moveParticle prtA t g
        b = moveParticle prtB t g                

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
        

collWall: Wall -> Particle -> Float -> Float -> Particle
collWall wall prt t g =
    let 
        prti = moveParticle prt t g
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

        
