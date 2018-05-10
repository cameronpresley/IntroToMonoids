module Rover

type Direction = North | East | West | South
type Rover = {x:int; y:int; direction:Direction}

let moveForward r =
    match r.direction with
    | North -> {r with y = r.y+1}
    | South -> {r with y = r.y-1}
    | East -> {r with x = r.x+1}
    | West -> {r with x = r.x-1}

let moveBackward r =
    match r.direction with
    | North -> {r with y=r.y-1}
    | South -> {r with y=r.y+1}
    | East -> {r with x=r.x-1}
    | West -> {r with x=r.x+1}

let turnLeft r =
    match r.direction with
    | North -> {r with direction = West}
    | West -> {r with direction = South}
    | South -> {r with direction = East}
    | East -> {r with direction = North}

let turnRight r =
    match r.direction with
    | North -> {r with direction = East}
    | East -> {r with direction = South}
    | South -> {r with direction = West}
    | West -> {r with direction = North}

type Message = MoveForward | MoveBackward | TurnLeft | TurnRight


// Message -> Rover -> Rover
let update message rover =
    match message with
    | MoveForward -> moveForward rover
    | MoveBackward -> moveBackward rover
    | TurnLeft -> turnLeft rover
    | TurnRight -> turnRight rover

[
  MoveForward; 
  MoveForward; 
  TurnLeft; 
  TurnLeft; 
  MoveBackward; 
  TurnRight; 
  MoveForward; 
  MoveForward
]
|> List.map update // Message list -> (Rover -> Rover) list
|> List.reduce (>>) // (Rover -> Rover) list -> (Rover -> Rover)
<| {x=0; y=0; direction=North}