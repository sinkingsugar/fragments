import times, os

# A robust game-engine inspired run loop timer, using nim concepts

# TODO, CGameState is not working at all for now.. since it's not var and we don't assign it properly

type
  CGameState* = concept x, y, type T
    x + y is T
    x * float is T
  
  CGame* = concept x
    x is ref
    
    type T = auto
    x.stateType is type(T)
    
    x.fps is float
    x.time is float
    x.quitting is bool
    
    x.start()
    x.fixedUpdate(CGameState)
    x.update(CGameState, float)

proc runGame*(game: CGame) =
  game.start()

  let oneFrame = 1.0 / game.fps
  var
    currentTime = epochTime()
    accumulator = 0.0
    previousState: game.stateType
    currentState: game.stateType
  
  while not game.quitting:
    let newTime = epochTime()
    var frameTime = newTime - currentTime
    
    currentTime = newTime
    accumulator += frameTime

    while accumulator >= oneFrame:
      previousState = currentState
      game.fixedUpdate(currentState)
      game.time += oneFrame
      accumulator -= oneFrame

    let 
      alpha = accumulator / oneFrame
      state = currentState * alpha + previousState * (1.0 - alpha)

    game.update(state, frameTime)

when isMainModule:
  type
    MyGameState = object
    MyGame = ref object
      fps: float
      time: float
      quitting: bool  

  template stateType(_: MyGame): typedesc = MyGameState

  proc `*`(x: MyGameState; y: float): MyGameState = x
  proc `+`(x, y: MyGameState): MyGameState = x

  proc start(game: MyGame) = discard
  
  proc fixedUpdate(game: MyGame; state: MyGameState) =
    echo "Update"

  proc update(game: MyGame; state: MyGameState; deltaTime: float) =
    echo "Render..."
    sleep(1000)
  
  var myGame = MyGame(fps: 60, quitting: false)

  runGame(myGame)
