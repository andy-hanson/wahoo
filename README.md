# Wahoo

Wahoo is an Object-oriented Scala game engine that I made in college.
There is little documentation, but the following should work as an overview.


## Building

Install SBT: `http://www.scala-sbt.org/release/tutorial/Installing-sbt-on-Linux.html`

Then: `sbt run`

It should download the appropriate versions of sbt, scalac, and lwjgl.


## Playing the demo game, Simon

Press Enter to start playing.
Press the arrow keys to match what Simon shows you.
Then press them again from memory.
The red box will count down your time left.

Mess up even once and it's all over!


## Creating a game

(paths are relative to `src/main/scala/org/wahoo`)

To create a new game, make a new class extending `WahooMain`.
This class itself is only responsible for setting certain game-wide properties such as the window size.

All game data is held in `GameState` objects, which are switched between by the `WahooMain`.
The `WahooMain` is responsible only for declaring the `StartGameState`.

`GameState`s operate like a stack.
You can call `pushState` at any time to pause the current GameState and enter a new one, or `popState` to return to the previous one. (This won't happen until the end of the current frame.)
`replaceState` transitions the current state without saving it on the stack.


### GameState

`state/GameState`

Much like `WahooMain` is responsible only for determining `StartGameState`, a `GameState` is mostly just responsible for setting `initObjects`, the set of `GameObject`s that it starts out with.

`GameState`s access their `WahooMain` through the `main` method.
`GameObject`s access their `GameState` through the `gameState` method.

Most `GameObject`s have a physical presense on the 2D plane, as indicated by the InArena trait.
The state should set the `ArenaSize` attribute to determine how big it is.
`GameObject`s leaving the arena should override `onStageExit`.


## GameObjects

`gameObject/GameObject`

Since almost everything is a GameObject, it's a minimal interface.
Every one can `step` each frame, doing arbitrary work except for drawing.
They can also `draw`, which is called in z-order and should not affect the `GameObject`'s state.

After the GameState adds `initObjects`, GameObjects can give birth to new ones using `export` and die using `die`.

Functionality is provided through traits. The basic GameObject traits are:

Trait | Meaning
:-: | :-:
`InArena` | Having a specific `shape` in the 2D plane.
`InArenaAndMutableShape` | Moves that shape around.
`Moves` | And keeps track of velocity.
`Sprite` | Has an `Animation` that it uses to draw.

Combining all these, we get the `InArenaAndMutableShapeAndMovesSprite` type, suitable for most common `GameObject`s.
Just kidding, it's called `Moo`.


### @Catalogued

`gameObject/Catalogued`

When you want to be able to get all objects of a type, mark that type as `@Catalogued`.
Then you can get those with `all[MyType]` or iterate through them with `each` or `eachWhen`.

If the type is `InArena`, you can get colliding ones using `colliding[T]`, `collidingWhen[T]`, `eachColliding[T]`, or `eachCollidingWhen[T]`.


### Unique

`gameObject/Unique`

When a type implements `Unique`, there should only be one instance in a given `GameState`.
You can then access the instance using `the[T]` or `maybeThe[T]` (if it might be missing) or `maybeTheColliding[T]` (if it is InArena).


### State

`gameObject/logic/HasState`

Every GameObject has one State at any time.
Anything can be a State. By default you get `Idle`.
To start a state, call `be`.
If something should happen when you enter or leave a state, override `begin` and `end`.

Generally, GameObjects will pattern match on `state` during their `step`. The `state` object is for storing data related to the *current* state; data that you always need (such as velocity) does not belong in it.


### Flags

`gameObject/logic/HasFlags`

There can only be one State at a time. If you want multiple on/off conditions, inherit from `HasFlags`.


### Time

`gameObject/time`

If you want your GameObjects to take actions delayed by a certain amount of time, inherit from `UsesTimers`.
Times are always in frames.


### Input

`gameObject/control`

Get controller feedback by implementing `ControllerListener`.
This acts on an abstract controller, which could be a keyboard or game controller.

You can get the desired direction at any time by calling `joyStick`. (On keyboards this will allow 8 directions through combining arrow keys.)

The `Button` type is similarly abstract. Currently the keyboard implementation of buttons is hardcoded via `KeyboardController.keyButtonMap`.


### Camera

`gameObject/camera`

Each `GameState` by default gets a Unique `Camera` in its `initObjects`.
You can make it follow a GameObject using `follow`, or just mark a type as `FollowedByCamera`.


### Light

`gameObject/light`

Each `GameState` by default gets a Unique `LightTracker` in its `initObjects`, which stores a 2D map of colors.
(To have interesting light you'll want to override the GameState's `startAmbientLight` to be something other than white.)

An `InArena` that is not `NotLit` will be affected by light in the environment.

`EmitsLight`s affect the light map around them.
`PointLight` is the simplest kind, needing just a `lightRadius` and `lightColor`.

You can visualize light by turning on `Settings.DrawLightNodes`.



## License

DO WHATEVER THE FUCK YOU WANT, PUBLIC LICENSE
TERMS AND CONDITIONS FOR COPYING, DISTRIBUTION AND MODIFICATION

0. You just DO WHATEVER THE FUCK YOU WANT.
