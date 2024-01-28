-module(turtletalk).

-export([new_canvas/0, blast/1, new_turtle/1, picture/1, turtles/1, graveyard/1]).
-export([forward/2, anti/2, clock/2, setpen/2, clone/2, position/1]).

%-type position() :: {integer(), integer()}.
%-type line_seg() :: {position(), position()}.
%-type picture()  :: [line_seg()].

%% Turtle API

forward(_, _) -> not_implemented.
anti(_, _) -> not_implemented.
clock(_, _) -> not_implemented.
setpen(_, _) -> not_implemented.
clone(_, _) -> not_implemented.
position(_) -> not_implemented.

%% Canvas API

new_canvas() -> not_implemented.
blast(_) -> not_implemented.
new_turtle(_) -> not_implemented.
picture(_) -> not_implemented.
turtles(_) -> not_implemented.
graveyard(_) -> not_implemented.
