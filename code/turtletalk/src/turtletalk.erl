-module(turtletalk).

-export([new_canvas/0, blast/1, new_turtle/1, picture/1, turtles/1, graveyard/1]).
-export([forward/2, anti/2, clock/2, setpen/2, clone/2, position/1]).
-export([start/0]).

%-type position() :: {integer(), integer()}.
%-type line_seg() :: {position(), position()}.
%-type picture()  :: [line_seg()].

%% Turtle API

-record(turtle, {id, x, y, angle, pen}).
-define(TURTLE_MAP, global_turtle_map).

start() ->
    set_turtle_map(maps:new()),
    % Other initialization code
    ok.

get_turtle_state(TurtleID) ->
    case maps:get(TurtleID, ?TURTLE_MAP, undefined) of
        undefined -> {error, "Turtle not found"};
        TurtleState -> {ok, TurtleState}
    end.

% Function to update the state of a turtle
update_turtle_state(TurtleID, TurtleState) ->
    case maps:is_key(TurtleID, ?TURTLE_MAP) of
        false -> {error, "Turtle not found"};
        true -> 
            NewMap = maps:put(TurtleID, TurtleState, ?TURTLE_MAP),
            set_turtle_map(NewMap),
            ok
    end.

% Helper function to set the global turtle map
set_turtle_map(NewMap) -> put(?TURTLE_MAP, NewMap).

forward(T, N) when is_integer(N), N >= 0 ->
    % Fetch the turtle state
    case get_turtle_state(T) of
        {ok, #turtle{id = T, x = X, y = Y, angle = Angle, pen = Pen}} ->
            % Calculate new position based on angle
            {NewX, NewY} = case Angle of
                0   -> {X + N, Y};    % Moving east
                90  -> {X, Y + N};    % Moving north
                180 -> {X - N, Y};    % Moving west
                270 -> {X, Y - N};    % Moving south
                _   -> {X, Y}         % Invalid angle
            end,
            % Update turtle state
            update_turtle_state(T, #turtle{id = T, x = NewX, y = NewY, angle = Angle, pen = Pen}),
            ok;
        {error, _Reason} ->
            {error, "Turtle not found"}
    end;
forward(_, _) ->
    {error, "Invalid input"}.

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
