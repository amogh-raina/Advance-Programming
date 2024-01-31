-module(test_turtletalk).

-export([turtle_cmd/0, turtle_cmds/0, cmds_to_picture/1, prop_no_empty/0]).
-export([test_all/0, test_everything/0]).

-include_lib("eqc/include/eqc.hrl").

turtle_cmd() ->
    eqc_gen:oneof([{setpen, [eqc_gen:elements([up, down])]},
               {forward, [eqc_gen:int()]},
               {turn, [eqc_gen:int()]},
               {clone, []},
               {position, []}]).

turtle_cmds() ->
eqc_gen:bind(eqc_gen:choose(0,20), fun(Len) ->
    eqc_gen:list_of_length(Len, turtle_cmd())
end).

%% Executes a sequence of turtle commands and returns the resulting picture.
%% Executes a sequence of turtle commands and returns the resulting picture.
cmds_to_picture(Cmds) ->
    {ok, TurtleID} = turtletalk:new_canvas(),  % Assuming new_canvas starts a new canvas and returns its ID.
    {ok, Turtle} = turtletalk:new_turtle(TurtleID),  % Create a new turtle on the canvas.
    lists:foreach(fun(Command) -> execute_command(Turtle, Command) end, Cmds),
    turtletalk:picture(TurtleID).

%% Helper function to execute a command on the given turtle.
execute_command(Turtle, {Command, Args}) ->
    apply(turtletalk, Command, [Turtle | Args]).

%% Property to ensure no empty line segments in the generated picture.
prop_no_empty() ->
    ?FORALL(Cmds, turtle_cmds(),
            begin
                Picture = cmds_to_picture(Cmds),
                no_empty_segments(Picture)
            end).

%% @doc Checks that the picture contains no empty line segments.
%% An empty line segment is defined as having identical start and end points.
%% @spec no_empty_segments([{{integer(), integer()}, {integer(), integer()}}]) -> boolean().
no_empty_segments(Picture) ->
    lists:all(fun({Start, End}) -> Start =/= End end, Picture).
test_all() ->
    eqc:quickcheck(prop_no_empty()).
test_everything() -> test_all().
