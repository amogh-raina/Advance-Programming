%% test_turtletalk.erl
-module(test_talk).
-export([all_tests/0]).
-export([run_tests/0]).
-include_lib("eunit/include/eunit.hrl").

forward_test() ->
    {ok, CanvasPid} = turtletalk:new_canvas(),
    {ok, TurtlePid} = turtletalk:new_turtle(CanvasPid),

    %% Move the turtle forward by 10 units
    ok = turtletalk:forward(TurtlePid, 10),

    %% Get the current position of the turtle
    {ok, Position} = turtletalk:position(TurtlePid),

    %% Check if the position is as expected (assuming starting position is {0, 0} and moving North)
    ?assertEqual({0, 10}, Position).

anti_clockwise_test() ->
    % Create a new canvas and turtle
    CanvasPid = turtletalk:new_canvas(),
    TurtlePid = turtletalk:new_turtle(CanvasPid),

    % Rotate the turtle anticlockwise by 90 degrees
    turtletalk:anti(TurtlePid, 90),

    % Get the current angle of the turtle
    {ok, Angle} = turtletalk:get_turtle_state(TurtlePid),

    % Check if the angle is as expected (assuming starting angle is 0)
    ?assertEqual(270, maps:get(angle, Angle)).

clockwise_test() ->
    % Create a new canvas and turtle
    CanvasPid = turtletalk:new_canvas(),
    TurtlePid = turtletalk:new_turtle(CanvasPid),

    % Rotate the turtle clockwise by 90 degrees
    turtletalk:clock(TurtlePid, 90),

    % Get the current angle of the turtle
    {ok, Angle} = turtletalk:get_turtle_state(TurtlePid),

    % Check if the angle is as expected (assuming starting angle is 0)
    ?assertEqual(90, maps:get(angle, Angle)).

setpen_test() ->
    % Create a new canvas and turtle
    CanvasPid = turtletalk:new_canvas(),
    TurtlePid = turtletalk:new_turtle(CanvasPid),

    % Set the pen state to down
    turtletalk:setpen(TurtlePid, down),

    % Check if the pen state is down
    {ok, PenState} = turtletalk:get_turtle_state(TurtlePid),

    ?assertEqual(down, maps:get(pen, PenState)).

clone_test() ->
    % Create a new canvas and turtle
    CanvasPid = turtletalk:new_canvas(),
    TurtlePid = turtletalk:new_turtle(CanvasPid),

    % Clone the turtle to create 3 additional turtles
    {ok, CloneIDs} = turtletalk:clone(TurtlePid, 3),

    % Check if the number of clones is as expected
    ?assertEqual(4, length(CloneIDs)).

% Add more tests as needed

all_tests() ->
    ?_assert(forward_test()),
    ?_assert(clone_test()).


run_tests() ->
    eunit:test(test_turtletalk).

