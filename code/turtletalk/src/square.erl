-module(square).
-export([draw/0, draw_to/1]).

draw() ->
    {ok, C} = turtletalk:new_canvas(), %%  C = turtletalk:new_canvas() -> This is the pattern matching I'm doing in my turtletalk.erl file
    {ok, T} = turtletalk:new_turtle(C),  %% T = turtletalk:new_turtle(C) -> This is the pattern matching I'm doing in my turtletalk.erl file
    turtletalk:setpen(T, down),
    turtletalk:forward(T, 42),
    turtletalk:anti(T, 90),
    turtletalk:forward(T, 42),
    turtletalk:anti(T, 90),
    turtletalk:forward(T, 42),
    turtletalk:anti(T, 90),
    turtletalk:forward(T, 42),
    ok = turtletalk:anti(T, 90),
    {ok, Pic} = turtletalk:picture(C),
    Pic.

draw_to(File) ->
    Pic = draw(),
    svg:write(File, Pic).
