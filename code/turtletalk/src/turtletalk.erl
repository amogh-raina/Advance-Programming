-module(turtletalk).
-export([line_segment/3,dead_turtle/2,clone_turtle_map/6,get_canvas/1,count_turtles/1, collect_live_turtle_pictures/2,collect_dead_turtle_pictures/2]).
-export([new_canvas/0, blast/1, new_turtle/1, picture/1, turtles/1, graveyard/1]).
-export([forward/2, anti/2, clock/2, setpen/2, clone/2, position/1]).

%-type position() :: {integer(), integer()}.
%-type line_seg() :: {position(), position()}.
%-type picture()  :: [line_seg()].

%%%% Helper Functions %%%%
%%% Add a line segment in the canvas map for turtle T's drawing history
line_segment(CanvasMap, T, {PositionA, PositionB}) ->
    {DoA, Segment} = maps:get(T, CanvasMap),
    UpdateSegment = Segment ++ [{PositionA, PositionB}],
    UpdatedCanvasMap = maps:put(T, {DoA, UpdateSegment}, CanvasMap),
    UpdatedCanvasMap.

%% Logic of dead turtle
dead_turtle(CanvasMap, T) ->
    UpdatedCanvasMap = maps:put(T, {dead,[]}, CanvasMap),
    UpdatedCanvasMap.

%% Count Alive and Dead Turtles
count_turtles(CanvasMap) ->
    LiveTurtles = [T || {T, {alive, _}} <- maps:to_list(CanvasMap)],
    DeadTurtles = [T || {T, {dead, _}} <- maps:to_list(CanvasMap)],
    {LiveTurtles, length(DeadTurtles)}.

%%% Colleect Live Turtle Pictures
collect_live_turtle_pictures([], Pictures) ->
    lists:reverse(Pictures);
collect_live_turtle_pictures([{_, {alive, TurtleData}} | Rest], Pictures) ->
    {_, TurtlePictures} = TurtleData,
    collect_live_turtle_pictures(Rest, [TurtlePictures | Pictures]);
collect_live_turtle_pictures([_|Rest], Pictures) ->
    collect_live_turtle_pictures(Rest, Pictures).

%% colleect dead turtle pictures
collect_dead_turtle_pictures([], Pictures) ->
    lists:reverse(Pictures);
collect_dead_turtle_pictures([{_, {dead, TurtleData}} | Rest], Pictures) ->
    {_, TurtlePictures} = TurtleData,
    collect_dead_turtle_pictures(Rest, [TurtlePictures | Pictures]);
collect_dead_turtle_pictures([_|Rest], Pictures) ->
    collect_dead_turtle_pictures(Rest, Pictures).
%% Get Canvas Map
get_canvas(_) -> implement. 

%% Clone Turtle Map Function
clone_turtle_map(_C, _Position, _Angle, _PenState, TurtleList, 0) ->
    TurtleList;
clone_turtle_map(C, Position, Angle, PenState, TurtleList, N) when N > 0 ->
    T = new_turtle(C),
    clone_turtle_map(C, Position, Angle, PenState, [{T, {alive, []}} | TurtleList], N - 1).

% Initialize a canvas process with a given ID
new_canvas() ->
   spawn(fun() -> canvas_loop(#{}) end).
% Initialize a turtle process with a given canvas ID
new_turtle(CanvasId) ->
    Position = {0, 0},
    Angle = 0,
    PenState = up,
    CanvasMap = get_canvas(CanvasId),
    line_segment(CanvasMap, self(), {{0,0}, {0,0}}),
    spawn(fun() -> 
        turtle_loop(CanvasId, Position, Angle, PenState) end).

%% Turtle Loop API
turtle_loop(C, {X, Y}, Angle, PenState) -> 
    receive
        {From, forward, N} when is_integer(N) ->
            CurrentTurtle = {C, {X, Y}, Angle, PenState},
            case N of
                0 ->
                    From ! {reply, ok, CurrentTurtle},
                    turtle_loop(C, {X, Y}, Angle, PenState);
                N when N > 0 ->
                    NewPosition =
                        case Angle of
                            0 ->
                                {X + N, Y};
                            90 ->
                                {X, Y + N};
                            180 ->
                                {X - N, Y};
                            270 ->
                                {X, Y - N};
                            _ ->
                                {X, Y} % Invalid angle, keep the same position
                        end,
            NewTurtle = {C, NewPosition, Angle, PenState},
            if PenState == down ->
                line_segment(C, self(), {{X,Y}, NewPosition});
                PenState == up ->
                    io:format("Pen is up.~n"),
                    {reply,ok, CurrentTurtle}
                    end,
             From ! {reply, ok, NewTurtle},
            turtle_loop(C, NewPosition, Angle, PenState);

        _ -> 
        {ok, CanvasMap} = get_canvas(C),
        dead_turtle(CanvasMap, self()),
        From ! {error, invalid_message},
            turtle_loop(C, {X, Y}, Angle, PenState)
            
end;

    {From, anti, Degree} ->
            case lists:member(Degree, [0, 90, 180, 270]) of
                true ->
                    Anti_Angle = Angle + Degree, %% check for clockwise or anti-clockwise??
                    NewTurtle = {C, {X,Y}, Anti_Angle, PenState},
                    {reply, ok, NewTurtle},
                    turtle_loop(C, {X, Y}, Anti_Angle, PenState);
                false ->
                    {ok, CanvasMap} = get_canvas(C),
                    dead_turtle(CanvasMap, self()),
                    From ! {error, "Invalid angle"}
            end;
                    
       
     {From, clock, Degree} ->
        case lists:member(Degree, [0, 90, 180, 270]) of
            true ->
                Clock_Angle = Angle - Degree, %% check for clockwise or anti-clockwise??
                NewTurtle = {C, {X,Y}, Clock_Angle, PenState},
                {reply, ok, NewTurtle},
                turtle_loop(C, {X, Y}, Clock_Angle, PenState);
            false ->
                {ok, CanvasMap} = get_canvas(C),
                dead_turtle(CanvasMap, self()),
                From ! {error, "Invalid angle"}
        end;

    {From, setpen, NewPenState} ->
        case NewPenState of
            up ->
                NewTurtle = {C, {X,Y}, Angle, up},
                {reply, ok, NewTurtle},
                turtle_loop(C, {X, Y}, Angle, NewPenState);
            down -> 
                NewTurtle = {C, {X,Y}, Angle, down},
                {reply, ok, NewTurtle},
                turtle_loop(C, {X, Y}, Angle, NewPenState);
            _ ->
                {ok, CanvasMap} = get_canvas(C),
                dead_turtle(CanvasMap, self()),
                From ! {error, "Invalid angle"}
    end;

    {From, clone, N} ->
            case N >= 0 of
                true ->
                    TurtleList = clone_turtle_map(C, {X, Y}, Angle, PenState, [], N),
                   % NewTurtle = {C, {X, Y}, Angle, PenState},
                    From ! {ok, TurtleList},
                    turtle_loop(C, {X, Y}, Angle, PenState);
                false ->
                    {ok, CanvasMap} = get_canvas(C),
                dead_turtle(CanvasMap, self()),
                From ! {error, "Invalid angle"}
            end;
   {From, position, P} -> 
        P = {self(), {ok, {X, Y}}},
        From ! P,
        turtle_loop(C, {X, Y}, Angle, PenState);
    
    _ ->
            io:format("Invalid message received by Turtle.~n"),
            turtle_loop(C, {X, Y}, Angle, PenState)
    end.



%% Canvas API

     
% Canvas loop function
canvas_loop(Canvas) ->
    receive
        {_, blast} ->
            {reply, ok};
        {From, picture} -> 
            LiveTurtlePictures = collect_live_turtle_pictures(Canvas, []),

            % Concatenate the pictures
            CombinedPicture = lists:concat(LiveTurtlePictures),

            From ! {reply, ok, CombinedPicture},

            canvas_loop(Canvas);


        {From, turtles} ->
            {LiveTurtles, DeadTurtles} = count_turtles(Canvas),
            From ! {reply, ok, {LiveTurtles, DeadTurtles}},
            canvas_loop(Canvas);

        {From, graveyard} ->
            DeadTurtlePictures = collect_dead_turtle_pictures(Canvas, []),

            % Concatenate the pictures
            CombinedGraveyardPicture = lists:concat(DeadTurtlePictures),

            From ! {reply, ok, CombinedGraveyardPicture},
            canvas_loop(Canvas);
        _ ->
            io:format("Invalid message received by Canvas.~n"),
            canvas_loop(Canvas)
    end.



% Receive calls for Turtle_loop
forward(T, N) -> 
    T ! {self(), forward,N},
    receive
        ok -> ok;
        {error, Reason} -> {error, Reason}
    end.

anti(T, Degree) ->
    T ! {self(), anti, Degree},
    receive
        ok -> ok;
        {error, Reason} -> {error, Reason}
    end.

clock(T, Degree) ->
    T ! {self(), clock, Degree},
    receive
        ok -> ok;
        {error, Reason} -> {error, Reason}
    end.

setpen(T, P) -> 
    T ! {self(), setpen, P},ok.

clone(T, N) ->
    T ! {self(), {clone, N}},
    receive
        {ok, TurtleList} -> {ok, TurtleList};
        {error, Reason} -> {error, Reason}
    end.

position(T) -> 
    T ! {self(), position},
    receive
        {ok, Position} -> {ok, Position}
    end.

%% Receive calls for Canvas_loop
blast(Canvas) ->
    Canvas ! {self(), blast},
    receive
        ok -> ok;
        {error, Reason} -> {error, Reason}
    end.

picture(Canvas) -> 
    Canvas ! {self(), picture},
    receive
        {ok, P} -> {ok, P};
        {error, Reason} -> {error, Reason}
    end.
turtles(Canvas) ->
    Canvas ! {self(), turtles},
    receive
        {ok,{LA,N}} -> {ok,{LA,N}};
        {error, Reason} -> {error, Reason}
    end.

graveyard(Canvas) ->
    Canvas ! {self(), graveyard},
    receive
        ok -> ok;
        {error, Reason} -> {error, Reason}
    end.
