-module(turtletalk).
-export([turtle_init/2, line_segment/3,dead_turtle/2,clone_turtle_map/6,get_canvas/1,count_turtles/1, collect_live_turtle_pictures/1,collect_dead_turtle_pictures/2]).
-export([new_canvas/0, blast/1, new_turtle/1, picture/1, turtles/1, graveyard/1]).
-export([forward/2, anti/2, clock/2, setpen/2, clone/2, position/1]).

%-type position() :: {integer(), integer()}.
%-type line_seg() :: {position(), position()}.
%-type picture()  :: [line_seg()].

%%%% Helper Functions %%%%
%%% Add a line segment in the canvas map for turtle T's drawing history
%add_line_segment(CanvasMap, T, {PositionA, PositionB}) ->
    %{_, Segment} = maps:get(T, CanvasMap),
    %UpdateSegment = Segment ++ [{PositionA, PositionB}],
    %UpdatedCanvasMap = maps:put(T, {alive, UpdateSegment}, CanvasMap),
    %UpdatedCanvasMap.

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
collect_live_turtle_pictures(CanvasMap) ->
    maps:fold(fun(_TurtleID, {alive, TurtleData}, Acc) ->
                    TurtlePictures = TurtleData,
                    [TurtlePictures | Acc];
                  (_, _, Acc) ->
                    Acc
              end, [], CanvasMap).


%% colleect dead turtle pictures
collect_dead_turtle_pictures([], Pictures) ->
    lists:reverse(Pictures);
collect_dead_turtle_pictures([{_, {dead, TurtleData}} | Rest], Pictures) ->
    {_, TurtlePictures} = TurtleData,
    collect_dead_turtle_pictures(Rest, [TurtlePictures | Pictures]);
collect_dead_turtle_pictures([_|Rest], Pictures) ->
    collect_dead_turtle_pictures(Rest, Pictures).
%% Get Canvas Map from CanvasID

%% Clone Turtle Map Function
clone_turtle_map(_C, _Position, _Angle, _PenState, TurtleList, 0) ->
    TurtleList;
clone_turtle_map(C, Position, Angle, PenState, TurtleList, N) when N > 0 ->
    T = new_turtle(C),
    clone_turtle_map(C, Position, Angle, PenState, [{T, {alive, []}} | TurtleList], N - 1).

% Initialize a canvas process with a given ID
new_canvas() ->
   CanvasId = spawn(fun() -> canvas_loop(#{}) end),
   CanvasId.
% Initialize a turtle process with a given canvas ID
new_turtle(CanvasId) ->
    TurtleID = spawn(fun() -> turtle_init(CanvasId,self()) end),
    
    CanvasId ! {add_new_turtle, TurtleID},
    TurtleID.

turtle_init(CanvasId,TurtleID) ->
    receive
        {add_new_turtle_ack, ok} ->
            io:format("Turtle successfully added to canvas.~n"),
            turtle_loop(TurtleID, CanvasId, {0,0}, 0, up);
        {add_new_turtle_ack, {error, Reason}} ->
            io:format("Failed to add turtle to canvas: ~p.~n", [Reason])
    end.


%% Turtle Loop API
turtle_loop(TurtleID, C, {X, Y}, Angle, PenState) -> 
    receive
        {From, forward, N} ->
            CurrentTurtle = {C, {X, Y}, Angle, PenState},
            io:format("Received forward command with N = ~p.~n", [N]), % Check received command
            case N of
                0 ->
                    From ! {ok, CurrentTurtle},
                    turtle_loop(TurtleID,C, {X, Y}, Angle, PenState);
                
                N when N>0 ->
                    io:format("Moving forward with N = ~p.~n", [N]),
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
                                io:format("Error: Invalid angle ~p.~n", [Angle]),
                                {X, Y} % Keeping the same position for invalid angle % Invalid angle, keep the same position
                            end,
                            NewTurtle = {C, NewPosition, Angle, PenState},
                            io:format("New Position calculated as ~p.~n", [NewPosition]),
                            case PenState of 
                                down ->
                                    Result = line_segment(TurtleID, C, {{X,Y}, NewPosition}),
                                    case Result of
                                        ok ->
                                            From ! {TurtleID, ok}, % Send response back to the calling process
                                            turtle_loop(TurtleID, C, NewPosition, Angle, PenState);
                                            {error, Reason} ->
                                                From ! {TurtleID, {error, Reason}},
                                                turtle_loop(TurtleID, C, NewPosition, Angle, PenState)
                                                end;
                                    up ->
                                        io:format("Pen is up, moving without drawing from ~p to ~p.~n", [{X, Y}, NewPosition]),
                                        From ! {ok, NewTurtle},
                                        turtle_loop(TurtleID, C, NewPosition, Angle, PenState)
                                

                        end;
                _->
                    io:format("Error: Negative move value ~p.~n", [N]),
                    {ok, CanvasMap} = get_canvas(C),
                    dead_turtle(CanvasMap, self()),
                    From ! {error, "Negative move value"},
                    {X, Y}
            

            
        end;

        {From, anti, Degree} ->
            io:format("Received anti command with Degree = ~p.~n", [Degree]),
            case lists:member(Degree, [0, 90, 180, 270]) of
                true ->
                    Anti_Angle = (Angle + Degree) rem 360,
                    io:format("Anti angle calculated as ~p.~n", [Anti_Angle]),
                    From ! {TurtleID, ok},
                    io:format("Sent ok response to ~p from TurtleID ~p.~n", [From, TurtleID]),
                    turtle_loop(TurtleID, C, {X, Y}, Anti_Angle, PenState);
                false ->
                    From ! {TurtleID, {error, "Invalid angle"}},
                    turtle_loop(TurtleID, C, {X, Y}, Angle, PenState)
    end;
                    
       
     {From, clock, Degree} ->
        case lists:member(Degree, [0, 90, 180, 270]) of
            true ->
                    New_Angle = Angle - Degree,
                    case New_Angle < 0 of
                        true ->
                            Clock_Angle = New_Angle + 360,
                            turtle_loop(TurtleID,C, {X, Y}, Clock_Angle, PenState);
                        false ->
                            turtle_loop(TurtleID,C, {X, Y}, New_Angle, PenState)
                    end;
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
                turtle_loop(TurtleID,C, {X, Y}, Angle, NewPenState);
            down -> 
                NewTurtle = {C, {X,Y}, Angle, down},
                {reply, ok, NewTurtle},
                turtle_loop(TurtleID,C, {X, Y}, Angle, NewPenState);
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
                    turtle_loop(TurtleID,C, {X, Y}, Angle, PenState);
                false ->
                    {ok, CanvasMap} = get_canvas(C),
                dead_turtle(CanvasMap, self()),
                From ! {error, "Invalid angle"}
            end;
   {From, position, P} -> 
        P = {self(), {ok, {X, Y}}},
        From ! P,
        turtle_loop(TurtleID,C, {X, Y}, Angle, PenState);
    _Other ->
        io:format("Unexpected message received by Turtle: ~p~n", [_Other])

    end.



%% Canvas API

     
% Canvas loop function
canvas_loop(CanvasMap) ->
    receive
        {add_new_turtle, TurtleID} ->
            UpdatedCanvasMap = maps:put(TurtleID, {alive, []}, CanvasMap),
            % Send acknowledgment back to the TurtleID
            TurtleID ! {add_new_turtle_ack, ok},
            canvas_loop(UpdatedCanvasMap);
        
        {add_line_segment, TurtleID, LineSegment} ->
            case maps:find(TurtleID, CanvasMap) of
                {ok, {Status, Segments}} ->
                    UpdatedSegments = Segments ++ [LineSegment],
                    CanvasUpdation = maps:put(TurtleID, {Status, UpdatedSegments}, CanvasMap),
                    TurtleID ! {line_added, ok},
                    io:format("Line segment added to turtle ~p.~n", [TurtleID]),
                    canvas_loop(CanvasUpdation);
                error ->
                    TurtleID ! {error, {turtle_not_found, TurtleID}},
                    io:format("Failed to add line segment: Turtle ~p not found.~n", [TurtleID]),
                    canvas_loop(CanvasMap)
            end;

        {_, blast} ->
            {reply, ok};

        {From, picture} -> 
                LiveTurtlePictures = collect_live_turtle_pictures(CanvasMap),
                CombinedPicture = lists:concat(LiveTurtlePictures),
                io:format("Sending picture to ~p.~n", [From]),
                io:format("Sending picture response from Canvas ~p.~n", [self()]),
                From ! {reply, ok, CombinedPicture},
                canvas_loop(CanvasMap);


        {From, turtles} ->
            {LiveTurtles, DeadTurtles} = count_turtles(CanvasMap),
            From ! {reply, ok, {LiveTurtles, DeadTurtles}},
            canvas_loop(CanvasMap);

        {From, graveyard} ->
            DeadTurtlePictures = collect_dead_turtle_pictures(CanvasMap, []),

            % Concatenate the pictures
            CombinedGraveyardPicture = lists:concat(DeadTurtlePictures),

            From ! {reply, ok, CombinedGraveyardPicture},
            canvas_loop(CanvasMap);
        _Other ->
            io:format("Invalid message received by Canvas: ~p~n", [_Other]),
            canvas_loop(CanvasMap)
    end.
%%%%%%%%%%%
line_segment(TurtleID, CanvasID, LineSegment) ->
    CanvasID ! {add_line_segment,TurtleID, LineSegment},
    receive
        {line_added, ok} ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end.


% Receive calls for Turtle_loop
forward(TurtleID, N) -> 
    TurtleID ! {self(), forward, N},
    receive
        {TurtleID, ok} -> 
            ok;
        {TurtleID, {error, Reason}} -> 
            {error, Reason}
    end.

anti(TurtleID, Degree) -> 
    TurtleID ! {self(), anti, Degree},
    receive
        {TurtleID, ok} -> 
            ok;
        {TurtleID, {error, Reason}} -> 
            {error, Reason}
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
get_canvas(CanvasID) ->
    CanvasID ! {self(), get_canvas},
    receive
        {canvas_map, CanvasMap} ->
            CanvasMap
        % Add handling for potential timeouts or errors
    end.
blast(Canvas) ->
    Canvas ! {self(), blast},
    receive
        ok -> ok;
        {error, Reason} -> {error, Reason}
    end.

picture(CanvasID) -> 
    CanvasID ! {self(), picture},
    receive
        {reply, ok, Picture} -> 
            {ok, Picture};
        {error, Reason} -> 
            {error, Reason}
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
