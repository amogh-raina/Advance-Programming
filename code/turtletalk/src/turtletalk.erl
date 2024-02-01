-module(turtletalk).
-export([turtle_init/2, line_segment/3,dead_turtle/2,clone_turtle_map/6, collect_live_turtle_pictures/1,collect_dead_turtle_pictures/1]).
-export([new_canvas/0, blast/1, new_turtle/1, picture/1, turtles/1, graveyard/1]).
-export([forward/2, anti/2, clock/2, setpen/2, clone/2, position/1]).

%-type position() :: {integer(), integer()}.
%-type line_seg() :: {position(), position()}.
%-type picture()  :: [line_seg()].

%%%% Helper Functions %%%%

%% Line Segment Function
line_segment(TurtleID, CanvasID, LineSegment) ->
    CanvasID ! {add_line_segment,TurtleID, LineSegment},
    receive
        {line_added, ok} ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end.
%% Logic of dead turtle
dead_turtle(CanvasID, TurtleID) ->
    CanvasID !{add_dead_turtle, TurtleID},
    ok.

%%% Colleect Live Turtle Pictures
collect_live_turtle_pictures(CanvasMap) ->
    maps:fold(fun(_TurtleID, {alive, TurtleData}, Acc) ->
                    TurtlePictures = TurtleData,
                    [TurtlePictures | Acc];
                  (_, _, Acc) ->
                    Acc
              end, [], CanvasMap).


%% colleect dead turtle pictures
collect_dead_turtle_pictures(CanvasMap) ->
    maps:fold(fun(_TurtleID, {dead, TurtleData}, Acc) ->
                    TurtlePictures = TurtleData,
                    [TurtlePictures | Acc];
                  (_, _, Acc) ->
                    Acc
              end, [], CanvasMap).

%% Clone Turtle Map Function
clone_turtle_map(_C, _Position, _Angle, _PenState, TurtleList, 0) ->
    TurtleList;
clone_turtle_map(C, Position, Angle, PenState, TurtleList, N) when N > 0 ->
    io:format("Cloning turtle, remaining: ~p.~n", [N]),
    T = new_turtle(C),
    io:format("New turtle created: ~p.~n", [T]),
    clone_turtle_map(C, Position, Angle, PenState, [{T, {alive, []}} | TurtleList], N - 1).

% Initialize a canvas process
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
        {From, forward, N} when is_integer(N) ->
            io:format("Received forward command with N = ~p.~n", [N]), % Check received command
            case N of
                0 ->
                    From ! {TurtleID, ok},
                    turtle_loop(TurtleID, C, {X, Y}, Angle, PenState);

                N when N<0 ->
                    io:format("Error: Invalid format of N ~p.~n", [N]),
                    From ! {TurtleID, {error, "Invalid format of N"}},
                    dead_turtle(C, TurtleID),
                    turtle_loop(TurtleID, C, {X, Y}, Angle, PenState);
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
                                dead_turtle(C, TurtleID),
                                From ! {TurtleID, {error, "Invalid format of Angle"}},
                                turtle_loop(TurtleID,C, {X, Y}, Angle, PenState) % Keeping the same position for invalid angle % Invalid angle, keep the same position
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
            _ ->
                From ! {TurtleID, {error, "Invalid movement value"}},
                dead_turtle(C, TurtleID),
                turtle_loop(TurtleID, C, {X, Y}, Angle, PenState)
            

            
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
                    dead_turtle(C, TurtleID),
                    From ! {TurtleID,{error, "Invalid angle"}}
    end;
                    
        {From, clock, Degree} ->
            io:format("Received Clock command with Degree = ~p.~n", [Degree]),
            case lists:member(Degree, [0, 90, 180, 270]) of
                true ->
                    Clock_Angle = abs(Angle - Degree),
                    io:format("Clock angle calculated as ~p.~n", [Clock_Angle]),
                    From ! {TurtleID, ok},
                    io:format("Sent ok response to ~p from TurtleID ~p.~n", [From, TurtleID]),
                    turtle_loop(TurtleID, C, {X, Y}, Clock_Angle, PenState);
                false ->
                    dead_turtle(C, TurtleID),
                    From ! {TurtleID,{error, "Invalid angle"}}
    end;
        
        {From, setpen, NewPenState} ->
            case NewPenState of
                up ->
                    turtle_loop(TurtleID,C, {X, Y}, Angle, NewPenState);
                down ->
                    turtle_loop(TurtleID,C, {X, Y}, Angle, NewPenState);
                _ ->
                    dead_turtle(C, TurtleID),
                    From ! {TurtleID,{error, "Invalid Pen State"}}
    end;

    {From, clone, N} when is_integer(N)->
            case N >= 0 of
                true ->
                    io:format("Starting cloning process for ~p times.~n", [N]),
                    TurtleList = clone_turtle_map(C, {X, Y}, Angle, PenState, [], N),
                   % NewTurtle = {C, {X, Y}, Angle, PenState},
                    ClonedTurtleIDs = [TID || {TID, _} <- TurtleList],
                    io:format("Cloning successful, created Turtles: ~p.~n", [ClonedTurtleIDs]),
                    % Respond with the list of new turtle IDs
                    From ! {ok, ClonedTurtleIDs};
                false ->
                    io:format("Clone command received with invalid N: ~p.~n", [N]),
                    dead_turtle(C, TurtleID),
                    From ! {TurtleID,{error, "Invalid value of N"}}
            end,
            turtle_loop(TurtleID,C, {X, Y}, Angle, PenState);
   {From, position} -> 
        Reply = {ok, {X, Y}},
        From ! Reply, % Send the position back to the requester
        turtle_loop(TurtleID, C, {X, Y}, Angle, PenState);

    {terminate} ->
        io:format("Turtle ~p is terminating.~n", [TurtleID]),
        exit(normal); % Terminate the turtle process
    
    _Other ->
        io:format("Unexpected message received by Turtle: ~p~n", [_Other])

    end.



%% Canvas Loop API

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
        {add_dead_turtle, TurtleID} ->
            io:format("Updating state of Turtle ~p to dead.~n", [TurtleID]),
            io:format("CanvasMap before update: ~p~n", [CanvasMap]),
            UpdatedCanvasMap = maps:map(fun(TID, {Status, Segments}) when TID == TurtleID, Status == alive ->
                {dead, Segments};
           (_, Value) -> Value
        end, CanvasMap),
            io:format("CanvasMap after update: ~p~n", [UpdatedCanvasMap]),
            canvas_loop(UpdatedCanvasMap),
            io:format("Still running??: ~n");


        {From, blast} ->
            maps:foreach(
                fun(TurtleID, _Value) ->
                TurtleID ! {terminate} % Send a terminate message to each turtle
                end, CanvasMap),
            From ! {reply, ok}, % Reply to the sender before terminating
            io:format("Canvas and all turtles are being terminated.~n"),
            exit(normal);

        {From, picture} -> 
            LiveTurtlePictures = collect_live_turtle_pictures(CanvasMap),
            CombinedPicture = lists:concat(LiveTurtlePictures),
            io:format("Sending picture to ~p.~n", [From]),
            io:format("Sending picture response from Canvas ~p.~n", [self()]),
            From ! {reply, ok, CombinedPicture},
            canvas_loop(CanvasMap);


        {From, turtles} ->
            LiveTurtles = [T || {T, {alive, _}} <- maps:to_list(CanvasMap)],
            DeadTurtles = [T || {T, {dead, _}} <- maps:to_list(CanvasMap)],
            DeadTurtleCount = length(DeadTurtles),
            From ! {ok, {LiveTurtles, DeadTurtleCount}},
            canvas_loop(CanvasMap);

        {From, graveyard} ->
            DeadTurtlePictures = collect_dead_turtle_pictures(CanvasMap),
            CombinedGraveyardPicture = lists:concat(DeadTurtlePictures),
            io:format("Sending picture to ~p.~n", [From]),
            io:format("Sending picture response from Canvas ~p.~n", [self()]),

            From ! {reply, ok, CombinedGraveyardPicture},
            canvas_loop(CanvasMap);
        _Other ->
            io:format("Invalid message received by Canvas: ~p~n", [_Other]),
            canvas_loop(CanvasMap)
    end.

%%%%%%%%%%%%%%

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
clock(TurtleID, Degree) -> 
    TurtleID ! {self(), clock, Degree},
    receive
        {TurtleID, ok} -> 
            ok;
        {TurtleID, {error, Reason}} -> 
            {error, Reason}
    end.
setpen(T, P) -> 
    T ! {self(), setpen, P},ok.

clone(T, N) ->
    io:format("Sending clone request to Turtle ~p with N = ~p.~n", [T, N]),
    T ! {self(), clone, N},
    receive
        {ok, TurtleList} -> 
            io:format("Received cloned TurtleList: ~p.~n", [TurtleList]),
            {ok, TurtleList};
        {error, Reason} -> 
            io:format("Received error during cloning: ~p.~n", [Reason]),
            {error, Reason}
    end.

position(T) -> 
    T ! {self(), position},
    receive
        {ok, Position} -> {ok, Position};
        _ -> 
            {error, "Failed to get position"}
    end.

%% Receive calls for Canvas_loop

blast(CanvasID) ->
    CanvasID ! {self(), blast},
    receive
        {reply, ok} -> 
            ok;
        _ -> 
            {error, "Failed to blast canvas"}
    end.

picture(CanvasID) -> 
    CanvasID ! {self(), picture},
    receive
        {reply, ok, Picture} -> 
            {ok, Picture};
        {error, Reason} -> 
            {error, Reason}
    end.
turtles(CanvasID) ->
    CanvasID ! {self(), turtles},
    receive
        {ok,{LA,N}} -> {ok,{LA,N}};
        {error, Reason} -> {error, Reason}
    end.

graveyard(CanvasID) ->
    CanvasID ! {self(), graveyard},
    receive
        {reply, ok, Picture} -> 
            {ok, Picture};
        {error, Reason} -> 
            {error, Reason}
    end.
