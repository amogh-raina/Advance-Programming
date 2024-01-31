-module(turtletalk).
-export([forward/2, anti/2, clock/2, setpen/2, clone/2, position/1]).
-export([create_turtle_clone/4]).
-export([move_turtle_forward/2]).
-export([update_turtle_state/2]).
-export([draw_if_pen_down/3]).
-export([canvas_loop/2]).
-export([get_turtle_state/1]).
-export([new_canvas/0]).
-export([blast/1]).
-export([new_turtle/1]).
-export([turtles/1]).
-export([graveyard/1]).
-export([picture/1]).
-export([start_link/0, init/1, handle_call/3, handle_cast/2, terminate/2]).

%-type position() :: {integer(), integer()}.
%-type line_seg() :: {position(), position()}.
%-type picture()  :: [line_seg()].
-define(REGISTRY, global_registry_process).
-behaviour(gen_server).

% Receive calls for Turtle_loop
forward(T, N) when is_integer(N), N >= 0 ->
    gen_server:call(T, {forward, N}).

%% Function to start the gen_server
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
    
init(InitialState) ->
        {ok, InitialState}.

terminate(Reason, _State) ->
    %% Handle any cleanup needed when the server terminates.
    %% Reason is why the server is terminating.
    io:format("Server terminating due to: ~p~n", [Reason]),
    ok.

%% Turtle Process Handling
handle_call({forward, N}, _From, State = #{position := {X, Y}, angle := Angle}) ->
    NewPosition = case Angle of
        0 -> {X + N, Y};      % East
        90 -> {X, Y + N};     % North
        180 -> {X - N, Y};    % West
        270 -> {X, Y - N}     % South
        %% Add more cases if you support more angles
    end,
    NewState = State#{position => NewPosition},
    {reply, ok, NewState}; % Reply with 'ok' and update the state
handle_call({forward, _N}, _From, State) ->
    {reply, {error, invalid_input}, State};
handle_call({rotate, Degrees}, _From, State = #{angle := Angle}) ->
    NewAngle = (360 + Angle + Degrees) rem 360, % Ensure positive result
    NewState = State#{angle => NewAngle},
    {reply, ok, NewState}; % Reply with 'ok' and update the state
handle_call({rotate, _Degrees}, _From, State) ->
    {reply, {error, invalid_input}, State};
handle_call({set_pen, PenState}, _From, State) when PenState == up; PenState == down ->
    NewState = State#{pen => PenState},
    {reply, ok, NewState};
handle_call({set_pen, _PenState}, _From, State) ->
    {reply, {error, invalid_input}, State};
handle_call({clone, N}, _From, State = #{canvas := CanvasID, position := Pos, angle := Angle, pen := Pen}) ->
    CloneIDs = lists:map(fun(_) -> create_turtle_clone(CanvasID, Pos, Angle, Pen) end, lists:seq(1, N)),
    {reply, {ok, CloneIDs}, State};
handle_call({clone, _N}, _From, State) ->
    {reply, {error, invalid_input}, State};
handle_call(get_position, _From, State = #{position := Position}) ->
        {reply, {ok, Position}, State};
handle_call(get_turtles, _From, State = #{turtles := Turtles}) ->
    LiveTurtles = [Turtle || Turtle <- Turtles, is_process_alive(Turtle)],
    DeadTurtlesCount = length(Turtles) - length(LiveTurtles),
    {reply, {ok, {LiveTurtles, DeadTurtlesCount}}, State};
handle_call(get_graveyard, _From, State = #{dead_pictures := DeadPictures}) ->
    {reply, {ok, DeadPictures}, State};
handle_call({get_state, _TurtleID}, _From, State) ->
    {reply, {state_response, State}, State}.
    
handle_cast(blast, State = #{turtles := Turtles}) ->
    lists:foreach(fun(TurtlePid) -> gen_server:cast(TurtlePid, stop) end, Turtles),
    {stop, normal, State}; % Stop the canvas process after sending stop messages to all turtles.
        
handle_cast({add_turtle, TurtlePid}, State = #{turtles := Turtles}) ->
    NewTurtles = [TurtlePid | Turtles],
        {noreply, State#{turtles => NewTurtles}}.

create_turtle_clone(CanvasID, Position, Angle, Pen) ->
    %% Assuming a function exists to start a new turtle process on the canvas with given initial state
    {ok, TurtleID} = turtletalk:new_turtle(CanvasID, #{position => Position, angle => Angle, pen => Pen}),
    TurtleID.

get_turtle_state(T) ->
    T ! {self(), {get_state, T}},
    receive
        {T, State} ->
            {ok, State}
    end.

move_turtle_forward(State, N) ->
    #{position := {X, Y}, angle := Angle} = State,
    NewPosition = case Angle of
        0   -> {X + N, Y};     % East
        90  -> {X, Y + N};     % North
        180 -> {X - N, Y};     % West
        270 -> {X, Y - N}      % South
    end,
    State#{position => NewPosition}.

update_turtle_state(TurtleID, NewState) ->
    % Send a message to the turtle process to update its state
    turtle_process ! {update_state, TurtleID, NewState}, ok.

draw_if_pen_down(TurtleID, OldState, NewState) ->
    OldPosition = maps:get(position, OldState),
    NewPosition = maps:get(position, NewState),
    PenState = maps:get(pen, NewState),
    
    case PenState of
        down ->
            % Draw a line segment on the canvas
            CanvasID = get_canvas_id(TurtleID),
            if
                is_tuple(OldPosition) andalso tuple_size(OldPosition) == 2 andalso
                is_tuple(NewPosition) andalso tuple_size(NewPosition) == 2 ->
                % Both OldPosition and NewPosition have the expected types and sizes
                line_segment = {OldPosition, NewPosition};
            true ->
                % Handle the case where types or sizes don't match
                % You can log an error or take appropriate action here
                % For example:
                io:format("Error: OldPosition and NewPosition should be tuples of size 2.~n")
            end,
            canvas_process ! {draw_line, CanvasID, line_segment},
            ok;
        up ->
            % If the pen is up, do nothing
            ok
    end.
    
get_canvas_id(TurtleID) ->
    % Retrieve the canvas ID associated with the turtle
    % This could be a lookup in a process dictionary, a message to the turtle process, etc.
    case get_turtle_canvas_mapping(TurtleID) of
        {ok, CanvasID} ->
            CanvasID;
        error ->
            % Handle the error (e.g., turtle not found, no associated canvas)
            undefined
    end.

get_turtle_canvas_mapping(TurtleID) ->
    % Send a message to the registry process to get the canvas ID for the given turtle
    ?REGISTRY ! {self(), {get_canvas_id, TurtleID}},
    % Wait for the response
    receive
        {turtle_canvas_id, TurtleID, CanvasID} ->
            {ok, CanvasID};
        {error, Reason} ->
            {error, Reason}
    after 5000 -> % Timeout
        {error, timeout}
    end.

anti(T, D) when D == 0; D == 90; D == 180; D == 270 ->
    gen_server:call(T, {rotate, -D}). % Negative for anticlockwise

clock(T, D) when D == 0; D == 90; D == 180; D == 270 ->
    gen_server:call(T, {rotate, D}). % Positive for clockwise

setpen(T, P) when is_atom(P), P == up; P == down -> 
    gen_server:call(T, {set_pen, P}).

clone(T, N) when is_integer(N), N >= 0 ->
    gen_server:call(T, {clone, N}, 5000). % Timeout of 5000ms to allow for clone creation

position(T) ->
    gen_server:call(T, get_position).

new_canvas() ->
    CanvasPid = spawn_link(fun canvas_loop/0),
    {ok, CanvasPid}.

canvas_loop() -> % Initialize canvas loop with empty lists
canvas_loop([], []).

canvas_loop(Turtles, Pictures) ->
    receive
        {new_turtle, TurtlePid} ->
            %% Add the new turtle PID to the list of live turtles
            NewTurtles = [TurtlePid | Turtles],
            canvas_loop(NewTurtles, Pictures);

        {draw_line, TurtlePid, LineSegment} ->
            %% Directly check if TurtlePid is in Turtles list within the clause
            NewPictures = case lists:member(TurtlePid, Turtles) of
                true ->
                    [LineSegment | Pictures]; % TurtlePid is in Turtles list, update Pictures
                false ->
                    Pictures % TurtlePid not in Turtles list, do not update Pictures
            end,
            canvas_loop(Turtles, NewPictures);
            

        {turtle_dead, TurtlePid, DeadTurtlePicture} ->
            %% Remove the turtle PID from the list of live turtles
            %% and add its final picture to DeadPictures
            NewTurtles = lists:delete(TurtlePid, Turtles),
            NewDeadPictures = DeadTurtlePicture ++ Pictures, % Assuming DeadTurtlePicture is a list
            canvas_loop(NewTurtles, NewDeadPictures);

        {get_picture, Requester} ->
            %% Send the current picture to the requester
            Requester ! {current_picture, Pictures},
            canvas_loop(Turtles, Pictures);

        {get_turtles, Requester} ->
            LiveTurtlesCount = length(Turtles),
            Requester ! {turtle_info, LiveTurtlesCount, length(Pictures)}, % Simplified for example
            canvas_loop(Turtles, Pictures);

        {get_graveyard, Requester} ->
            %% Assuming DeadPictures accumulates pictures from dead turtles
            Requester ! {graveyard_picture, Pictures},
            canvas_loop(Turtles, Pictures);

        blast ->
            %% Stop all turtles
            lists:foreach(fun(TurtlePid) -> gen_server:cast(TurtlePid, stop) end, Turtles),
            %% Optionally, clear Pictures and Turtles lists or stop the canvas process
            ok;

        _Other ->
            %% Handle unknown messages or log them
            canvas_loop(Turtles, Pictures)
    end.

blast(CanvasPid) ->
    gen_server:cast(CanvasPid, blast),
    ok.
    
new_turtle(CanvasPid) ->
    InitialState = #{position => {0, 0}, angle => 0, pen => up, canvas => CanvasPid},
    {ok, TurtlePid} = start_link(),
    gen_server:cast(CanvasPid, {add_turtle, TurtlePid}),
    {ok, TurtlePid}.

picture(CanvasPid) ->
    gen_server:call(CanvasPid, get_picture).

turtles(CanvasPid) ->
    gen_server:call(CanvasPid, get_turtles, 5000). % Timeout after 5000ms

graveyard(CanvasPid) ->
    gen_server:call(CanvasPid, get_graveyard, 5000). % Timeout after 5000ms