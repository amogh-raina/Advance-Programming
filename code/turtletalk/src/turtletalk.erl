-module(turtletalk).
-export([init/1,handle_call/3,handle_cast/2]).
-export([new_canvas/0, blast/1, new_turtle/1, picture/1, turtles/1, graveyard/1]).
-export([forward/2, anti/2, clock/2, setpen/2, clone/2, position/1,line_segment/3]).

%-type position() :: {integer(), integer()}.
%-type line_seg() :: {position(), position()}.
%-type picture()  :: [line_seg()].

%% Turtle API

forward(T, N) -> gen_server:call(T, {forward,T, N}, infinity).


line_segment(CanvasMap, T, {PositionA, PositionB}) ->
    {DoA,Segment} = maps:get(T,  CanvasMap),
    UpdateSegment = Segment ++ [{PositionA, PositionB}],
    NewTurtles = maps:put(T, {DoA, UpdateSegment}, CanvasMap),
    gen_server:cast(CanvasMap, {segment_history, NewTurtles}).

anti(T, D) ->
    case lists:member(D, [0, 90, 180, 270]) of
        true ->
            gen_server:call(T, {anti, D});
        false ->
            {error, "Invalid angle"}
    end.

clock(T, D) ->
    case lists:member(D, [0, 90, 180, 270]) of
        true ->
            gen_server:call(T, {clock, D});
        false ->
            {error, "Invalid angle"}
    end.

setpen(T, P) ->
    case lists:member(P, [up, down]) of
        true ->
            gen_server:call(T, {setpen, P});
        false ->
            {error, "Invalid pen state"}
    end.

clone(T, N) ->
        case N >= 0 of
            true ->
                gen_server:call(T, {clone, N});
            false ->
                {error, "N must be greater than or equal to zero"}
        end.
    
    % handle call using new_turtle .
    position(T) ->
        case gen_server:call(T, get_position) of
            {ok, Position} ->
                {ok, Position};
            {error, Reason} ->
                {error, Reason}
        end.

%% Canvas API

new_canvas() -> gen_server:start_link(?MODULE, new_canvas, []).
blast(_) -> not_implemented.
new_turtle(C) -> 
    T = gen_server:start_link(?MODULE, {new_turtle,{C,{0,0},0,up}}, []),
    gen_server:call(C, {add_new_turtle, C, T}).
picture(_) -> not_implemented.
turtles(_) -> not_implemented.
graveyard(_) -> not_implemented.


init(State) ->
    case State of
        new_canvas ->
            {ok, #{}};
        new_turtle ->
            {ok, State}
    end.
%Get the current state of the canvas i.e is the map of turtles
% handle_cast operations
handle_cast({segment_history, NewTurtles}, _) ->
    {noreply, NewTurtles}.


% Turtle Operations
handle_call({anti, D}, _From, {C, {X,Y}, A, Ps}) ->
    Anti_Angle = A + D, %% check for clockwise or anti-clockwise??
    NewTurtle = {C, {X,Y}, Anti_Angle, Ps},
    {reply, ok, NewTurtle};
handle_call({clock, D}, _From, {C, {X,Y}, A, Ps}) ->
    Clock_Angle = A - D, %% check for clockwise or anti-clockwise?? %% Make transformation too for negativity
    NewTurtle = {C, {X,Y}, Clock_Angle, Ps},
    {reply, ok, NewTurtle};
handle_call({setpen, P}, _From, {C, {X,Y}, A, _Ps}) ->
    NewTurtle = {C, {X,Y}, A, P},
    {reply, ok, NewTurtle};
handle_call({clone, N}, _From, MoveTurtle) ->
    {C, {X, Y}, A, Ps} = MoveTurtle,
    NewTurtles = lists:map(
        fun(_) ->
            NewTurtle = {C, {X, Y}, A, Ps},
            {ok, NewTurtle}
        end,
        lists:seq(1, N)
    ),
    {reply, {ok, NewTurtles}, MoveTurtle};

%handle_call(get_position, _From, {_, Position, _, _}) ->
    %{reply, {ok, Position}, {_, Position, _, _}};


handle_call({add_new_turtle, _C, T}, _From, CanvasMap) ->
    NewTurtles = maps:put(T, {alive, []}, CanvasMap),
    {reply, {ok, T}, NewTurtles};

handle_call({forward, T, N}, _From, MoveTurtle) ->
    {C, {X,Y}, A, Ps} = MoveTurtle,
    if N > 0 ->
        case A of
            0 ->
                Pos_new = {(X + N), Y},
                NewTurtle = {C, Pos_new, A, Ps},
                if Ps == down ->
                    line_segment(C, T, {{X,Y}, Pos_new});
                    Ps == up ->
                        nothing end,
                        {reply,ok, NewTurtle};
                                        
            90 ->
                Pos_new = {X, (Y + N)},
                NewTurtle = {C, Pos_new, A, Ps},
                if Ps == down ->
                    line_segment(C, T, {{X,Y}, Pos_new});
                    Ps == up ->
                        nothing end,
                        {reply,ok, NewTurtle};
            180 ->
                Pos_new = {(X - N), Y},
                NewTurtle = {C, Pos_new, A, Ps},
                if Ps == down ->
                    line_segment(C, T, {{X,Y}, Pos_new});
                    Ps == up ->
                        nothing end,
                        {reply,ok, NewTurtle};
            270 ->
                Pos_new = { X, (Y - N)},
                NewTurtle = {C, Pos_new, A, Ps},
                if Ps == down ->
                    line_segment(C, T, {{X,Y}, Pos_new});
                    Ps == up ->
                    ok end,
                        {reply,ok, NewTurtle}
        end;
        N==0 ->
            {reply, ok, MoveTurtle};
        N<0 ->
            {reply, error, "N must be greater than 0"}
            

    end.

    
% Canvas Operations
