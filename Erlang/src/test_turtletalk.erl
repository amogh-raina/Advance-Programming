-module(test_turtletalk).

-export([turtle_cmd/0, turtle_cmds/0, cmds_to_picture/1, prop_no_empty/0]).
-export([test_all/0, test_everything/0]).

-include_lib("eqc/include/eqc.hrl").

turtle_cmd() -> not_implemented.
turtle_cmds() -> not_implemented.
cmds_to_picture(_) -> not_implemented.
prop_no_empty() -> not_implemented.
test_all() -> not_implemented.
test_everything() -> not_implemented.
