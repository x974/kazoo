-ifndef(SMOKE_HRL).

-include_lib("whistle/include/wh_types.hrl").

-define(CHILD_SUP(M), {M, {M, start_link, []}, permanent, 5000, supervisor, [M]}).

-define(SMOKE_HRL, true).
-endif.
