-module(sensor).
-compile(export_all).
-inlucde("records.hrl").

gen(ExoSelf_PId, Node) ->
    spawn(Node, ?MODULE, loop, [ExoSelf_PId]).

loop(ExoSelf_PId) ->
    receive
        {ExoSelf_PId, {Id, Cx_PId, SensorName, VL, Fanout_PIds}} ->
            loop(Id, Cx_PId, SensorName, VL, Fanout_PIds)
end.

loop(Id, Cx_PId, SensorName, VL, Fanout_PIds) ->
    receive
        {Cx_PId, sync} ->
            SensoryVector = sensor:SensorName(VL),
            [PId ! {self(), forward, SensoryVector} || PId <- Fanout_PIds],
            loop(Id, Cx_PId, SensorName, VL, Fanout_PIds);
        {Cx_PId, terminate} ->
            ok
end.

rng(VL) ->
    rng(VL, []).
rng(0, Acc) ->
    Acc;
rng(VL, Acc) ->
    rng(VL-1, [rand:uniform() | Acc]).