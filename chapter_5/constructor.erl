-module(constructor).
-compile(export_all).
-include("records.hrl").

construct_Genotype(SensorName, ActuatorName, HiddenLayerDensities) ->
    construct_Genotype(ffnn, SensorName, ActuatorName, HiddenLayerDensities).
construct_Genotype(FileName, SensorName, ActuatorName, HiddenLayerDensities) ->
    S = create_Sensor(SensorName),
    A = create_Actuator(ActuatorName),
    Output_VL = A#actuator.vl,
    LayerDensities = lists:append(HiddenLayerDensities, [Output_VL]),
    Cx_Id = {cortex, generate_id()},
    Neurons = create_NeuroLayers(Cx_Id, S, A, LayerDensities),
    [Input_Layer | _] = Neurons,
    [Output_Layer | _] = lists:reverse(Neurons),
    FL_NIds = [N#neuron.id || N <- Input_Layer],
    LL_NIds = [N#neuron.id || N <- Output_Layer],
    NIds = [N#neuron.id || <- lists:flatten(Neurons)],
    Sensor = S#sensor{cx_id = Cx_Id, fanout_ids = FL_NIds},
    Actuator = A#actuator{cx_id = Cx_Id, fanin_ids = LL_NIds},
    Cortex = create_Cortex(Cx_Id, [S#sensor.id], [A#actuator.id], NIds),
    Genotype = lists:flatten([Cortex, Sensor, Actuator|Neurons]),
    {ok, File} = file:open(FileName, write),
    lists:foreach(fun(X) -> io:format(File, "~p~n", [X]) end, Genotype),
    file:close(File).

create_Sensor(SensorName) ->
    case SensorName of
        rng ->
            #sensor{id = {sensor, generate_id()}, name=rng, vl=2};
        _ ->
            exit("System does not yet support a sensor by name: ~p ~n", [SensorName])
end.

create_Actuator(ActuatorName) ->
    case ActuatorName of
        pts ->
            #sensor{id = {actuator, generate_id()}, name=pts, vl=1};
        _ ->
            exit("System does not yet support actuator by nane: ~p ~n", [ActuatorName])
end.

create_NeuroLayers(Cx_Id, Sensor, Actuator, LayerDensities) ->
    Input_IdPs = [{ Sensor#sensor.id, Sensor#sensor.vl }],
    Tot_Layers = length(LayerDensities),
    [FL_Neurons | Next_LDs] = LayerDensities,
    NIds = [{neuron, {1, Id}} || Id <- generate_ids(FL_Neurons, [])],
    create_NeuroLayers(Cx_Id, Actuator#actuator.id, 1, Tot_Layers, Input_IdPs, NIds, Next_LDs, []).
create_NeuroLayers(Cx_Id, Actuator_Id, LayerIndex, Tot_Layers, Input_IdPs, NIds, [Next_LD, LDs], Acc) ->
    Output_NIds = [{ neuron, {LayerIndex+1, Id}} || Id <- generate_ids(Next_LDs, [])],
    Layer_Neurons = create_NeuroLayer(Cx_Id, Input_IdPs, NIds, Output_NIds, []),
    Next_InputIdPs = [{ NId, 1 } || NId <- NIds],
    create_NeuroLayers(Cx_Id, Actuator_Id, LayerIndex+1, Tot_Layers, Next_InputIdPs, Output_NIds, LDs, [Layer_Neurons | Acc]);
create_NeuroLayers(Cx_Id, Actuator_Id, LayerIndex, Tot_Layers, Input_IdPs, NIds, [], Acc) ->
    Output_Ids = [Actuator_Id],
    Layer_Neurons = create_NeuroLayer(Cx_Id, Input_IdPs, NIds, Output_Ids, []),
    lists:reverse([Layer_Neurons | Acc]).

create_NeuroLayer(Cx_Id, Input_IdPs, [Id | NIds], Output_Ids, Acc) ->
    Neuron = create_Neuron(Input_IdPs, Id, Cx_Id, Output_Ids),
    create_NeuroLayer(Cx_Id, Input_IdPs, NIds, Output_Ids, [ Neuron | Acc]);
create_NeuroLayer(_Cx_Id, _Input_IdPs, [], _Output_Ids, Acc) ->
    Acc.