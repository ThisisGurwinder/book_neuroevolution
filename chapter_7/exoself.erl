-module(exoself).
-compile(export_all).
-include("records.hrl").
-record(state, {file_name, genotype, idsNpids, cx_pid, spids, npids, apids, highest_fitness, tot_evaluations, tot_cycles}).
-define(MAX_ATTEMPTS, 50).

map() -> map(ffnn).
map(FileName) ->
    Genotype = genotype:load_from_file(FileName),
    spawn(exoself, prep, [FileName, Genotype]).

prep(FileName, Genotype) ->
    {V1, V2, V3} = now(),
    random:seed(V1, V2, V3),
    IdsNPIds = ets:new(idsNpids, [set, private]),
    Cx = genotype:read(Genotype, cortex),
    Sensor_Ids = Cx#cortex.sensor_ids,
    Actuator_Ids = Cx#cortex.actuator_ids,
    NIds = Cx#cortex.nids,
    ScapePIds = spawn_Scapes(IdsNPIds, Genotype, Sensor_Ids, Actuator_Ids),
    spawn_CerebralUnits(IdsNPIds, cortex, [Cx#cortex.id]),
    spawn_CerebralUnits(IdsNPIds, sensor, Sensor_Ids),
    spawn_CerebralUnits(IdsNPIds, actuator, Actuator_Ids),
    spawn_CerebralUnits(IdsNPIds, neuron, NIds),
    link_Sensors(Genotype, Sensor_Ids, IdsNPIds),
    link_Actuators(Genotype, Actuator_Ids, IdsNPIds),
    link_Neurons(Genotype, NIds, IdsNPIds),
    {SPIds, NPIds, APIds} = link_Cortex(Cx, IdsNPIds),
    Cx_PId = ets:lookup_element(IdsNPIds, Cx#cortex.id, 2),
    loop(FileName, Genotype, IdsNPIds, Cx_PId, SPIds, NPIds, APIds, ScapePIds, 0, 0, 0, 0, 1).

loop(FileName, Genotype, IdsNPIds, Cx_PId, SPIds, NPIds, APIds, ScapePIds, HighestFitness, EvalAcc, CycleAcc, TimeAcc, Attempt) ->
    receive
        {Cx_PId, evaluation_completed, Fitness, Cycles, Time} ->
            {U_HighestFitness, U_Attempt} = case Fitness > HighestFitness of
                true ->
                    [NPId ! {self(), weight_backup} || NPId <- NPIds],
                    {Fitness, 0}
                false ->
                    Perturbed_NPIds = get(perturbed),
                    [NPId ! {self(), weight_restore} || NPId <- Perturbed_NPIds],
                    {HighestFitness, Attempt+1}
            end,
            case U_Attempt >= ?MAX_ATTEMPTS of
                true ->
                    U_CycleAcc = CycleAcc+Cycles,
                    U_TimeAcc = TimeAcc+Time,
                    backup_genotype(FileName, IdsNPIds, Genotype, NIds),
                    terminate_phenotype(Cx_PId, SPIds, NPIds, APIds, ScapePIds),
                    io:format("Cortex:~p finished training. Genotype has been backed up.~n Fitness: ~p~n TotEvaluatons: ~p~n TotalCycles: ~p~n TimeAcc: ~p~n", 
                                [Cortex, U_HighestFitness, EvalAcc, U_CycleAcc, U_TimeAcc]),
                    case whereis(trainer) of
                        undefined ->
                            ok;
                        PId ->
                            PId ! {self(), U_HighestFitness, EvalAcc, U_CycleAcc, U_TimeAcc}
                    end;
                false ->
                    Tot_Neurons = length(NPIds),
                    MP = 1/math:sqrt(Tot_Neurons),
                    Perturbed_NPIds = [NPId || NPId <- NPIds, rand:uniform()<MP],
                    put(perturbed, Perturbed_NPIds),
                    [NPId ! {self(), weight_perturb} || NPId <- Perturbed_NPIds],
                    Cx_PId ! {self(), reactivate},
                    loop(FileName, Genotype, IdsNPIds, Cx_PId, SPIds, NPIds, APIds, ScapePIds, U_HighestFitness, EvalAcc+1, CycleAcc+Cycles, TimeAcc+Time, U_Attempt)
            end
end.

spawn_CerebralUnits(IdsNPIds, CerebralUnitType, [Id|Ids]) ->
    PId = CerebralUnitType:gen(self(), node()),
    ets:insert(IdsNPIds, {Id, PId}),
    ets:insert(IdsNPIds, {PId, Id}),
    spawn_CerebralUnits(IdsNPIds, CerebralUnitType, Ids);
spawn_CerebralUnits(_IdsNPIds, _CerebralUnitType, []) ->
    true.

spawn_Scapes(IdsNPIds, Genotype, Sensor_Ids, Actuator_Ids) ->
    Sensor_Scapes = [(genotype:read(Genotype, Id))#sensor.scape || Id <- Sensor_Ids],
    Actuator_Scapes = [(genotype:read(Genotype, Id))#actuator.scape || Id <- Actuator_Ids],
    Unique_Scapes = Sensor_Scapes++(Actuator_Scapes--Sensor_Scapes),
    SN_Tuples = [{scape:gen(self(), node()), ScapeName} || {private, ScapePIds} <- Unique_Scapes],
    [ets:insert(IdsNPIds, {ScapeName, PId}) || {PId, ScapePIds} <- SN_Tuples],
    [ets:insert(IdsNPIds, {PId, ScapeName}) || {PId, ScapePIds} <- SN_Tuples],
    [PId ! {self(), ScapeName} || {PId, ScapeName} <- SN_Tuples],
    [PId || {PId, _Scape_Name} <- SN_Tuples].

