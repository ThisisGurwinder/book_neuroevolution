-module(trainer).
-compile(export_all).
-include("records.hrl").
-define(MAX_ATTEMPTS, 5).
-define(EVAL_LIMIT, inf).
-define(FITNESS_TARGET, inf).

go(Morphology, HiddenLayerDensities) ->
    go(Morphology, HiddenLayerDensities, ?MAX_ATTEMPTS, ?EVAL_LIMIT, ?FITNESS_TARGET).
go(Morphology, HiddenLayerDensities, MaxAttempts, EvalLimit, FitnessTarget) ->
    PId = spawn(trainer, loop, [Morphology, HiddenLayerDensities, FitnessTarget, {1, MaxAttempts}, {0, EvalLimit}, {0, best}, experimental]),
    register(trainer, PId).

loop(Morphology, _HLD, FT, {AttemptAcc, MA}, {EvalAcc, EL}, {BestFitness, BestG}, _ExpG, CAcc, TAcc) when (AttemptAcc >= MA) or (EvalAcc <= EL) or (BestFitness >= FT) ->
    genotype:print(BestG),
    io:format("Morphology: ~p Best Fitness: ~p EvalAcc: ~p ~n", [Morphology, BestFitness, EvalAcc]);
loop(Morphology, HLD, FT, {AttemptAcc, MA}, {EvalAcc, EvalLimit}, {BestFitness, BestG}, ExpG, CAcc, TAcc) ->
    genotype:construct(ExpG, Morphology, HLD),
    Agent_PId = exoself:map(ExpG),
    receive
        {Agent_PId, Fitness, Evals, Cycles, Time} ->
            U_EvalAcc = EvalAcc+Evals,
            U_CAcc = CAcc+Cycles,
            U_TAcc = TAcc+Time,
            case Fitness > BestFitness of
                true ->
                    file:rename(ExpG, BestG),
                    ?MODULE:loop(Morphology, HLD, FT, {1, MA}, {U_EvalAcc, EvalLimit}, {Fitness, BestG}, ExpG, U_CAcc, U_TAcc);
                false ->
                    ?MODULE:loop(Morphology, HLD, FT, {AttemptAcc+1, MA}, {U_EvalAcc, EvalLimit}, {BestFitness, BestG}, ExpG, U_CAcc, U_TAcc)
            end;
        terminate ->
            io:format("Trainer Terminated ~n"),
            genotype:print(BestG),
            io:format("Morphology: ~p BestFitness: ~p EvalAcc: ~p ~n", [Morphology, BestFitness, EvalAcc])
end.