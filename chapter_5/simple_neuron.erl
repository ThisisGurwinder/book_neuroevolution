-module(simple_neuron).
-compile(export_all).

create() ->
    Weights = [rand:uniform()-0.5, rand:uniform()-0.5, rand:uniform()-0.5 ],
    register(neuron, spawn(?MODULE, loop, [Weights])).
% The create function spawns a single neuron, where bias and weights are generated rand between 0.5 to -0.5

loop(Weights) ->
    receive
        {From, Input} ->
            io:format("**** Processing **** ~n Input: ~p~n Using Weights: ~p ~n", [Input, Weights]),
            Dot_Product = dot(Input, Weights, 0),
            Output = [math:tanh(Dot_Product)],
            From ! {result, Output},
            loop(Weights)
end.
% The spawned process accepts an Input vector, print it and the weight vector to the screen, calculates the output,
% and then send the calculated part to the contacting process. The output is also a vector of one.

dot([I|Input], [W|Weights], Acc) ->
    dot(Input, Weights, I*W+Acc);
dot([], [Bias], Acc) ->
    Acc+Bias.
% The dot product function that we use works on the assumption that the bias is incorporated into the weight list as
% the last value into lists of weights.After calculating, Input will be empty and Weights list will still have Bias
% value remaining, which we than add to accumulator.

sense(Signal) ->
    case is_list(Signal) and (length(Signal) == 2) of
        true ->
            neuron ! {self(), Signal},
            receive
                {result, Output} ->
                    io:format(" Output: ~p~n", [Output])
            end;
        false ->
            io:format("The signal must be of length 2 ~n")
end.
% We use sense function to contact to neuron and send it to input vector.