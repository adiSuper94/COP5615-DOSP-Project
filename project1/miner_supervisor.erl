-module(miner_supervisor).
-import(miner,[findCoin/0]).
-export([start/4]).


start(ZeroCount, WorkerCount, StartNumber, EndNumber) -> 
  io:format("Start:  ZeroCount:: ~p\t WorkerCount:: ~p \n", [ZeroCount, WorkerCount]),
  statistics(runtime),
  WorkerProcessArray = spawnMiner(ZeroCount, WorkerCount, WorkerCount, array:new(), StartNumber, EndNumber),
  collectResult(array:new(0), WorkerCount, WorkerProcessArray).


collectResult(ResultArray, WorkerCount, WorkerProcessArray) ->
  receive
    {MinerId, {done}} ->
      io:format("Miner (~p) did not find coin \n", [MinerId]),
      exit(MinerId, kill),
      AllWorkersTerminated = terminateWorkers(WorkerProcessArray, WorkerCount),
      if 
        AllWorkersTerminated == true ->
          io:format("AllWorkersTerminated \n"),
          ResultArray;
        true -> 
          collectResult(ResultArray, WorkerCount, WorkerProcessArray)
      end;
    {_MinerId, {Result}} ->
      io:format(Result),
      %NewResultArray = array:set(array:size(ResultArray) + 1, Result, ResultArray),
      collectResult(ResultArray, WorkerCount, WorkerProcessArray);
    _X ->
      ResultArray
  end.


spawnMiner(ZeroCount, WorkerCount, WorkerId, WorkerProcessArray, StartNumber, EndNumber) ->
  if
    WorkerId > 0 -> 
      WorkerProcess = spawn(miner, findCoin, []),
      WorkerProcess ! {self(), {WorkerId, ZeroCount, WorkerCount, StartNumber, EndNumber}},
      ProcessName = string:concat("miner_worker", erlang:integer_to_list(WorkerId, 10)),
      register(list_to_atom(ProcessName), WorkerProcess),
      NewWorkerProcessArray = array:set(WorkerId, ProcessName ,WorkerProcessArray),
      spawnMiner(ZeroCount, WorkerCount, WorkerId - 1, NewWorkerProcessArray, StartNumber, EndNumber);
    true -> 
      WorkerProcessArray
  end.


terminateWorkers(WorkerProcessArray, WorkerId) ->
  if 
    WorkerId > 0 ->
      ProcessName = array:get(WorkerId, WorkerProcessArray),
      Process = whereis(list_to_atom(ProcessName)),
      if
        Process /= undefined -> 
          io:fwrite("Main: sending termiante signal to ~p\n", [Process]),
          %exit(Process, kill),
          %terminateWorkers(WorkerProcessArray, WorkerId - 1)
          false;
        true ->
          terminateWorkers(WorkerProcessArray, WorkerId - 1)
      end;
    true -> true 
  end.


