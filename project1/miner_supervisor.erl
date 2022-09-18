-module(miner_supervisor).
-import(miner,[findCoin/0]).
-export([start/2]).


start(ZeroCount, WorkerCount) -> 
  io:format("Start:  ZeroCount:: ~p\t WorkerCount:: ~p \n", [ZeroCount, WorkerCount]),
  WorkerProcessArray = spawnMiner(ZeroCount, WorkerCount, WorkerCount, array:new()),
  receive
    X ->
      io:format("\nTerminating all workers :: ~p\n", [WorkerCount]),
      terminateWorkers(WorkerProcessArray, WorkerCount),
      io:format("\nTerminated all workers :: ~p\n", [WorkerCount]),
      X 
  end.

spawnMiner(ZeroCount, WorkerCount, WorkerId, WorkerProcessArray) ->
  if
    WorkerId > 0 -> 
      WorkerProcess = spawn(miner, findCoin, []),
      WorkerProcess ! {self(), {WorkerId, ZeroCount, WorkerCount}},
      ProcessName = string:concat("miner_worker", erlang:integer_to_list(WorkerId, 10)),
      register(list_to_atom(ProcessName), WorkerProcess),
      NewWorkerProcessArray = array:set(WorkerId, ProcessName ,WorkerProcessArray),
      spawnMiner(ZeroCount, WorkerCount, WorkerId - 1, NewWorkerProcessArray);
    true -> 
      WorkerProcessArray
  end.


terminateWorkers(WorkerProcessArray, WorkerId) ->
  if 
    WorkerId > 0 ->
      ProcessName = array:get(WorkerId, WorkerProcessArray),
      Process = whereis(list_to_atom(ProcessName)),
      if
        Process == undefined ->
          terminateWorkers(WorkerProcessArray, WorkerId - 1);
        true ->
          io:fwrite("Main: sending termiante signal to ~p\n", [Process]),
          exit(Process, kill),
          terminateWorkers(WorkerProcessArray, WorkerId - 1)
      end;
    true -> ok 
  end.
