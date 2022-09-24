-module(miner_supervisor).
-import(miner,[findCoin/0]).
-export([start/2]).


start(ServerName, WorkerCount) ->
  net_adm:ping(ServerName),
  MinerServerId = rpc:call(ServerName, erlang, whereis, [server]),
  MinerServerId ! {self(), {request}},
  receive
    {ServerId, {ZeroCount, StartNumber, EndNumber}} ->
      io:format("Start:  ZeroCount:: ~p\t WorkerCount:: ~p \n", [ZeroCount, WorkerCount]),
      statistics(runtime),
      statistics(wall_clock),
      WorkerProcessArray = spawnMiner(ZeroCount, WorkerCount, WorkerCount, array:new(), StartNumber, EndNumber),
      collectResult(ServerId, WorkerCount, WorkerProcessArray),
      {_, CPUTime} = statistics(runtime),
      {_, RealTime} = statistics(wall_clock),
      io:format("CPU Time: ~p \t Real Time: ~p \t CPU Utilization: ~p\n", [CPUTime, RealTime, CPUTime/RealTime])
  end,
  start(ServerName, WorkerCount).


collectResult(ServerId, WorkerCount, WorkerProcessArray) ->
  receive
    {MinerId, {done}} ->
      exit(MinerId, kill),
      AllWorkersTerminated = ensureAllWorkersTerminated(WorkerProcessArray, WorkerCount),
      if 
        AllWorkersTerminated == true ->
          io:format("AllWorkersTerminated ~p\n", [self()]);
        true -> 
          collectResult(ServerId, WorkerCount, WorkerProcessArray)
      end;
    {_MinerId, {Result}} ->
      ServerId ! {self(), {coin, Result}},
      terminateWorkers(WorkerProcessArray, WorkerCount);
    _X ->
      ok
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
        Process == undefined ->
          terminateWorkers(WorkerProcessArray, WorkerId - 1);
        true ->
          io:fwrite("Main: sending termiante signal to ~p\n", [Process]),
          exit(Process, kill),
          terminateWorkers(WorkerProcessArray, WorkerId - 1)
      end;
    true -> ok 
  end.

ensureAllWorkersTerminated(WorkerProcessArray, WorkerId) ->
  if 
    WorkerId > 0 ->
      ProcessName = array:get(WorkerId, WorkerProcessArray),
      Process = whereis(list_to_atom(ProcessName)),
      if
        Process /= undefined -> 
          false;
        true ->
          ensureAllWorkersTerminated(WorkerProcessArray, WorkerId - 1)
      end;
    true -> true 
  end.


