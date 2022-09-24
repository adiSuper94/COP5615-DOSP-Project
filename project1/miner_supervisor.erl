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
      WorkerProcessArray = spawnMiner(ZeroCount, WorkerCount, WorkerCount, array:new(), StartNumber, EndNumber),
      collectResult(ServerId, WorkerCount, WorkerProcessArray)
  end,
  start(ServerName, WorkerCount).


collectResult(ServerId, WorkerCount, WorkerProcessArray) ->
  receive
    {MinerId, {done}} ->
      exit(MinerId, kill),
      AllWorkersTerminated = terminateWorkers(WorkerProcessArray, WorkerCount),
      if 
        AllWorkersTerminated == true ->
          io:format("AllWorkersTerminated ~p\n", [self()]);
        true -> 
          collectResult(ServerId, WorkerCount, WorkerProcessArray)
      end;
    {_MinerId, {Result}} ->
      ServerId ! {self(), {coin, Result}},
      collectResult(ServerId, WorkerCount, WorkerProcessArray);
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
        Process /= undefined -> 
          false;
        true ->
          terminateWorkers(WorkerProcessArray, WorkerId - 1)
      end;
    true -> true 
  end.


