-module(miner_server).
-export([startServer/1]).

startServer(ZeroCount) ->
  register(server, self()),
  startServer(ZeroCount, 10_000_000, 0).

startServer(ZeroCount, BatchSize, Start) ->
  receive
    {MinerSupervisorId, {request}} -> 
      MinerSupervisorId ! {self(), {ZeroCount, Start, Start + BatchSize}};
    {_MinerSupervisorId, {coin, Result}} ->
      io:format("~p\n", [Result])
  end,
  startServer(ZeroCount, BatchSize, Start + BatchSize).
