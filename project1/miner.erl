-module(miner).
-import(crypto, [hash/2]).
-import(binary, [decode_unsigned/1]).
-import(io_lib, [format/2]).
-export([findCoin/0]).

hash(Str) -> format("~64.16.0b", [decode_unsigned(hash(sha256, Str))]).

findCoin() ->
  receive
    {MinerSupervisorId, {Id, ZeroCount, WorkerCount, StartNumber, EndNumber}} -> 
      io:format("Worker(~p): Received Message: ~pfrom: ~p \n ", [self(), {Id, ZeroCount, WorkerCount, StartNumber, EndNumber}, MinerSupervisorId]),
      recFindCoin(MinerSupervisorId, ZeroCount, WorkerCount, StartNumber + Id, EndNumber)
  end.

recFindCoin(MinerSupervisorId, ZeroCount, WorkerCount, Counter, EndNumber) ->
  Prefix = "aditya.subramani;",
  Coin = string:concat(Prefix, lists:flatten(to_base_string(Counter, 64))),
  CoinHash = hash(Coin),
  IsValidCoin = string:substr(CoinHash, 1, ZeroCount) == lists:flatten(lists:duplicate(ZeroCount, "0")),
  if
    Counter >= EndNumber + WorkerCount ->
      MinerSupervisorId ! {self(), {done}};
    true -> 
      if
        IsValidCoin == true ->
          Result = string:concat(string:concat(Coin, " : "), CoinHash),
          MinerSupervisorId ! {self(), {Result}};
        true -> 
          recFindCoin(MinerSupervisorId, ZeroCount, WorkerCount, Counter + WorkerCount, EndNumber)
      end
  end.

  
% From https://gist.github.com/Fabsolute/7e3a442e5f01bcbf32f5843bb8948525
get_base_char(Index, Lookup) ->
  [lists:nth(Index + 1, Lookup)].

to_base_string(Number, Base) when is_integer(Number), is_integer(Base), Base =< 64 ->
  Lookup = [
    "0",
    "1",
    "2",
    "3",
    "4",
    "5",
    "6",
    "7",
    "8",
    "9",
    "a",
    "b",
    "c",
    "d",
    "e",
    "f",
    "g",
    "h",
    "i",
    "j",
    "k",
    "l",
    "m",
    "n",
    "o",
    "p",
    "q",
    "r",
    "s",
    "t",
    "u",
    "v",
    "w",
    "x",
    "y",
    "z",
    "A",
    "B",
    "C",
    "D",
    "E",
    "F",
    "G",
    "H",
    "I",
    "J",
    "K",
    "L",
    "M",
    "N",
    "O",
    "P",
    "Q",
    "R",
    "S",
    "T",
    "U",
    "V",
    "W",
    "X",
    "Y",
    "Z",
    "-",
    "_"
  ],
  to_base_string(Number, Base, Lookup).

to_base_string(Number, Base, Lookup) when is_integer(Number), is_integer(Base), Base > 1 ->
  case Number < Base of
    true ->
      get_base_char(Number, Lookup);
    false ->
      to_base_string(Number div Base, Base, Lookup) ++ get_base_char(Number rem Base, Lookup)
  end.
