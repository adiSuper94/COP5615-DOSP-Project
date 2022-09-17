-module(miner).
-import(crypto, [hash/2]).
-import(binary, [decode_unsigned/1]).
-import(io_lib, [format/2]).
-export([findCoin/2]).

hash(Str) -> format("~64.16.0b", [decode_unsigned(hash(sha256, Str))]).

findCoin(Str, ZeroCount) -> recFindCoin(Str, ZeroCount, 0).

recFindCoin(Str, ZeroCount, Count) ->
  Prefix = string:concat("aditya.subramani;", Str),
  Coin = string:concat(Prefix, erlang:integer_to_list(Count, 36)),
  CoinHash = hash(Coin),
  IsValidCoin = string:substr(CoinHash, 1, ZeroCount) == lists:flatten(lists:duplicate(ZeroCount, "0")),
  if 
    IsValidCoin == true -> Result = string:concat(string:concat(Coin, " : "), CoinHash);
    IsValidCoin == false -> Result = recFindCoin(Str, ZeroCount, Count + 1)
  end,
  Result.

