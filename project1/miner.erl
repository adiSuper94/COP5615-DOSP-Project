-module(miner).
-import(crypto, [hash/2]).
-import(binary, [decode_unsigned/1]).
-import(io_lib, [format/2]).
-export([test/1]).

test(Str) -> format("~64.16.0b", [decode_unsigned(hash(sha256, Str))]).
