-module(tweeter_server).
-export([initServer/1]).

initServer(LoggedInUsers) -> 
  receive
    {UserProcessId, {register, Handle}} -> 
      NewLoggedInUsers = registerUser(UserProcessId, Handle, LoggedInUsers);
    {UserProcessId,{login, Handle}} -> 
      NewLoggedInUsers = loginUser(UserProcessId, Handle, LoggedInUsers);
    {UserProcessId, {tweet, Message}} ->
      NewLoggedInUsers = LoggedInUsers,
      tweet(UserProcessId, Message, LoggedInUsers);
    {UserProcessId, {getAllTweets}} -> 
      NewLoggedInUsers = LoggedInUsers,
      getAllTweets(UserProcessId, LoggedInUsers);
    {UserProcessId, {follow, Handle}} ->
        NewLoggedInUsers = LoggedInUsers,
        followUser(UserProcessId, Handle, LoggedInUsers)
  end,
  initServer(NewLoggedInUsers).

registerUser(UserProcessId, Handle, LoggedInUsers) -> 
    pgInsert("INSERT INTO tweeters (handle) VALUES ($1) RETURNING *", [Handle]),
    loginUser(UserProcessId, Handle, LoggedInUsers).

loginUser(UserProcessId, Handle, LoggedInUsers) ->
  _Rows = pgSelect("SELECT * FROM tweeters WHERE handle = $1", [Handle]),
  NewLoggedInUsers = maps:put(UserProcessId, Handle ,LoggedInUsers),
  NewLoggedInUsers.

tweet(UserProcessId, Message, LoggedInUsers) -> 
    Handle = maps:get(UserProcessId, LoggedInUsers),
    pgInsert("INSERT INTO tweets(by, tweet) VALUES ($1, $2) RETURNING *", [Handle, Message]),
    notifyLoggedInUsers(LoggedInUsers, Message, Handle).

getAllTweets(UserProcessId, LoggedInUsers) ->
    Handle = maps:get(UserProcessId, LoggedInUsers),
    Followers = pgSelect("SELECT ee FROM followers WHERE er = $1", [Handle]),
    Rows = pgSelect("SELECT * FROM tweets WHERE by in $1", [Followers]),
    Rows.

followUser(UserProcessId, Handle, LoggedInUsers) -> 
    ER = maps:get(UserProcessId, LoggedInUsers),
    pgInsert("INSERT INTO followers(ee, er) VALUES ($1, $2) RETURNING *", [Handle, ER]).

notifyLoggedInUsers(_LoggedInUsers, _Message, Tweeter) ->
    _Followers = pgSelect("SELECT er from followers WHERE ee = $1", [Tweeter]),
    io:format("notify").

pgSelect(Query, Params) ->
  {ok, C} = epgsql:connect(#{ host => "localhost", username => "postgres", password => "postgres", database => "tweeter_db", port => 5432, timeout => 4000}),
  {ok, _Columns, Rows} = epgsql:equery(Query, Params),
  ok = epgsql:close(C),
  Rows.

pgInsert(Query, Params) ->
  {ok, C} = epgsql:connect(#{ host => "localhost", username => "postgres", password => "postgres", database => "tweeter_db", port => 5432, timeout => 4000}),
  {ok, _Count, _Coulumns, _Rows} = epgsql:equery(Query, Params),
  ok = epgsql:close(C).
