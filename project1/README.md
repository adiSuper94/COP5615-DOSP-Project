### Steps to run.

#### Common Steps to follow on both Server and Client Machine
- `cd` to `project1` directory.
- open `erl` shell in this directory with `-name` and `-setcookie` CLI options.
- compile code files
  - `c(miner).`
  - `c(miner_supervisor).`
  - `c(miner_server).`

#### On Server Machine 
Start server with `miner_server:startServer(#RequiredZero)`

#### On Client/Worker Machine
Start a Worker with `miner_supervisor:start(serverName, WorkerProcessCount)`

