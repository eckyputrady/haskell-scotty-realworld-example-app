#!/bin/bash
kill -9 `pgrep realworld-exe`
stack exec realworld-exe &
sleep 0.6
echo -e "server running with PID={`pgrep realworld-exe`} (if that's empty, then the server is not running)"