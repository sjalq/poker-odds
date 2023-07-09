#!/bin/bash

# Start your process
dotnet run -- "$@" &

# Get its PID
POKER_ODDS_PID=$!

echo "Process ID for poker-odds is: $POKER_ODDS_PID"

# Use the PID in the trace command
dotnet trace collect --format speedscope -p $POKER_ODDS_PID -o traces/trace_$POKER_ODDS_PID
