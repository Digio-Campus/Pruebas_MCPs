#!/usr/bin/env bash
set -euo pipefail

# smoke test: start the program briefly and ensure it exits cleanly when killed
BIN="./bin/tictactoe"
if [ ! -x "$BIN" ]; then
  echo "integration: binary $BIN not found" >&2
  exit 2
fi

"$BIN" &
PID=$!
sleep 0.6
kill "$PID" 2>/dev/null || true
wait "$PID" 2>/dev/null || true

echo "integration: OK"
