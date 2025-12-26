#!/bin/bash
# Check Pharo MCP connection on session start

PHARO_PORT="${PHARO_SIS_PORT:-8086}"

# Try to connect to Pharo
if command -v nc >/dev/null 2>&1; then
  # Use netcat to check if port is open
  if ! nc -z -w1 localhost "$PHARO_PORT" 2>/dev/null; then
    # Connection failed - output notification
    cat <<EOF
{
  "hookSpecificOutput": {
    "hookEventName": "SessionStart",
    "additionalContext": "⚠️ Pharo connection check failed on port $PHARO_PORT. Make sure PharoSmalltalkInteropServer is running.\n\nTo start the server in Pharo:\n  SisServer current start.\n\nThen run /st:buddy to begin development."
  }
}
EOF
    exit 0
  fi
fi

# Connection successful or nc not available - show quick start hint
cat <<EOF
{
  "hookSpecificOutput": {
    "hookEventName": "SessionStart",
    "additionalContext": "💡 Ready for Smalltalk development! Run /st:buddy to start your friendly development assistant."
  }
}
EOF
exit 0
