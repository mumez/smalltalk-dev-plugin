#!/bin/bash
# Suggest class comments for modified Tonel files (occasionally)

# Read hook input from stdin
INPUT=$(cat)

# Extract tool name and file path from hook input
# Use jq if available, otherwise fall back to python3, then grep/sed
if command -v jq &>/dev/null; then
  TOOL_NAME=$(echo "$INPUT" | jq -r '.tool_name // empty')
  FILE_PATH=$(echo "$INPUT" | jq -r '.tool_input.file_path // empty')
elif command -v python3 &>/dev/null; then
  TOOL_NAME=$(echo "$INPUT" | python3 -c "import sys,json; d=json.load(sys.stdin); print(d.get('tool_name',''))")
  FILE_PATH=$(echo "$INPUT" | python3 -c "import sys,json; d=json.load(sys.stdin); print(d.get('tool_input',{}).get('file_path',''))")
else
  TOOL_NAME=$(echo "$INPUT" | grep -o '"tool_name"[[:space:]]*:[[:space:]]*"[^"]*"' | sed 's/.*"tool_name"[[:space:]]*:[[:space:]]*"\([^"]*\)".*/\1/')
  FILE_PATH=$(echo "$INPUT" | grep -o '"file_path"[[:space:]]*:[[:space:]]*"[^"]*"' | sed 's/.*"file_path"[[:space:]]*:[[:space:]]*"\([^"]*\)".*/\1/')
fi

# Only process Write/Edit operations on .st files
if [[ "$TOOL_NAME" != "Write" && "$TOOL_NAME" != "Edit" ]]; then
  exit 0
fi

if [[ ! "$FILE_PATH" =~ \.st$ ]]; then
  exit 0
fi

# Occasional suggestion: 10% probability (adjust as needed)
# Use random number 0-99, trigger if < 10
RANDOM_NUM=$((RANDOM % 100))
if [ $RANDOM_NUM -ge 10 ]; then
  # Don't suggest this time (90% of the time)
  exit 0
fi

# Output decision to trigger /smalltalk-commenter
cat <<EOF
{
  "hookSpecificOutput": {
    "hookEventName": "PostToolUse",
    "additionalContext": "💡 Tip: Modified Tonel file detected. Consider running /smalltalk-commenter to add or improve class comments for better documentation."
  }
}
EOF
