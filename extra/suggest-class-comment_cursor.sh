#!/bin/bash
# Suggest class comments for modified Tonel files (occasionally)

set -euo pipefail

# Read hook input from stdin
INPUT=$(cat)

# Extract file path from hook input
# Try multiple possible formats for afterFileEdit event
FILE_PATH=$(echo "$INPUT" | jq -r '.file_path // .tool_input.file_path // .path // empty' 2>/dev/null || echo "")

# If no file path found, try to get from tool_input
if [[ -z "$FILE_PATH" ]]; then
  FILE_PATH=$(echo "$INPUT" | jq -r '.tool_input.file_path // empty' 2>/dev/null || echo "")
fi

# If still no file path, check if there's a direct path field
if [[ -z "$FILE_PATH" ]]; then
  FILE_PATH=$(echo "$INPUT" | jq -r 'if type == "string" then . else .path // .file // empty end' 2>/dev/null || echo "")
fi

# Only process .st files
if [[ -z "$FILE_PATH" ]] || [[ ! "$FILE_PATH" =~ \.st$ ]]; then
  exit 0
fi

# Occasional suggestion: 10% probability (adjust as needed)
# Use random number 0-99, trigger if < 10
RANDOM_NUM=$((RANDOM % 100))
if [ $RANDOM_NUM -ge 10 ]; then
  # Don't suggest this time (90% of the time)
  exit 0
fi

# Output suggestion in standard Cursor hook format
cat <<EOF
{
  "continue": true,
  "suppressOutput": false,
  "systemMessage": "💡 Tip: Modified Tonel file detected ($FILE_PATH). Consider running @smalltalk-commenter to add or improve class comments for better documentation."
}
EOF
