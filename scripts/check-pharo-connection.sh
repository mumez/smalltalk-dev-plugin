#!/bin/bash
# Session start message for Smalltalk development

# Display quick start hint
cat <<EOF
{
  "hookSpecificOutput": {
    "hookEventName": "SessionStart",
    "additionalContext": "💡 Ready for Smalltalk development! Run /st:buddy to start your friendly development assistant."
  }
}
EOF
exit 0
