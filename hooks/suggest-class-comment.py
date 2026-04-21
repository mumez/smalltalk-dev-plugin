#!/usr/bin/env python3
import sys
import random
import json

sys.stdin.read()  # drain stdin

if random.randint(0, 99) >= 10:
    sys.exit(0)

print(json.dumps({
    "hookSpecificOutput": {
        "hookEventName": "PostToolUse",
        "additionalContext": "💡 Tip: Modified Tonel file detected. Consider running /smalltalk-commenter to add or improve class comments for better documentation."
    }
}))
