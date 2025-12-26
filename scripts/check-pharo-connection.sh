#!/bin/bash
# Check Pharo MCP connection on session start

PHARO_PORT="${PHARO_SIS_PORT:-8086}"

# Display environment information
cat >&2 <<EOF
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
🔧 Smalltalk Development Environment
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

📦 MCP Servers:
   • pharo-interop: Pharo Smalltalk Interop
   • smalltalk-validator: Tonel Validator

🌐 Pharo Connection:
   Expected port: $PHARO_PORT
   Make sure PharoSmalltalkInteropServer is running

💡 Quick Start:
   Run /st:buddy to start your friendly development assistant
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
EOF

# Return success (no JSON needed for display-only hook)
exit 0
