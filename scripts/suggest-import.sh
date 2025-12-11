#!/bin/bash
# Suggest import command after .st file changes

FILE_PATH="$1"
PACKAGE_NAME=$(basename "$FILE_PATH" .st | cut -d'.' -f1)
DIR_PATH=$(dirname "$FILE_PATH")

echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "📝 Tonel file modified: $FILE_PATH"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo ""
echo "💡 Suggested commands:"
echo "   /st:import $PACKAGE_NAME $DIR_PATH"
echo ""
echo "Or use MCP tool directly:"
echo "   mcp__smalltalk-interop__import_package: '$PACKAGE_NAME' path: '$DIR_PATH'"
echo ""
echo "After import, run tests:"
echo "   /st:test ${PACKAGE_NAME}Test"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
