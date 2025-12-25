#!/bin/bash
# Check Pharo MCP connection on session start

PHARO_PORT="${PHARO_SIS_PORT:-8086}"

echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "🔧 Smalltalk Development Environment"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo ""
echo "📦 MCP Servers:"
echo "   • pharo-interop: Pharo Smalltalk Interop"
echo "   • smalltalk-validator: Tonel Validator"
echo ""
echo "🌐 Pharo Connection:"
echo "   Expected port: $PHARO_PORT"
echo "   Make sure PharoSmalltalkInteropServer is running"
echo ""
echo "💡 Quick Start:"
echo "   Ask @smalltalk-buddy what you want to do or any questions"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
