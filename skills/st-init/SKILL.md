---
name: st-init
description: Smalltalk (Pharo/Squeak) development session initializer. Use when starting a new Smalltalk development session, when the Smalltalk image connection needs to be established or verified, when the user asks about the development workflow or available commands, when development environment setup is needed, or when Docker or local Pharo/Squeak installation is required.
allowed-tools: Skill Bash mcp__smalltalk-interop__eval
---

# Initialize Smalltalk Development Session

Start a Smalltalk (Pharo or Squeak) development session: verify project structure, confirm the Smalltalk image connection, and orient to the development workflow.

## Initialization Sequence

### 1. Check Project Structure

```bash
if [ ! -f ".project" ] && [ ! -d "src" ]; then
  echo "No Smalltalk project structure detected"
fi
```

- ✅ `.project` OR `src/` found → continue
- ⚠️ Neither found → recommend `smalltalk-dev:st-setup-project` skill

**If no project exists**, offer to create one:
```
⚠️  No Smalltalk project structure detected in current directory.

Use /st-setup-project MyProjectName to set up:
  • .project configuration file
  • src/ directory with package structure
  • BaselineOf class and Core/Tests packages

Would you like to run /st-setup-project now?
```

- **User says yes**: Invoke `smalltalk-dev:st-setup-project` skill, then continue with steps 2–4.
- **User says no / continue anyway**: Proceed directly to step 2 (load smalltalk-developer) without project setup. Note in the status report that project structure is missing.

### 2. Load smalltalk-developer Skill

Use the `Skill` tool to load `smalltalk-dev:smalltalk-developer`. This activates the full development workflow guidance.

### 3. Verify Smalltalk Image Connection

Run `Smalltalk version` via eval:

```
mcp__smalltalk-interop__eval: 'Smalltalk version'
```

- ✅ Returns version string → connected, proceed
- ❌ Fails or times out → follow [Connection Setup](#connection-setup) below

### 4. Confirm Ready

Report status to user:
```
✅ Smalltalk image connected: Pharo X.Y (...) [or Squeak X.Y (...)]
✅ smalltalk-developer skill loaded
📋 Development workflow: Edit → Lint → Import → Test
```

## Connection Setup

If the Smalltalk image is not connected, check Docker availability first:

```bash
docker --version
```

### Docker Setup (Recommended, Pharo only)

Generate `compose.yml` in the project root:

```yaml
services:
  sis-pharo:
    image: mumez/smalltalk-interop-docker
    ports:
      - "5900:5900"
      - "6901:6901"
      - "8086:8086"
    environment:
      SIS_PORT: 8086
    volumes:
      - /tmp:/root/screenshots
      - .:/root/repos
```

Then instruct the user:
```bash
docker compose up -d
```

Re-verify connection after startup.

### Local Installation

If Docker is not available or not preferred, ask the user which Smalltalk dialect they use:

#### Pharo

**1. Install Pharo:**
```bash
mkdir pharo-stable
cd pharo-stable
curl https://get.pharo.org | bash
```

**2. Load PharoSmalltalkInteropServer:**
```bash
./pharo Pharo.image metacello install github://mumez/PharoSmalltalkInteropServer:main/src BaselineOfPharoSmalltalkInteropServer
```

**3. Start Pharo (SisServer starts automatically):**
```bash
./pharo-ui
```

Re-verify connection after Pharo is running.

#### Squeak

**1. Install Squeak** from [squeak.org](https://squeak.org/downloads/).

**2. Load SqueakSmalltalkInteropServer** — see [SqueakSmalltalkInteropServer](https://github.com/mumez/SqueakSmalltalkInteropServer) for installation instructions.

**3. Start Squeak** (SisServer starts automatically, or start it manually as below).

Re-verify connection after Squeak is running.

### Manual Start (Smalltalk image already installed)

If Pharo/Squeak is installed but the server is not running, ask the user to execute in the image:
```smalltalk
SisServer current start
```

## Development Workflow Overview

The core cycle (full details in `smalltalk-dev:smalltalk-developer`):

```
Edit .st file
    ↓
Lint (/st-lint)
    ↓
Import package (absolute path)
    ↓
Run tests
    ↓
Pass? → Done  |  Fail? → Debug → Fix → Repeat
```

## Available Skills and Commands

| Command | Purpose |
|---------|---------|
| `/st-import PackageName /abs/path` | Import Tonel package to the Smalltalk image |
| `/st-test TestClass` | Run SUnit tests |
| `/st-eval code` | Execute Smalltalk snippet |
| `/st-export PackageName` | Export package from the Smalltalk image |
| `/st-lint src/PackageName` | Validate Smalltalk best practices |

## Troubleshooting

| Problem | Solution |
|---------|---------|
| Connection timeout | Check if the Smalltalk image is running |
| Server not started | Run `SisServer current start` in the Smalltalk image |
| Port mismatch | Check `SIS_PORT` env var (default: 8086) |
| MCP issues | Verify `smalltalk-interop-mcp-server` is installed |
| Docker path issues | Check volume mounts in `compose.yml` |

## Related Skills

- `smalltalk-dev:st-setup-project` — Create project structure if missing
- `smalltalk-dev:smalltalk-developer` — Full development workflow
- `smalltalk-dev:smalltalk-debugger` — Debug test failures and errors
