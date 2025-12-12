# Commands Reference

Quick reference for all available slash commands. For detailed AI-driven usage, see the command files in [`commands/`](commands/) directory.

## Available Commands

### `/st:eval [code]`

Execute Smalltalk code snippets for quick testing and debugging.

```bash
/st:eval Smalltalk version
/st:eval MyClass new doSomething
```

**Use for:**
- Connection checks
- Quick code testing
- Debugging with error handling patterns

### `/st:import [PackageName] [path]`

Import Tonel packages into running Pharo image.

```bash
/st:import MyPackage /home/user/project/src
/st:import MyPackage-Tests /home/user/project/src
```

**Use for:**
- Loading changes after editing Tonel files
- Re-importing after fixes

### `/st:export [PackageName] [path]`

Export package from Pharo image back to Tonel files.

```bash
/st:export MyPackage /home/user/project/src
/st:export MyPackage-Tests /home/user/project/src
```

**Use for:**
- Saving debugger fixes back to Tonel files
- Exporting code generated in Pharo
- Syncing after interactive development in Pharo

### `/st:test [TestClass|PackageName]`

Run SUnit tests.

```bash
/st:test MyTestClass
/st:test MyPackage-Tests
```

**Use for:**
- Verifying implementations
- Regression testing

### `/st:validate [file_path]` (Optional)

Validate Tonel syntax. Rarely needed with modern AI.

```bash
/st:validate /path/to/file.st
```

**Use for:**
- Debugging mysterious import failures
- Validating manually edited files
