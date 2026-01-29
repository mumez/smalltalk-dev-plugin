# Commands Reference

Quick reference for all available slash commands. For detailed AI-driven usage, see the command files in [`commands/`](commands/) directory.

## Available Commands

### `/st:buddy` ⭐ Recommended Entry Point

Start your friendly Smalltalk development assistant. This is the easiest way to use the plugin.

```bash
/st:buddy
```

**Use for:**
- Getting started with Smalltalk development
- General questions about development, debugging, or code understanding
- Guided workflows with automatic skill loading
- Learning AI-assisted Smalltalk development

**What it does:**
- Provides a friendly, conversational interface
- Routes questions to appropriate specialized skills
- Guides through development, testing, and debugging workflows
- Helps you learn effective AI development patterns

**Example interaction:**
```
/st:buddy

You: "I want to create a Person class with name and age"
AI:  I'll help you create that! [Loads smalltalk-developer skill and guides implementation]

You: "My test failed with MessageNotUnderstood"
AI:  Let me help debug this... [Loads smalltalk-debugger skill and investigates]
```

**After running /st:buddy once**, you can simply ask questions naturally without prefixing commands.

---

### `/st:init`

Start a new Smalltalk development session. Loads the `smalltalk-developer` skill and explains the Edit → Import → Test workflow.

```bash
/st:init
```

**Use for:**
- Starting a new development session
- Getting oriented with the Pharo development workflow
- Verifying your environment is ready
- Learning the available commands

**What it does:**
- Loads smalltalk-developer skill
- Verifies Pharo connection
- Explains the standard development cycle
- Lists available commands and tools

### `/st:setup-project [ProjectName]`

Set up Pharo project boilerplate structure from scratch. Creates standard package layout with BaselineOf, Core, and Tests packages.

```bash
/st:setup-project MyProject
/st:setup-project
```

**Use for:**
- Starting a new Pharo project from zero
- Creating standard package structure
- Generating baseline configuration automatically

**What it creates:**
- `.project` file with src directory configuration
- `src/BaselineOfXXX/` with baseline class and package.st
- `src/XXX-Core/` with package.st
- `src/XXX-Tests/` with package.st
- Baseline method with proper package dependencies

**Requirements:**
- Project name must be in PascalCase (e.g., MyProject, RedisClient)
- Will not overwrite existing projects (stops if src/ contains packages)

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

### `/st:lint [path]`

Lint Tonel files for Smalltalk best practices before importing.

```bash
/st:lint src/MyPackage
/st:lint src/MyPackage/MyClass.st
/st:lint src
```

**Use for:**
- Checking code quality before import
- Ensuring Smalltalk best practices
- Finding common code issues early

**What it checks:**
- Class prefix (name collision prevention)
- Method length (15 lines standard, 40 for UI/tests)
- Instance variable count (max 10)
- Direct instance variable access (use accessors)

### `/st:import [PackageName] [path]`

Import Tonel packages into running Pharo image.

```bash
/st:import MyPackage /home/user/project/src
/st:import MyPackage-Tests /home/user/project/src
```

**Use for:**
- Loading changes after editing Tonel files
- Re-importing after fixes

**Recommended workflow:**
1. Edit Tonel files
2. Run `/st:lint` to check quality
3. Run `/st:import` to load into Pharo
4. Run `/st:test` to verify

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
