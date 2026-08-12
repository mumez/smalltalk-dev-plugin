# smalltalk-dev Plugin Test Scenarios

Comprehensive test scenarios for verifying the smalltalk-dev plugin functionality. Use these scenarios to validate that all components work correctly after installation or updates.

## Prerequisites

Before running these tests:

1. **Smalltalk image running** (Pharo or Squeak) with the matching Interop Server (PharoSmalltalkInteropServer or SqueakSmalltalkInteropServer) started:
   ```smalltalk
   SisServer current start.
   ```

2. **Plugin installed**:
   ```bash
   claude plugin install smalltalk-dev
   ```

3. **Test project prepared** (optional):
   ```bash
   mkdir -p ~/st-test/src
   cd ~/st-test
   ```

---

## Test Suite 1: Entry Point Command

### Test 1.1: /st-init Command Execution

**Goal**: Verify /st-init command loads skill and provides guidance.

**Steps**:
1. Start a new Claude Code session
2. Run `/st-init`

**Expected Results**:
- ✅ Command executes without errors
- ✅ `smalltalk-developer` skill is mentioned as loaded
- ✅ Development workflow explanation appears (Edit → Import → Test)
- ✅ Available commands are listed
- ✅ Smalltalk image connection verification (version check or connection test)
- ✅ Quick start examples are provided

**Success Criteria**:
- Output includes "Edit → Import → Test workflow"
- Lists at least 5 commands (/st-import, /st-test, /st-eval, /st-export, /st-validate)
- Provides concrete examples

**Common Issues**:
- If the Smalltalk image connection fails, verify SisServer is running
- If skill doesn't load, check plugin installation

---

### Test 1.2: /st-init Without the Smalltalk Image Running

**Goal**: Verify graceful handling when the Smalltalk image is not connected.

**Steps**:
1. Stop the Interop Server in the Smalltalk image
2. Run `/st-init`

**Expected Results**:
- ✅ Command executes (doesn't crash)
- ✅ Connection error or warning message
- ✅ Troubleshooting guidance provided
- ✅ Workflow explanation still appears

**Success Criteria**:
- Clear error message about the Smalltalk image connection
- Provides steps to start SisServer
- Remains helpful despite connection failure

---

## Test Suite 2: Skill Triggering

### Test 2.1: smalltalk-developer Skill Activation

**Goal**: Verify skill activates on correct trigger phrases.

**Test Cases**:

| Trigger Phrase | Expected Skill |
|----------------|----------------|
| "Create a Smalltalk class called Person" | smalltalk-developer |
| "Add a method to the Person class" | smalltalk-developer |
| "Write Smalltalk code for a counter" | smalltalk-developer |
| "Edit Tonel files for my project" | smalltalk-developer |
| "Import package to the Smalltalk image" | smalltalk-developer |
| "Implement a Calculator in Smalltalk" | smalltalk-developer |

**Steps** (for each trigger phrase):
1. Start fresh conversation
2. Use exact trigger phrase
3. Observe which skill/approach Claude uses

**Expected Results**:
- ✅ `smalltalk-developer` skill activates
- ✅ Claude understands Tonel file editing
- ✅ Suggests `/st-import` after creating code
- ✅ Mentions Edit → Import → Test workflow

**Verification**:
```
Look for responses that:
- Create or edit .st files in Tonel format
- Suggest import commands
- Reference Smalltalk-specific concepts
```

---

### Test 2.2: smalltalk-debugger Skill Activation

**Goal**: Verify debugging skill activates on error scenarios.

**Test Cases**:

| Trigger Phrase | Expected Skill |
|----------------|----------------|
| "Test failed with MessageNotUnderstood" | smalltalk-debugger |
| "Debug this Smalltalk error" | smalltalk-debugger |
| "Why is this failing?" | smalltalk-debugger |
| "Inspect the object" | smalltalk-debugger |
| "Run partial code to debug" | smalltalk-debugger |
| "Stack trace analysis" | smalltalk-debugger |

**Steps**:
1. Use trigger phrase
2. Optionally provide error message/stack trace

**Expected Results**:
- ✅ `smalltalk-debugger` skill activates
- ✅ Suggests using `/st-eval` for investigation
- ✅ Systematic debugging approach (step-by-step)
- ✅ Mentions partial execution with error handling

**Verification**:
```
Look for:
- Step-by-step debugging strategy
- Use of eval tool for investigation
- Error pattern recognition (MessageNotUnderstood, nil issues, etc.)
```

---

### Test 2.3: smalltalk-usage-finder Skill Activation

**Goal**: Verify usage finder skill activates correctly.

**Test Cases**:

| Trigger Phrase | Expected Skill |
|----------------|----------------|
| "How to use OrderedCollection class?" | smalltalk-usage-finder |
| "Show usage examples of Array" | smalltalk-usage-finder |
| "What is Dictionary responsible for?" | smalltalk-usage-finder |
| "Find examples of Point usage" | smalltalk-usage-finder |
| "What does Collection class do?" | smalltalk-usage-finder |
| "Package overview of Collections" | smalltalk-usage-finder |

**Expected Results**:
- ✅ `smalltalk-usage-finder` skill activates
- ✅ Uses MCP tools: `search_references`, `get_class_comment`, `search_methods_like`
- ✅ Provides usage examples from codebase
- ✅ Explains class responsibilities

**Verification**:
```
Look for:
- Analysis of existing usage patterns
- Code examples from the codebase
- Responsibility/purpose explanations
```

---

### Test 2.4: smalltalk-implementation-finder Skill Activation

**Goal**: Verify implementation finder skill activates correctly.

**Test Cases**:

| Trigger Phrase | Expected Skill |
|----------------|----------------|
| "Who implements printOn:?" | smalltalk-implementation-finder |
| "Find implementors of initialize" | smalltalk-implementation-finder |
| "How is hash implemented?" | smalltalk-implementation-finder |
| "Show implementations of select:" | smalltalk-implementation-finder |
| "Which classes override at:put:?" | smalltalk-implementation-finder |
| "Abstract method implementations for do:" | smalltalk-implementation-finder |

**Expected Results**:
- ✅ `smalltalk-implementation-finder` skill activates
- ✅ Uses `search_implementors` MCP tool
- ✅ Gets method source for comparison
- ✅ Identifies implementation patterns

**Verification**:
```
Look for:
- search_implementors usage
- Pattern analysis across implementations
- Idiom identification (e.g., "hash uses bitXor:")
```

---

## Test Suite 3: Command Execution with allowed-tools

### Test 3.1: /st-eval Command

**Goal**: Verify eval command uses only allowed tools.

**Steps**:
1. Run `/st-eval 1 + 1`
2. Run `/st-eval Smalltalk version`

**Expected Results**:
- ✅ Command executes
- ✅ Uses only `mcp__smalltalk-interop__eval` tool
- ✅ Returns result from the Smalltalk image
- ✅ No other MCP tools used

**Verification**:
```bash
# Check tool usage in debug mode
claude --debug
/st-eval 1 + 1

# Should see: mcp__smalltalk-interop__eval
# Should NOT see: import_package, run_test, etc.
```

---

### Test 3.2: /st-import Command

**Goal**: Verify import command uses allowed tools only.

**Setup**:
```bash
# Create test Tonel file
mkdir -p ~/test-package/src/TestPackage
cat > ~/test-package/src/TestPackage/TestClass.st << 'EOF'
Class {
    #name : #TestClass,
    #superclass : #Object,
    #category : #TestPackage
}

{ #category : #accessing }
TestClass >> name [
    ^ 'Test'
]
EOF
```

**Steps**:
1. Run `/st-import TestPackage ~/test-package/src`

**Expected Results**:
- ✅ Uses `mcp__smalltalk-interop__import_package`
- ✅ Optionally uses `mcp__smalltalk-validator__validate_tonel_smalltalk_from_file`
- ✅ Package imported successfully
- ✅ No other tools used

**Cleanup**:
```smalltalk
"In the Smalltalk image:"
TestPackage removeFromSystem
```

---

### Test 3.3: /st-test Command

**Goal**: Verify test command uses allowed tools only.

**Steps**:
1. Run `/st-test SomeTestClass` (or package name)

**Expected Results**:
- ✅ Uses `mcp__smalltalk-interop__run_class_test` OR `run_package_test`
- ✅ Test results displayed
- ✅ No other tools used

---

### Test 3.4: /st-export Command

**Goal**: Verify export command uses allowed tools only.

**Steps**:
1. Run `/st-export SomePackage ~/export-test/src`

**Expected Results**:
- ✅ Uses only `mcp__smalltalk-interop__export_package`
- ✅ Tonel files created
- ✅ No other tools used

---

### Test 3.5: /st-validate Command

**Goal**: Verify validate command uses allowed tools only.

**Steps**:
1. Create test Tonel file
2. Run `/st-validate /path/to/file.st`

**Expected Results**:
- ✅ Uses `mcp__smalltalk-validator__validate_tonel_smalltalk_from_file`
- ✅ Validation result shown
- ✅ May also use: `validate_tonel_smalltalk`, `validate_smalltalk_method_body`
- ✅ No other tools used

---

## Test Suite 4: Complete Development Workflows

### Test 4.1: Basic Class Creation Workflow

**Scenario**: Create a simple class, import, and test.

**Steps**:
1. "Create a Person class in Smalltalk with firstName and lastName instance variables"
2. Observe Tonel file creation
3. Accept `/st-import` suggestion
4. "Add a test for Person class"
5. Run `/st-test PersonTest`

**Expected Results**:
- ✅ Person.st created in Tonel format
- ✅ Correct class definition syntax
- ✅ Import suggestion appears
- ✅ Import succeeds
- ✅ PersonTest.st created
- ✅ Test runs successfully

**Success Criteria**:
```smalltalk
"In the Smalltalk image after import:"
Person new firstName: 'John'; lastName: 'Doe'; yourself
→ a Person
```

---

### Test 4.2: Debug Failed Test Workflow

**Scenario**: Test fails, debug using eval, fix, re-import.

**Steps**:
1. Create Person class with intentional bug:
   ```smalltalk
   Person >> fullName [
       ^ firstName, lastName  "Missing space!"
   ]
   ```
2. Create test:
   ```smalltalk
   PersonTest >> testFullName [
       | person |
       person := Person new firstName: 'John'; lastName: 'Doe'.
       self assert: person fullName equals: 'John Doe'
   ]
   ```
3. Import and run test: `/st-test PersonTest`
4. Test fails
5. "Debug this test failure"
6. Claude uses `/st-eval` to investigate
7. Fix identified
8. Re-import
9. Re-test

**Expected Results**:
- ✅ `smalltalk-debugger` skill activates
- ✅ Uses `/st-eval` for investigation
- ✅ Identifies missing space
- ✅ Suggests fix
- ✅ Re-import workflow clear
- ✅ Test passes after fix

---

### Test 4.3: Usage Discovery Workflow

**Scenario**: Learn how to use existing class.

**Steps**:
1. "How do I use OrderedCollection in Smalltalk?"
2. Observe skill activation and MCP tool usage
3. "Show me examples of adding items to OrderedCollection"

**Expected Results**:
- ✅ `smalltalk-usage-finder` skill activates
- ✅ Class comment retrieved
- ✅ Usage examples from codebase shown
- ✅ Common methods explained (add:, remove:, do:, etc.)

---

### Test 4.4: Implementation Learning Workflow

**Scenario**: Learn how to implement a method idiomatically.

**Steps**:
1. "I need to implement hash for my Person class. Show me how other classes do it."
2. Observe analysis
3. Apply pattern

**Expected Results**:
- ✅ `smalltalk-implementation-finder` skill activates
- ✅ Shows Point>>hash, Association>>hash examples
- ✅ Identifies `bitXor:` idiom
- ✅ Suggests:
   ```smalltalk
   Person >> hash [
       ^ firstName hash bitXor: lastName hash
   ]
   ```

---

## Test Suite 5: Hooks Verification

### Test 5.1: PostToolUse Hook (suggest-class-comment.py)

**Goal**: Verify hook occasionally suggests class documentation after editing .st files.

**Steps**:
1. Ensure hook is enabled (check `hooks/hooks.json`)
2. Edit or create a `.st` file
3. Repeat several times (hook triggers with ~10% probability)

**Expected Results**:
- ✅ Hook script executes on Write/Edit of `.st` files
- ✅ Occasionally displays suggestion:
   ```
   💡 Tip: Modified Tonel file detected. Consider running /smalltalk-commenter
      to add or improve class comments for better documentation.
   ```
- ✅ Non-.st files do NOT trigger the hook

**Verification**:
```bash
# Manually test hook script
echo '{"tool_name":"Edit","tool_input":{"file_path":"test.st"}}' | \
  uv run python hooks/suggest-class-comment.py

# May output documentation suggestion JSON (10% chance)
```

---

## Test Suite 6: Progressive Disclosure

### Test 6.1: References Loading

**Goal**: Verify skill references are accessible.

**Test for smalltalk-implementation-finder**:

**Steps**:
1. Activate skill with trigger phrase
2. Ask for detailed analysis techniques
3. Observe if reference files are mentioned/loaded

**Expected**:
- ✅ Main SKILL.md provides quick reference
- ✅ References to `references/implementation-analysis.md` for details
- ✅ References to `examples/implementation-scenarios.md` for scenarios
- ✅ Progressive disclosure working (not all content loaded at once)

---

## Test Suite 7: MCP Integration

### Test 7.1: smalltalk-interop MCP Server

**Goal**: Verify all smalltalk-interop tools are accessible.

**Test Tools**:
```bash
# These should all work via commands or skills
mcp__smalltalk-interop__eval
mcp__smalltalk-interop__import_package
mcp__smalltalk-interop__export_package
mcp__smalltalk-interop__run_class_test
mcp__smalltalk-interop__run_package_test
mcp__smalltalk-interop__get_class_source
mcp__smalltalk-interop__get_method_source
mcp__smalltalk-interop__search_implementors
mcp__smalltalk-interop__search_references
```

**Verification**:
Run `/st-eval 1 + 1` - Should use eval tool successfully.

---

### Test 7.2: smalltalk-validator MCP Server

**Goal**: Verify validator tools work.

**Test Tools**:
```bash
mcp__smalltalk-validator__validate_tonel_smalltalk_from_file
mcp__smalltalk-validator__validate_tonel_smalltalk
mcp__smalltalk-validator__validate_smalltalk_method_body
```

**Verification**:
Run `/st-validate test.st` - Should validate successfully.

---

## Troubleshooting Guide

### Issue: Skills Not Activating

**Symptoms**: Trigger phrases don't activate expected skills.

**Checks**:
1. Verify plugin installed: `claude plugin list`
2. Check skill descriptions have trigger phrases
3. Try exact trigger phrases from tests
4. Restart Claude Code session

**Fix**:
```bash
claude plugin uninstall smalltalk-dev
claude plugin install smalltalk-dev
```

---

### Issue: MCP Connection Failures

**Symptoms**: Tools return connection errors.

**Checks**:
1. Verify the Smalltalk image is running
2. Check SisServer status:
   ```smalltalk
   SisServer current
   ```
3. Verify port (default 8086):
   ```bash
   echo $SIS_PORT
   ```

**Fix**:
```smalltalk
"In the Smalltalk image:"
SisServer current start.
SisServer current  "Should show running server"
```

---

### Issue: Hooks Not Executing

**Symptoms**: No suggestions after editing .st files.

**Checks**:
1. Verify hooks.json exists: `cat hooks/hooks.json`
2. Verify `uv` is installed: `uv --version`
3. Test hook script manually:
   ```bash
   echo '{"tool_name":"Edit","tool_input":{"file_path":"test.st"}}' | \
     uv run python hooks/suggest-class-comment.py
   ```

**Note**: Hook triggers with ~10% probability by design. Try several times if no suggestion appears.

---

## Test Checklist

Quick checklist for complete validation:

### Commands (6)
- [ ] `/st-init` - Entry point working
- [ ] `/st-eval` - Execution working
- [ ] `/st-import` - Import working
- [ ] `/st-test` - Testing working
- [ ] `/st-export` - Export working
- [ ] `/st-validate` - Validation working

### Skills (4)
- [ ] `smalltalk-developer` - Triggers on development tasks
- [ ] `smalltalk-debugger` - Triggers on errors/debugging
- [ ] `smalltalk-usage-finder` - Triggers on "how to use"
- [ ] `smalltalk-implementation-finder` - Triggers on "who implements"

### Hooks (1)
- [ ] PostToolUse hook - Occasionally suggests /smalltalk-commenter on .st file edits

### MCP Servers (2)
- [ ] smalltalk-interop - Connection working
- [ ] smalltalk-validator - Validation working

### Workflows (4)
- [ ] Class creation → Import → Test
- [ ] Test failure → Debug → Fix → Re-test
- [ ] Usage discovery
- [ ] Implementation learning

---

## Success Criteria

The plugin is considered fully functional when:

✅ All 6 commands execute without errors
✅ All 4 skills activate on correct triggers
✅ allowed-tools restrictions work (commands only use permitted tools)
✅ Hooks execute and provide helpful suggestions
✅ MCP servers connect and respond
✅ Complete workflows execute smoothly
✅ Progressive disclosure works (references accessible)
✅ Error handling is graceful

---

## Next Steps After Testing

If all tests pass:
1. Document any quirks or special cases found
2. Create additional examples based on real usage
3. Consider publishing to marketplace
4. Gather user feedback

If tests fail:
1. Use this document to identify specific failures
2. Check troubleshooting guide
3. Review plugin structure and configuration
4. Test individual components in isolation
