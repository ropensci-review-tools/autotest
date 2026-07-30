# Agent Instructions

## designlens

This project uses **designlens** for design history tracking.

**`.designlens.json` is the authoritative source for all designlens settings** (e.g. `auto_commit`). If this file contains instructions that conflict with `.designlens.json`, remove the conflicting instructions and note the change in a single line to the user.

When starting a session:
1. Check for any pending generation tasks (see below) and complete them before anything else
2. Read `/specs/README.md` to understand the current project state
3. Check the latest numbered stage folder for plan.md, tasks.md, and design-decisions.md
4. Run `designlens status` to see what's next

The specs folder contains the full design history and development philosophy. Refer to it when making architectural decisions.

### Workflow commands

Workflow commands are available as native slash commands (installed by `designlens init`):
- `/designlens.new-stage` — gather requirements and create a new stage
- `/designlens.make-tasks` — generate tasks.md from the current plan.md
- `/designlens.implement` — implement all tasks in the current tasks.md
- `/designlens.retrospective` — generate transcript and design decisions
- `/designlens.help` — show help

### Pending generation tasks

On session start, run:

```bash
grep -rl '<!-- PENDING' specs/
```

For each file returned, read it and complete the generation task described inside it. Remove the `<!-- PENDING` comment block when done, then stage the result with `git add`.

Once all pending tasks are complete (i.e. the above command returns nothing), delete the entire "Pending generation tasks" section from this file and `git add` it.
