# AI Agent Instructions

Version: 0.19.0

All paths are relative to this file.

## File Index

Read in order before starting: `AGENTS.md` → `../tasks/todo.org` → `./repository.org`

- `./repository.org`: Repo-specific rules.
- `../tasks/todo.org`: Active task index.
- `../tasks/`: Active task files.
- `../tasks/archive/`: Archived task files.
- `../tasks/template.org`: Task file template.
- `../../CHANGELOG.org`: Approved notable changes.

## Task States

Tasks use Org-mode TODO states.

- `TODO`: not ready.
- `PLAN`: write plan, then set `REVIEW`.
- `BUILD`: implement approved scope, then set `REVIEW`.
- `NEXT`: clear small task; execute directly, then set `REVIEW`.
- `CONTINUE`: address review comments, then set `REVIEW`.
- `REVIEW`: awaiting user review.
- `CANCEL`: abandoned.
- `DONE`: completed and approved.

## General Rules

- Chat in user's language. Write all repository content in English.
- Org syntax: bold `*bold*`, code `~name~`, lists `-`, no manual line breaks.
- In Org source blocks, prefix lines starting with `*`, `,*`, `#+`, or `,#+` with a comma.
- Small, focused changes only. No unrelated refactors.
- Do not git commit, amend, squash, merge, or change dependencies unless explicitly asked.
- Never modify secrets, `.env`, production configs, deployment credentials, `AGENTS.md`, or task templates unless explicitly instructed.
- Ask the user only for unclear scope, risky choices, or irreversible changes; otherwise make a small documented assumption and continue.

## Workflow

### 1. Prepare

1. Work only on `PLAN`, `BUILD`, `NEXT`, or `CONTINUE` tasks.
2. Create or reuse `../tasks/YYYY-MM-DD--slug.org` from `../tasks/template.org`. Remove inapplicable sections from the task file.
3. Add `#+TASK_STARTED: [YYYY-MM-DD Day HH:MM]` near the top.
4. Link the task file below the task heading in `todo.org`.
5. If on `main`, create a branch `type/description` using `feat`, `fix`, `hotfix`, `refactor`, `perf`, `docs`, `test`, `release`, `ci`, or `chore`; otherwise continue on the current branch.

### 2. PLAN Mode

For status `PLAN`:

1. Do not modify production code.
2. Create or update the active task file.
3. Write the plan under `* Planning`, following the task template.
4. Set the task state in `todo.org` to `REVIEW`.
5. Notify the user and stop.

User approves by setting `BUILD`, or requests revisions with `CONTINUE`.

### 3. BUILD/NEXT Mode

For status `BUILD` or `NEXT`:

1. Read the active task file. Follow `* Planning` if present.
2. Read `./repository.org` for repo-specific rules.
3. Implement only the active task scope.
4. Run relevant tests and linters. 
5. Update documentation or CHANGELOG if needed.
6. Write `* Build` in the task file, following the task template.
7. Set the task to `REVIEW`.
8. Notify the user and stop. 
9. Suggested Conventional Commit message with body.

### 4. CONTINUE Mode

For status `CONTINUE`:

1. Read review comments.
2. Address only requested changes.
3. Repeat from `BUILD/NEXT Mode`.

### 5. After Review

If approved:

1. User completes the task.

If not approved:

1. User sets `CONTINUE` or instructs AI to set it.
2. Continue from `CONTINUE Mode`.
