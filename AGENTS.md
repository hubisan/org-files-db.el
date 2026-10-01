# org-files-db.el - Agent Instructions

Emacs Lisp client for the `orgfdb` CLI of
[org-files-db](https://github.com/hubisan/org-files-db).

## Start here

- Work is tracked as GitHub issues in `hubisan/org-files-db.el`; the active issue defines
  scope and acceptance criteria. Prefer the most downstream artifact:
  `ticket > spec > conversation`. See `docs/agents/issue-tracker.md`.
- Stable context, goals, non-goals and rebuild decisions:
  `.project/tasks/project-context.org`. Public facts: `README.org`. Later phases and
  backlog are GitHub issues.
- Implementation, review, model routing, correction budget or completion workflow: read
  `docs/agents/WORKFLOW.md`.
- Read history (`.project/tasks/archive/`, `CHANGELOG.org`) only when the active issue
  requires it.

## Rules

- Chat in the user's language. Write all repository content in English.
- Org files (docs, changelog): bold `*bold*`, code `~name~`, lists `-`, no manual line
  breaks. Prefix source-block lines starting with `*` or `#+` with a comma.
- Small, focused changes; no unrelated refactors or formatting changes. Do not change
  dependencies unless asked.
- Never touch secrets, `.env`, production configs or credentials. Do not edit generated
  files (Eask state, package archives, `*.elc`, autoloads, `*-pkg.el`, test output).
- Branches: `<type>/<slug>` with `feat`, `fix`, `refactor`, `perf`, `docs`, `test`, `ci`,
  `chore`. Commits: Conventional Commits, English. Add `Refs: #<issue>`. Do not merge unless
  asked.
- Ask only for unclear scope, risky or irreversible choices; otherwise state a small
  assumption and continue.

## Emacs Lisp

- Package files in `lisp/`, Buttercup tests in `tests/`.
- Public `defcustom` and `defface` forms live in `lisp/org-files-db-core.el`. Keep Core small
  and free from specialized logic. No circular module dependencies.
- Prefix every global symbol with `org-files-db-`. Name public symbols by user-facing
  purpose, not by the implementing file. Private symbols use the prefix of their defining
  module before `--` (`org-files-db-process--run`), never a generic `org-files-db--`.
  Test-only helpers use `org-files-db-test--`.
- One space after a full stop. Docstrings, messages and errors start with a capital letter;
  no empty line after the first docstring line.
- No obsolete APIs (`when-let*`, not `when-let`), no unused lexical variables. When
  renaming private state, update all bindings and readers.
- Pass process arguments as a list; never build shell command strings.

## Checks and docs

- Run `make fmt` after editing Emacs Lisp. Run `make ci` (Eask package, install, compile,
  lint: package-lint, checkdoc, indent, relint; Buttercup tests) before declaring work done.
  Compiler and linter warnings are errors. State exactly which checks were not run.
- Add or update Buttercup tests for behavior changes; test guidance: `docs/agents/WORKFLOW.md`,
  section Tests.
- Update docs in the same change: `README.org` when user-facing behavior or claims change,
  `CHANGELOG.org` for user-visible changes.

## orgfdb seam

- The public `orgfdb` CLI is the only seam to the Rust project. No direct SQLite access.
- `presentation-json` version 3 is the presentation model (reference:
  [presentation.org](https://github.com/hubisan/org-files-db/blob/main/docs/reference/presentation.org)).
- Rust owns columns, sorting, row expansion, widths, truncation, padding and presentation
  caching. Emacs owns configuration, processes, JSON decoding, faces, completion,
  navigation and actions. Do not reimplement Rust responsibilities in Emacs.
- Needed Rust-side changes go as issues to `hubisan/org-files-db`; do not work around a
  missing CLI feature in Emacs Lisp.

## Agent skills

### Issue tracker

Issues and specs are GitHub issues in `hubisan/org-files-db.el`. See `docs/agents/issue-tracker.md`.

### Triage labels

Use the default mattpocock/skills vocabulary. See `docs/agents/triage-labels.md`.

### Domain docs

Single-context repo; domain docs are created lazily. See `docs/agents/domain.md`.

This block belongs in `AGENTS.md`; `CLAUDE.md` only imports it.
