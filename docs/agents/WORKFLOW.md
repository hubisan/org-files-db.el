# Agent workflow

## Work artifact order

Prefer the most downstream existing artifact: `ticket > spec > conversation`. Do not
repeat discovery that the active issue already settles.

Normal flow: `/to-spec` -> `/to-tickets` (usually done by the user) -> planner plans one
ticket -> `implementer` subagent -> focused tests -> planner review -> at most one
correction -> `make ci` -> commit -> PR.

## Roles and model routing

| Role | Claude Code |
| --- | --- |
| Planner and reviewer | main session, Opus 5.5, effort low |
| Implementer (default) | subagent `implementer`: Sonnet, effort medium |
| Escalated implementer | subagent `implementer-escalated`: Sonnet, effort high |

- The planner is the main session; the user selects its model and effort (`/model`,
  `/effort`). Subagent model and effort are fixed in `.claude/agents/*.md`.
- The planner reads the issue, writes a short plan (owned files, seam, focused test
  command, relevant docs section), and delegates to `implementer`.
- Use one implementer by default. Start several only for genuinely independent work with
  disjoint files.
- The implementer fixes ordinary test failures itself and returns a compact report.
- The planner reviews. For tickets that change the process layer, the watcher lifecycle,
  presentation-json decoding or cache-mode behavior, the planner may raise its own effort to
  medium for the review.
- `implementer-escalated` and any `xhigh`/`max` effort require explicit user approval.
- Git/GitHub housekeeping (branch, commit, push, PR, issues, labels) stays in the planner
  session; delegating it costs more context than it saves.
- Other harnesses keep the same three roles with their own models.

## Skills

Invoke a matching skill through the Skill tool (other harnesses: open its `SKILL.md`).

| Situation | Skill |
| --- | --- |
| Review of a ticket that changes code (`lisp/`, `tests/`) | `code-review` (planner, before commit) |
| Implementing a ticket with behavior change | `tdd` (implementer) |
| Hard bug, flaky or unexplained failure | `diagnosing-bugs` |
| A term or durable decision is being settled (glossary, ADR) | `domain-modeling` |
| Turning a settled conversation into a spec / tickets | `to-spec`, then `to-tickets` |
| Editing `AGENTS.md`, `CLAUDE.md`, agent docs or skills | `writing-for-agents` |

Skip `code-review` for docs-only, config-only or purely mechanical changes; the planner's
own diff review is enough there.

`code-review` fixed point: the commit before the ticket's first commit (or `HEAD` plus
working tree before committing). Spec source: the active GitHub issue. Standards source:
`AGENTS.md`.

## Correction budget

Initial implementation, including its own red/green fixes, does not count. Maximum
correction cycles after the first review: **1**.

A finding is **substantial** when an acceptance criterion is missing or wrong, a
repository rule is violated, behavior is incorrect, or a test is missing for changed
behavior. Naming and style judgement calls are not substantial: report them, fix them
only inside an already-planned correction.

1. First review finds a substantial issue: the planner adjusts the plan and delegates one
   focused correction.
2. Second review still finds a substantial issue: stop and ask the user. No further
   correction and no escalation without approval.

Also stop and ask when a failure exposes a wrong architectural assumption, a material
scope change, a needed `orgfdb` CLI change (file an issue in `hubisan/org-files-db`) or
dependency change, or the same structural failure repeating. Final `make ci`
compile/lint/indent fixes are not a correction cycle; a failing test that needs a code
change is.

## Testing and context budget

- The implementer runs the narrowest relevant tests while working; the planner runs
  `make ci` once at the end of the ticket.
- Focused Buttercup run, filtered by a regexp over `describe`/`it` descriptions (needs
  `make setup` once):

  ```sh
  eask emacs --batch -L lisp -L tests -l buttercup \
    -f buttercup-run-discover "$PWD/tests" --pattern "shared orgfdb process layer"
  ```

  Pass an absolute test directory (`"$PWD/tests"`); a relative one fails to load. Without
  Eask, replace `eask emacs` by `emacs -Q -L <path-to-buttercup>`. Byte-compile check:
  `eask recompile`. Full suite: `make test`.
- Keep command output compact; inspect detailed logs only when a step fails.
- Load context progressively. Subagent briefs name the exact files and sections so the
  implementer does not rediscover them. Do not scan `.claude/skills/`.

## Tests

- Tests are Buttercup specs in `tests/` (currently `tests/org-files-db-test.el`), grouped in
  one top-level `describe` per area (for example `"shared orgfdb process layer"`) so
  `--pattern` can select them. Add new specs to the matching area.
- Edge cases live in the specs of the module that owns them (process, presentation
  decoding, faces, views, watcher and so on), not in end-to-end command specs. Command specs
  keep one or two smoke tests per feature plus error-path checks. Do not repeat a
  lower-layer test at a higher layer.
- Prefer table-driven specs (a list of cases looped into `it` forms) over several
  near-identical `it` forms.
- Stub `orgfdb` at the process seam (`org-files-db-process.el`) with `spy-on` or
  `cl-letf`, returning recorded presentation-json or error payloads. Unit tests never start a
  real `orgfdb` process. An integration test that needs the real binary must be explicitly
  marked as such and skip cleanly when `orgfdb` is not installed.
- Use temporary files and buffers for file and buffer actions and clean them up; never touch
  the user's Org files or configuration.
- Test-only helpers use the `org-files-db-test--` prefix. Reuse existing helpers in
  `tests/` instead of defining local copies.

## Commits and completion

- One commit per completed ticket after review and `make ci`, on a `<type>/<slug>` branch
  (see `AGENTS.md`), with `Refs: #<issue>`. The PR body says `Closes #<issue>`.
- Before declaring a ticket complete: review the diff against the issue, run `make ci`,
  update docs and `CHANGELOG.org` as `AGENTS.md` requires, and state what was
  implemented, what was tested and what remains untested.
