# Agent workflow

## Work artifact order

Prefer the most downstream existing artifact: `ticket > spec > conversation`. Do not
repeat discovery that the active issue already settles.

Normal flow: use the existing ticket, plan only what is still unresolved, implement one
ticket, run focused tests, review behavior-changing code, allow at most one correction,
then run the full checks, commit and open a PR. Use `/to-spec` and `/to-tickets` only when
the current work has no adequate ticket or spec.

## Roles and model routing

**Codex Luna 5.6 max fallback:** The preferred effort for all Luna roles is `max`.
If the current Codex client rejects `max` for a subagent, retry that same role using
`gpt-5.6-luna` with
`model_reasoning_effort = "xhigh"`. This specific fallback is preapproved;
do not block the task waiting for another approval. Because subagent TOML
sets a fixed effort, first adjust the affected agent configuration for the
current run or use a supported explicit per-spawn override; a prose instruction
alone cannot change the effective effort. Keep persistent configuration changes
in a reviewed change, rather than silently committing them. Record the
requested effort, effective fallback and available runtime evidence. If `xhigh`
also fails, stop and ask instead of silently changing model families.


The main session coordinates the workflow, Git and PRs. Choose a cost-effective
session model. Delegate planning, implementation and behavior-changing reviews
to fixed-model subagents. Route by risk before size: security, data writes,
migrations, concurrency and architecture changes require the complex role.

| Task | Subagent | Codex | Claude Code |
| --- | --- | --- | --- |
| Simple planning | `planner-light` | Luna 5.6 max | Sonnet medium |
| Complex or risky planning | `planner` | Sol medium | Opus medium |
| Small implementation | `implementer-light` | Luna 5.6 max | Sonnet low |
| Normal implementation | `implementer` | Luna 5.6 max | Sonnet medium |
| Complex implementation | `implementer-complex` | Sol medium | Opus medium |
| Optional focused test triage | `tester` | Luna 5.6 max | Sonnet low |
| Normal code review | `reviewer-light` | Luna 5.6 max | Sonnet medium |
| Complex or risky code review | `reviewer` | Sol medium | Opus medium |
| Approved implementation escalation | `implementer-escalated` | Sol high | Sonnet high |

Codex model and effort are configured in `.codex/agents/*.toml`; those files are
authoritative when they exist. Claude roles follow the configured Claude agent profiles.
Haiku is not part of the default Claude workflow. Escalated implementation needs
explicit user approval; Luna 5.6 max is approved as a standard role.

**Multi-agent default:** for every repository task, launch at least two focused subagent
assignments in parallel. Pair implementation with an independent spec, standards or test
analysis that can proceed without editing the same files; split genuinely disjoint tickets
or file sets when useful. Keep one implementer per ticket and separate file ownership. The
coordinator owns integration and Git operations.

**Routing:** delegate planning when the ticket leaves implementation choices open; choose
`planner-light` or `planner` by risk. Let the implementer run focused tests. Use `tester`
when separate execution or diagnosis helps parallelize the work. Review behavior-changing
code with `reviewer-light` or `reviewer`; for docs-only, config-only and mechanical changes,
give a second agent a narrow consistency or reference check. Reuse settled specs, tests and
review evidence instead of repeating work. Repo-specific rules in `AGENTS.md`, this
workflow and the active ticket govern when a general skill gives conflicting guidance.

**Model evidence:** Before first delegation, check client support and warn about
unavailable or superseded model IDs when verifiable. Do not silently substitute
models. For each phase actually run, report
`phase / role / configured model / effort / status`.
Use `configured` for config only, `spawn-confirmed` for a launched role, and
`runtime-verified` only when independent client/task metadata shows the
effective model *and* effort. An agent self-report is not evidence. Inspect
`/subagents` in Codex or `/tasks` in Claude Code if supported; note possible
runtime overrides and unavailable verification. Summarize actual versus
configured roles in the final report.

## Skills

Use a matching skill from `.agents/skills/<name>/SKILL.md`. If the current interface has a
Skill tool, invoke it by name; otherwise read the file directly.

| Situation | Skill |
| --- | --- |
| Review of a ticket that changes code (`lisp/`, `tests/`) | `code-review` (independent reviewer, before commit) |
| Implementing a ticket with behavior change | `tdd` (implementer) |
| Hard bug, flaky or unexplained failure | `diagnosing-bugs` |
| A term or durable decision is being settled (glossary, ADR) | `domain-modeling` |
| Turning a settled conversation into a spec / tickets | `to-spec`, then `to-tickets` |
| Editing `AGENTS.md`, `CLAUDE.md`, agent docs or skills | `writing-for-agents` |

Skip `code-review` for docs-only, config-only or purely mechanical changes; the coordinator's
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

- The implementer runs the narrowest relevant tests while working; the coordinator runs
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
- To check a change against the real `orgfdb` by hand, `scripts/e2e-index.sh [CORPUS]`
  indexes a copy of a directory and prints the `org-files-db-configs` setup. The directory
  rename fixtures come from `scripts/regen-dir-fixtures.sh`; regenerate them instead of
  editing ids or byte offsets by hand.
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
- `make ci` needs Eask and package archives. If either is unavailable, run
  `make check-offline`, which performs compile, lint and Buttercup checks using the Emacs
  and development packages configured for the environment. Set `EMACS` and
  `ORG_FILES_DB_DEPS` when they are not on the script's default paths.
