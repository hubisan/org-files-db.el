# Issue Tracker

Issues and specifications for this repo live in GitHub repository
`hubisan/org-files-db.el`. A spec is an issue; implementation tickets are linked sub-issues
or checklist items marked `Part of #<spec>`.

Use `gh` when available. Otherwise use the connected GitHub tools for the same repository.
Resolve the repository from the current remote or workspace context; do not guess issue
numbers or switch repositories.

## Conventions

- Read an issue with its comments and labels before acting on it.
- Keep acceptance criteria as a Markdown task list. Triage state is represented by labels;
  see `triage-labels.md`.
- Use `Closes #<issue>` in a PR body when merging should close the ticket.
- PRs are not a request surface for triage.
- The `/wayfinder` skill defines maps, child tickets and blocking relationships; use its
  GitHub operations when creating or resolving a map.

## Common `gh` commands

```sh
gh issue view <number> --comments
gh issue create --title "..." --body-file <file>
gh issue comment <number> --body-file <file>
gh issue edit <number> --add-label <label>
gh pr view <number> --comments
```

When a skill says to publish work to the tracker, create or update a GitHub issue. For a
bare number that might refer to a PR, check `gh pr view <number>` before treating it as an
issue.

## Wayfinder

Follow `.agents/skills/wayfinder/SKILL.md` for map and ticket lifecycle. Represent maps as
`wayfinder:map` issues and decision tickets as child issues. Create tickets before wiring
blocking edges. Use GitHub's native issue dependencies; the `blocked_by` API expects the
blocker's database ID (`gh api ... --jq .id`), not its issue number. If native sub-issues or
dependencies are unavailable, use `Part of #<map>` and `Blocked by: #<issue>` in the body.
The frontier is open, unblocked and unassigned child tickets. Assign a ticket before working
it; when resolved, comment with the answer, close it and add a short linked decision to the
map.
