---
name: implement
description: "Implement a piece of work based on a spec or set of tickets."
disable-model-invocation: true
---

Implement the work described by the user in the spec or tickets.

If the user passes a ticket reference, read it from the configured issue tracker and use its
title and acceptance criteria as the scope. Resolve ambiguous references before editing.

Follow `AGENTS.md` and `docs/agents/WORKFLOW.md`. Use the `tdd` skill for behavior changes;
derive test seams from the ticket or plan and ask only when the interface decision is
unresolved.

Run the narrowest relevant checks while implementing. Before reporting completion, update
required docs, run the repository's full check command, and summarize any check that could
not run.

Leave Git commits, pushes, merges and PRs to the coordinator unless the repository workflow
assigns them to this role.
