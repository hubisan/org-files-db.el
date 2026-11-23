---
name: implement-spec
description: "Implement a settled multi-ticket specification in this repository."
disable-model-invocation: true
---

Implement the tickets linked to the supplied specification as one coordinated effort.

## Workflow

1. Read the spec and its ticket graph. Use the issue-tracker rules in `docs/agents/issue-tracker.md`.
2. Follow `docs/agents/WORKFLOW.md` for role selection, model routing, tests, review,
   correction limits and Git operations. Reuse settled requirements; do not re-plan them.
3. Work one unblocked ticket at a time by default. Parallelize only independent tickets
   with disjoint files and useful overlap in time.
4. Integrate each completed ticket into the feature branch and update its tracker state
   according to the configured workflow. Keep each change traceable to its ticket.
5. Review the integration diff against the spec, run the final full check, and open one PR
   for the feature when the repository workflow calls for one.

Stop and report a blocker when ticket dependencies are unclear, implementation would
change the agreed scope or architecture, or the required checks cannot be run. The main
session owns GitHub actions and the integration branch unless the user or repository
workflow explicitly delegates them.
