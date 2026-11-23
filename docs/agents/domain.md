# Domain Docs

This is a single-context Emacs package. Domain terms and durable design decisions are
recorded only when work settles them; do not create glossary or ADR files speculatively.

## Before domain work

- Read `.project/tasks/project-context.org` for the established goals, non-goals and
  decisions.
- Read `README.org` for the public behavior and `docs/agents/WORKFLOW.md` for development
  rules.
- If `GLOSSARY.md` or `docs/adr/` exists, read the entries relevant to the ticket. Their
  absence is normal; proceed without calling attention to it.

## Record a decision

Use the `domain-modeling` skill when the work settles a term or durable architectural
decision. Create `GLOSSARY.md` or `docs/adr/` only when a real term or decision needs a
lasting reference. Keep each definition and decision in one authoritative place.

Use established terms in issues, tests and code. If a proposed change conflicts with an
existing decision, surface the conflict before changing that decision.
