# ADR-004: Conversation-Oriented Web Interaction Model

## Status
Accepted

## Context
Skyve UI interactions include deep navigation and tab-local workflows that are difficult to model in purely stateless request patterns.

## Decision
The web interaction model is conversation-oriented. Server-side tab-local state is preserved and restored per conversation.

## Consequences
- Enables rich multi-step interactions and n-level zoom patterns.
- Requires robust cache/session integrity controls.
- Requires explicit operational handling for conversation cache capacity and failure modes.
