# ADR-005: Multi-Renderer View Interpretation

## Status
Accepted

## Context
Skyve serves multiple UI pipelines (Faces, SmartClient, Vue, Flutter) and needs consistent behavior across renderer targets.

## Decision
A single metadata view model is interpreted by multiple renderers with UX/UI-specific route selection.

## Consequences
- Improves reuse and consistency of declarative UI intent.
- Requires renderer-parity testing and compatibility controls.
- Renderer-specific behavior must remain bounded and documented.
