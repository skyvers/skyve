# ADR-001: Metadata-First System Definition

## Status
Accepted

## Context
Skyve applications are currently built from XML metadata describing customers, modules, documents, views, routes, security, and queries. The platform supports both runtime interpretation and generated domain artifacts.

## Decision
Business capabilities are defined metadata-first. Runtime behavior and generated domain code must remain aligned to metadata as the single source of truth.

## Consequences
- Reduces boilerplate and centralizes business intent.
- Requires strong validation and metadata governance.
- Generation and runtime paths must both consume equivalent metadata semantics.
