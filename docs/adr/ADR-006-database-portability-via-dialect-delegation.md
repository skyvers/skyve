# ADR-006: Database Portability via Dialect Delegation

## Status
Accepted

## Context
Skyve supports multiple relational databases with differing SQL semantics, schema evolution behavior, and spatial capabilities.

## Decision
Vendor-specific persistence behavior is encapsulated through dialect and delegate layers while preserving a stable platform API.

## Consequences
- Enables cross-database portability.
- Localizes vendor differences and evolution logic.
- Requires ongoing dialect coverage tests and migration validation.
