# ADR-002: Mandatory generateDomain Validation Gate

## Status
Accepted

## Context
Metadata errors can be structural, cross-module, or security-related and are expensive to detect late in runtime or production.

## Decision
All build and release paths must run full metadata validation through the generateDomain workflow and fail the build on validation errors.

## Consequences
- Prevents deployment of structurally invalid metadata.
- Makes generation a mandatory CI quality gate.
- Slightly increases build-time requirements but reduces runtime fault risk.
