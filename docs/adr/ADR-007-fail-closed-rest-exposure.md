# ADR-007: Fail-Closed REST Exposure

## Status
Accepted

## Context
Unintended API surface expansion is a common security failure mode.

## Decision
REST endpoints are fail-closed by default and must be explicitly enabled by deployment configuration.

## Consequences
- Reduces accidental endpoint exposure risk.
- Makes REST enablement an intentional deployment choice.
- Requires clear deployment guidance for environments that need REST access.
