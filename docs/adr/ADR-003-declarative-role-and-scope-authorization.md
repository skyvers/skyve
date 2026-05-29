# ADR-003: Declarative Role and Scope Authorization

## Status
Accepted

## Context
Manual endpoint-by-endpoint authorization is error-prone and difficult to keep consistent across web, query, and persistence paths.

## Decision
Authorization is modeled declaratively through roles, document permissions, and scope (global/customer/data-group/user), and enforced centrally across platform access paths.

## Consequences
- Reduces omission-based security defects.
- Requires deliberate permission modeling.
- Security semantics become part of metadata and compatibility contracts.
