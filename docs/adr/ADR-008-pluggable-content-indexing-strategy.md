# ADR-008: Pluggable Content Indexing Strategy

## Status
Accepted

## Context
Search/indexing backends can vary by customer and environment (for example Lucene or Elasticsearch), while application behavior should stay stable.

## Decision
Content indexing and storage providers are selected through a stable SPI boundary and can be swapped by configuration.

## Consequences
- Supports backend flexibility without changing business metadata.
- Requires consistent SPI contracts and integration tests.
- Operational teams must understand backend-specific scaling and recovery behavior.
