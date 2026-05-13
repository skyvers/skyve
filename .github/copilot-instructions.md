# Copilot Coding Instructions

These instructions apply to cloud and local Copilot coding tasks in this repository.

## Required Reading Order

1. Read `agents.md`.
2. Read `docs/learnings.md`.
3. If the task is test-coverage work, also read `docs/coverage-plan.md` and `docs/test-patterns.md`.

## Formatting Rules

- Do not leave generated code with broken indentation.
- Match the surrounding file's formatting exactly.
- Java source uses tab-based leading indentation in this repository.
- Do not mix tabs and spaces for leading indentation.
- Treat the Javadoc standards in `agents.md` as mandatory for Java API changes.
- Whenever you touch public/protected Java API (especially in `skyve-core`), add or update Javadoc in the same change.
- If you touch Java in `skyve-war`, run:
  - `mvn -Pspotless-with-download -DskipTests spotless:apply`
- For Java touched outside `skyve-war`, perform a whitespace cleanup pass so indentation is consistent with nearby code.

## Coverage Uplift Rules

- Prefer small, focused tests with descriptive names and one behaviour per test.
- Keep imports ordered and remove unused imports.
- Never hand-edit generated sources under `src/generated/java` or `src/generatedTest/java`.
- If formatting and behaviour changes are both needed, fix formatting first.
