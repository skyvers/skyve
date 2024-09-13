# Contributing to Skyve

Skyve is free and open source and we always are excited to receive contributions from our community. There are many ways to contribute, from writing tutorials, providing translations into new languages, improving the documentation or writing code to fix bugs or add new features to the platform itself.

## Issues and Bugs

If you find a bug in the source code or a mistake in the documentation, you can help us by creating a [GitHub Issue](https://github.com/skyvers/skyve/issues/new/choose).

If you have a question on using or getting started with a Skyve project, please see our [Getting Started](https://skyve.org/getting-started) page and watch the videos, follow the [tutorial](https://skyvers.github.io/Aged-care/), use the [skyve tag on StackOverflow](https://stackoverflow.com/tags/skyve) or join our [Slack channel](https://join.slack.com/t/skyveframework/shared_invite/enQtNDMwNTcyNzE0NzI2LTRkMWUxZDBlZmFlMmJkMjQzYWMzYWQxMmQzYWQ1ZTdlODNkNjRlYzVhYjFmMmQ4NTlhYWY4MjNhMGVkZGNlMjY).

## Feature Requests

If there is something you feel is missing from Skyve, we want to hear your ideas. We are constantly trying to improve the platform to make it as versatile as possible, and many of the features in Skyve exist because our users saw a need. 

Create a new [GitHub Issue](https://github.com/skyvers/skyve/issues/new/choose) which describes the feature, how it would be useful to you, and how it should work. The core team will discuss with you in the issue, and if you would like to contribute you can then start working on a Pull Request.

## Pull Requests

If you have contributed to a feature or addressed an issue, please [create a Pull Request](https://docs.github.com/en/pull-requests/collaborating-with-pull-requests/proposing-changes-to-your-work-with-pull-requests/creating-a-pull-request). Don't forget to link the [Pull Request to the related issue](https://docs.github.com/en/issues/tracking-your-work-with-issues/linking-a-pull-request-to-an-issue).

If you are creating a branch to address an issue, please name it in the form of `issue-##-name-of-issue`.

If you have been assigned a Trello card for this feature/issue, you can link the relevant PR to the [card](https://blog.trello.com/github-and-trello-integrate-your-commits).

## Skyve Development Setup

This Skyve repository is intended for developers with existing experience with Java projects and development environments. If you are trying to start a new Skyve project, please see our [Getting Started](https://skyve.org/getting-started) page instead.

## Coding Guidelines

Keep dependencies to a minimum
Keep performance in front of mind
Keep thread-safety in front of mind
Ensure startup time is low - aim for 200 millis for serverless cold starts.
Do not hard code javaee/spring resources with annotations, use web.xml to remain configurable.
Use json configuration as a single configuration point if possible.
Aim for null-safety over Optional.
Remain agnostic of DI - no DI in Skyve (except faces)
Usually 1 exit from a function called result
Watch boxing and unboxing - be explicit
Use functional coding to simplify (dont overuse) - stream processing.
Functional coding introduces more run overhead, invokedynamic jvm instruction, and work at the call site / capture point and class engineering at first run.
Functional is good for parallelism and scale-out.
Call getters once
Don't use String.format() unless you are using the formatting features - 50 times slower than StringBuilder.
skyve-core - enough to power metadata reading and interpretation and domain generation
skyve-ext - Bring in other dependencies, caching, reporting, addins etc.
skyve-web - The web container and faces code.
