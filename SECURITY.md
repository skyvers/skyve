# Security Policy

## Supported Versions

The following Skyve versions are currently being supported with security updates.

| Version | Supported          |
| ------- | ------------------ |
| 9.x.x   | :white_check_mark: |
| 8.x.x   | :white_check_mark: |
| 7.x.x   | :x:                |
| 6.0.x   | :x:                |
| 5.0.x   | :x:                |
| < 5.0   | :x:                |

_Note: only customers covered by a support agreement will receive patches for major versions less than the current major version (9.x.x)._

## Known Mitigations

### CVE-2026-0603 (Hibernate ORM)

- Skyve uses Hibernate `5.6.15.Final`, which is in the affected range for `CVE-2026-0603`.
- Skyve includes a local mitigation that back-ports safe escaping for inline bulk-id IN-clause handling.
- Skyve also blocks explicit configuration of Hibernate's vulnerable OR-clause inline bulk-id strategy (`org.hibernate.hql.spi.id.inline.InlineIdsOrClauseBulkIdStrategy`) during persistence bootstrap.

## Reporting a Vulnerability

If a vulnerability is found in Skyve, please use the [Contact Us](https://skyve.org/contact) 
page on our website to let us know either via email or on Slack.

Once confirmed, we will contact you to let you know which version of Skyve will resolve the 
vulnerability and when you can expect it to be resolved. We will contact you again once the 
vulnerability has been patched and is available.
