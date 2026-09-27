# Migrating to Jakarta EE 11

Skyve now compiles against Jakarta EE 11. Use Java 17 or later and an EE 11 server;
the standard WildFly 41 distribution provides EE 11.

The framework aligns its standalone/test implementations with EL 6 (Expressly 6),
Faces 4.1 (Mojarra 4.1), CDI 4.1 (Weld 6), and REST 4 (Jersey 4). OmniFaces 5
requires Faces 4.1. The Java compilation baseline remains 17.
The parent also manages Persistence API 3.2 and JBoss Logging 3.6.1 so older
transitive dependencies cannot override the EE 11 API or Weld 6's logging contract.
Skyve retains its native Hibernate integration; this migration does not switch
applications to container-managed JPA or change their persistence configuration.

## REST and CDI

The generic REST resource obtains its HTTP response directly from
`EXT.getHttpServletRespsone()` during each endpoint invocation. This delegates to
the response established by `RequestLoggingAndStatisticsFilter`; keep that filter
mapped before the REST servlet. No servlet-response CDI producer or injected
response field is needed. The endpoint paths, permissions and Java method
signatures are unchanged.

RESTEasy 7.0.3 converts legacy `@Context` fields into CDI injection points. Its
context producers do not supply `HttpServletResponse`, so Skyve avoids that
injection point.

REST 4 allows `SseEventSink.close()` to throw `IOException`. Skyve handles it during
stream cleanup and still deregisters the receiver.

## Responsive form widths

Mojarra 4.1.13 evaluates panel-group `styleClass` expressions during both opening
and closing markup. Skyve's responsive form getter previously advanced the column
cursor on each evaluation, causing fields to inherit the label column's width
(for example, 4 grid units instead of 8). It now reuses each component's calculated
style within the current Faces request. A new request recalculates the layout so
visibility changes still affect column placement. No theme or PrimeFaces upgrade
is needed for this correction.

## Server migration

Preserve the application's external JSON configuration, datasource, JDBC driver,
content directory and TLS configuration when preparing a new server.

Install the rebuilt `skyve-content` ZIP in the content directory's `addins` subdirectory.
