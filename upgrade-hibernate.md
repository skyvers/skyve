# Upgrading Hibernate (5.6 → 6.x)

## Background

The Hibernate version is locked at **5.6.15.Final** because byte-buddy must stay at
`1.12.18` in lock step with hibernate (see root `pom.xml` comment). This in turn
blocks Mockito inline mocking (required for `mockStatic`) on Java 17, which is why
`LoginServletTest` and related tests are `@Disabled`.

Unblocking this requires upgrading Hibernate. Upgrading Mockito to 5.x (which makes
the inline mock maker the default and removes the need for `mockito-inline`) then
follows naturally.

---

## Should we go to Hibernate 6 or 7?

### Hibernate 7 requires Jakarta EE 11

Hibernate 7 requires Jakarta Persistence 3.2 (Jakarta EE 11). Moving from EE 10
to EE 11 pulls in mandatory upgrades across the rest of the stack:

| Component | Current | EE 11 version required | Notes |
|---|---|---|---|
| `jakartaee-api` | 10.0.0 | 11.0.0 | |
| PrimeFaces | 13.0.10 | 15.x | (14 never existed) |
| OmniFaces | 4.3 | 5.x | XHTML namespace changes required |
| Weld | 5.1.2.Final | 6.0.4.Final | |
| Spring Framework | 6.2.17 | 7.x | **Requires Java 21** |
| Spring Security | 6.5.9 | 7.0.5 | |

The Spring Framework 7 requirement to move to **Java 21** (from the current Java 17
baseline) significantly widens the scope.

### Recommendation: target Hibernate 6

All current EE 10 libraries (PrimeFaces 13, OmniFaces 4, Weld 5, Spring 6) stay as-is.
Java version is unchanged. The Hibernate-specific migration work is the same either
way — H5→H6 is where all the breaking changes are. H6→H7 can be a follow-up once a
Java 21 baseline is decided.

---

## How Hibernate is assembled in Skyve

Skyve uses **native Hibernate bootstrap** (not Spring's `LocalSessionFactoryBean`).
The entire assembly happens in
`skyve-ext/src/main/java/org/skyve/impl/persistence/hibernate/AbstractHibernatePersistence.java`:

1. `StandardServiceRegistryBuilder` + `AvailableSettings` — builds the service registry
2. `MetadataSources` — registers generated `*_orm.hbm.xml` files (one per customer/module)
3. `MetadataBuilder` — registers a custom `Integrator` and Hibernate event listeners
4. `SessionFactoryBuilder` — builds the static `SessionFactory`

Domain registration is **entirely via generated hbm.xml files**
(e.g. `skyve-war/src/generated/java/modules/admin/domain/admin_orm.hbm.xml`),
not JPA annotations or package scanning. Each hbm.xml begins with `<typedef>`
declarations for the 9 custom UserTypes.

---

## What is incompatible (by severity)

### VERY HIGH — Spatial dialect rewrite (~20+ classes)

`hibernate-spatial` was **merged into `hibernate-core`** in Hibernate 6 and its entire
SPI was replaced. The old `SpatialDialect` base class is gone. All of Skyve's dialect
hierarchy must be rewritten:

- `H2SpatialDialect`, `H213SpatialDialect`, `H222SpatialDialect`
- `MySQL5InnoDBSpatialDialect`, `MySQL56InnoDBSpatialDialect`,
  `MySQL8InnoDBSpatialDialect` + `mysqlbugfix` variants
- 9 PostgreSQL version-specific dialects
- `SQLServer2008SpatialDialect`, `SQLServer2012SpatialDialect`
- Delegate classes: `MySQLSpatialDialectDelegate`,
  `PostgreSQLSpatialDialectDelegate`, `SQLServerSpatialDialectDelegate`,
  `AbstractH2SpatialDialect`
- Unique-delegate classes: `NoOpUniqueDelegate`, `NullsDistinctUniqueDelegate`,
  `SQLServer2008NullTolerantUniqueDelegate`

Hibernate 6 also **collapsed DB-version-specific dialect classes** (e.g. all
PostgreSQL 8/9/10/etc. variants collapse into one `PostgreSQLDialect`). This is a
**breaking config change for deployed Skyve applications** — every app's `skyve.json`
dialect class name will need updating.

### HIGH — UserType API (9 classes)

The `UserType` interface changed substantially in H6 — `nullSafeGet`/`nullSafeSet`
signatures changed, `sqlTypes()` replaced by `getSqlType()`, and `LiteralType` was
reworked. All of these need rewriting:

- `DateOnlyUserType`, `DateTimeUserType`, `TimeOnlyUserType`, `TimestampUserType`
- `Decimal2UserType`, `Decimal5UserType`, `Decimal10UserType`
- `EnumUserType`
- `OptimisticLockUserType`

Type bindings are declared via `<typedef>` in generated hbm.xml files and via
`@Type(type="...")` in generated Java domain classes (emitted by `JPADomainGenerator`).
Both need updating.

### HIGH — DDL and schema evolution

`DDLDelegate` and `generateDDL()` in `AbstractHibernatePersistence` use
`SchemaExport`/`SchemaUpdate` — these APIs changed in H6. `DDLDelegate` also uses
`org.hibernate.mapping.Table` and `Column` directly for schema diffing.

### MEDIUM — Session / Query API

- `Session.delete(entityName, bean)` — **removed in H6**, replaced by `Session.remove()`
- `Query.getReturnAliases()` — removed in H6 (used in `HibernateQueryDelegate`)
- `org.hibernate.hql.internal.ast.QuerySyntaxException` — package moved
- `HibernateSQL.java` uses `BigDecimalType`, `BooleanType`, `StringType` etc. from
  `org.hibernate.type.StandardBasicTypes` — renamed/restructured in H6
- `org.hibernate.type.Type` introspection in `HibernateListener` (event callbacks
  that drive content indexing)
- `Session.createQuery(String)` return type is more strictly typed in H6; all call
  sites in `HibernateQueryDelegate` and `AbstractHibernatePersistence` need auditing

### MEDIUM — Bootstrap API

- `LoadedConfig.baseline()` — verify still present in H6
- Custom `Integrator` registration — API may differ
- Hibernate event listener registration (`EventType` constants / `EventListenerRegistry`)
- `org.hibernate.internal.SessionImpl` direct JDBC connection access — find H6
  equivalent (e.g. `Session.doWork(Work)`)

### MEDIUM — Generator update

- `JPADomainGenerator` emits `@Type(type="org.skyve...")` — H6 requires
  `@Type(SomeType.class)`
- `OverridableDomainGenerator` emits hbm.xml `<typedef>` blocks — still supported
  in H6 but verify
- After generator changes, run `mvn skyve:generateDomain` from `skyve-war/` and
  re-verify all generated files compile under H6

---

## Migration phases

### Phase 1 — Dependency swap *(hours — opens compile error list)*

In root `pom.xml`:
- `hibernate.version` → `6.6.x.Final` (groupId `org.hibernate.orm`)
- Remove `hibernate-entitymanager` (merged into `hibernate-core` in H6)
- Remove `hibernate-spatial` artifact (merged into `hibernate-core` in H6)
- `byte-buddy.version` → `1.14+` (H6 aligned)
- `mockito.version` → `5.x` (inline mock maker is default; `mockito-inline`
  artifact no longer needed)

Run `mvn -pl skyve-ext,skyve-core -am compile` — the compile error list becomes
the task backlog.

### Phase 2 — UserType rewrite *(1–2 days)*

Rewrite all 9 UserType classes to the H6 `UserType<J>` generic interface.

### Phase 3 — Spatial dialect rewrite *(1–2 weeks — hardest)*

Rewrite all ~20 dialect/spatial classes against H6's new dialect + spatial SPI.
Decide on dialect consolidation strategy and document the customer migration path
for `skyve.json` dialect class names.

Note: this phase likely cannot be fully automated. The upstream SPI was replaced
with a different design (TypeContributor/FunctionContributor/SQM) and requires
understanding both systems simultaneously. Runtime validation against real databases
with geometry columns is essential.

### Phase 4 — DDL / SchemaExport *(2–3 days)*

Update `DDLDelegate` and `generateDDL()` to H6 `SchemaExport`/`SchemaUpdate` API.
Validate alter-column DDL against real schemas for each supported database.

### Phase 5 — Bootstrap API *(1–2 days)*

Update `AbstractHibernatePersistence` bootstrap chain, `Integrator` registration,
event listener registration, `SessionImpl` connection access.

### Phase 6 — Session / Query API *(2–3 days)*

Fix removed/changed APIs in `AbstractHibernatePersistence`,
`HibernateQueryDelegate`, `HibernateSQL`, and `HibernateListener`.

### Phase 7 — Generator + regeneration *(1–2 days)*

Update `@Type` emission in `JPADomainGenerator`; run `mvn skyve:generateDomain`.

### Phase 8 — Mockito cleanup *(hours)*

Mockito 5 includes the inline mock maker by default:
- Remove or update the commented-out `mockito-inline` block in root `pom.xml`
- Remove `@Disabled("Until byte buddy can be uplifted...")` from `LoginServletTest`
  and any other disabled tests
- Verify those tests pass

---

## Automation potential

| Phase | Automatable by AI? | Notes |
|---|---|---|
| 1 — Dependency swap | Yes | Mechanical pom.xml edit |
| 2 — UserType rewrite | Yes | Well-documented API delta; compiler-driven |
| 3 — Spatial dialects | **Partially** | AI can draft structure; human must validate geometry handling per DB |
| 4 — DDL/SchemaExport | Partially | AI adapts API calls; human validates against real schemas |
| 5 — Bootstrap API | Yes | Mechanical API translation |
| 6 — Session/Query API | Yes | Mechanical API translation |
| 7 — Generator + regen | Yes | Straightforward once UserTypes are settled |
| 8 — Mockito cleanup | Yes | Trivial |
| Integration testing | No | Requires real databases and a running Skyve WAR |

Approximately 60–70% of the work by volume is automatable. The spatial rewrite and
integration testing — which represent the bulk of the *risk* — require human domain
knowledge and a real test environment.

---

## Effort estimate

| Area | Difficulty | Estimate |
|---|---|---|
| Dependency swap | Low | Hours |
| UserType rewrite (9 classes) | Medium | 1–2 days |
| **Spatial dialect rewrite (~20 classes)** | **Very High** | **1–2 weeks** |
| DDL / SchemaExport | Medium | 2–3 days |
| Bootstrap API | Medium | 1–2 days |
| Session / Query API | Medium | 2–3 days |
| Generator + regeneration | Medium | 1–2 days |
| Mockito cleanup | Low | Hours |
| Integration testing + smoke | High | 1 week |

**Overall: ~4–6 weeks of focused engineering on a dedicated branch.**

This migration cannot be done incrementally — Hibernate 5 and 6 are binary
incompatible. It must be completed on a single branch and merged all at once.

---

## Verification checklist

1. `mvn -pl skyve-core,skyve-ext -am compile` — zero errors
2. `mvn -pl skyve-core -am test` — core unit tests pass
3. `mvn -pl skyve-ext -am test` — persistence unit tests pass (H2-backed)
4. `mvn -pl skyve-war -am package` — full WAR including generated domain
5. Manual smoke: start a Skyve WAR, confirm `SessionFactory` boots, `generateDDL()`
   produces valid SQL, a simple create/read/update/delete cycle works, geometry
   columns are created and queried correctly
6. `LoginServletTest` and other previously `@Disabled` Mockito tests now pass

---

## Scope boundaries

- **Not included**: Hibernate Search (not used), Envers (not used), JPA
  `persistence.xml` (not used — Skyve uses native bootstrap only)
- **hbm.xml format**: Keep as-is; Hibernate 6 still supports hbm.xml. Do not
  convert to annotations or JPA XML as part of this migration.
- **Dialect naming**: The consolidation of DB-version-specific dialect classes in
  H6 is a **breaking change for downstream Skyve applications**. Document the
  mapping from old to new class names in release notes.

## WildFly Jakarta EE 11 Support

As of WildFly 39.0.1.Final (February 2026):

| Distribution | EE Version | Java Requirement |
|---|---|---|
| Standard WildFly 36–39 | Jakarta EE 10 | Java 11+ (Java 21 LTS recommended) |
| WildFly Preview 36–39 | Jakarta EE 11 (tech preview) | Java 11+ (Java 21 LTS recommended) |

### Key Facts

- **No released version of standard WildFly implements Jakarta EE 11** as a stable GA feature. EE 11 support is confined to the **WildFly Preview** distribution, which is explicitly described as a tech-preview / early look, across all versions from WildFly 32 through the current 39.
- WildFly itself does **not require** Java 21 — Java 11 is the minimum, and Java 21 is just the recommended LTS. The Java 21 constraint for EE 11 comes from upstream libraries (Spring 7, Hibernate 7, etc.), not from WildFly itself.
- For production use: targeting EE 11 on WildFly today means using **WildFly Preview only**, which is not production-ready. Standard WildFly EE 11 graduation has not been announced and is likely WildFly 40 or later.

### Practical Implication for the Hibernate 7 / EE 11 Path

WildFly doesn't block it at the runtime level (Java 11+ is sufficient), but Spring 7 (required by Hibernate 7) mandates Java 21, and EE 11 stable server support does not yet exist in standard WildFly.