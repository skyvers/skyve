# Skyve Architecture & Design

## 1. Why Skyve Exists

AI can now generate code at extraordinary speed. But speed without structure produces sprawl — thousands of lines of procedural code that no human (and no AI) can confidently reason about, validate, or secure. The bottleneck in 2026 is not writing code; it's **knowing that what was written is correct, secure, and maintainable**.

Skyve was designed for exactly this constraint — long before AI agents made it urgent. The core insight: **the nature of your data implies the system required to maintain it**. Instead of generating mountains of code to handle persistence, security, UI, search, and content for every entity, you *declare* a domain model in concise XML metadata. Skyve interprets that declaration at runtime — validating it exhaustively, enforcing security universally, rendering UIs across multiple targets, and managing schema evolution — all from that single source of truth.

This is not a legacy low-code platform. Skyve is a **metadata engine with a compiler-grade validation pipeline**. Every declaration is statically verified before deployment. When you need custom behaviour, you drop into Java with full framework support — but only at the exact point of divergence, not across the entire surface area.

The result: whether a human or an AI writes the metadata, the system guarantees correctness at build time, security at runtime, and maintainability over the life of the application.

### Design Principles

Six principles govern every architectural decision:

| Principle | What It Means | Developer Impact |
|-----------|--------------|-----------------|
| **Parsimony** | Only the minimum information required to declare a system should be needed to build it. Intelligent defaults fill the gaps. | You write less. Far less. No HTML, no JavaScript, no SQL — unless you choose to. |
| **Authority** | Each element of declaration is authoritative — it exists in exactly one place. | No contradictions. No shotgun changes across layers. A single business concept lives in a single document package. |
| **Independence** | Application declarations are independent of platform, database, device, and browser. | Move between databases, switch rendering targets, or deploy to new infrastructure without refactoring. |
| **Security** | Best-practice security cannot be compromised by simple error or omission. Permissions are declarative, not programmatic. | You cannot accidentally expose data. Row-level scoping, CRUD permissions, and action privileges are enforced transparently. |
| **Scalability** | Declarations must not limit future growth. Best-practice data techniques are applied transparently. | Normalised schemas, proper transaction demarcation, and connection management happen automatically. |
| **Maintainability** | Changes are made with minimum effort and minimum risk, backed by extensive validation. | Skyve validates the entire metadata model on every build. Change impact analysis is immediate and exhaustive. |

A seventh implicit principle — **Exit** — ensures that all data remains accessible in open formats and that the system declaration is self-describing, enabling replacement or incorporation into other systems at end-of-life.

### What This Means In Practice

Whether a human developer or an AI agent is building the application:

1. **Declares a document** (business entity) in XML — attributes, relations, constraints, and a business key.
2. **Gets a complete application immediately** — Skyve generates default list views, edit views, CRUD actions, security enforcement, full-text search, and persistence with no additional effort.
3. **Overrides only where needed** — custom views, bizlet lifecycle hooks, actions, and interceptors refine behaviour at the precise point of divergence.
4. **Never maintains generated code** — domain classes are regenerated on every metadata change; custom code lives in bizlets, actions, and extension classes that the framework calls.
5. **Deploys once for all tenants** — customer overrides allow any metadata element to be tailored per tenant without forking the codebase.

The platform integrates 80+ open-source libraries (Hibernate, Spring Security, PrimeFaces, Lucene, Quartz, JTS, Tika, and many more) behind a high-level API. A single developer — or a single AI agent — working with Skyve produces the same robust, secure output that would traditionally require a team with persistence, security, frontend, and DevOps specialists.

### How Skyve Differs

| Code-Heavy Approach (human or AI-generated) | Skyve Approach |
|---------------------|---------------|
| Write entity classes, then DAO layers, then DTOs, then controllers, then views | Declare a document; everything else is derived |
| Hand-code security checks at every endpoint | Declare roles with document-scoped CRUD + action permissions; enforcement is automatic and universal |
| Build separate mobile and desktop UIs | One view metadata declaration renders across four pipelines (Faces, SmartClient, Vue, Flutter) |
| Write migration scripts when schemas change | Skyve dialect extensions detect and coerce schema changes automatically |
| Content management is a separate system | Content is a first-class attribute type — transactionally linked, text-extracted, and federated-searchable |
| Spatial is a bolt-on library | Geometry is a primary attribute type with native database support, query predicates, and map rendering built in |
| Multi-tenancy requires custom middleware | Every Skyve application is multi-tenant from the first line of metadata, with per-customer override of any element |
| Validation happens at runtime (if at all) | Skyve validates the *entire* metadata model at build time — cross-document, cross-module, cross-customer |

### Static Validation as a Development Discipline

Skyve does not wait until runtime to discover problems. **Domain generation (`generateDomain`) is a mandatory step in the development cycle** — and it validates the *entire* metadata model exhaustively before producing any code:

- Every document attribute type is checked for consistency with its converter, validator, and widget.
- Every binding expression in views, queries, conditions, and actions is resolved against the document model.
- Every role permission is confirmed to reference real documents and actions.
- Cross-module references, customer overrides, and inheritance hierarchies are all validated as a unit.
- If validation fails, no code is generated — the developer gets precise, actionable error messages.

This means the application is always in a **known-correct state**. There is no "it compiles but does it work?" gap. Domain generation acts as a continuous integration gate that catches structural errors, broken references, and security misconfigurations before a single request is served. Developers run `generateDomain` reflexively — after every metadata change — and trust that a clean generation means a deployable system.

### Built for AI-Assisted Development

Skyve's architecture makes it exceptionally well-suited to AI-driven development:

- **Minimal code surface** — most application capability is declared in concise XML metadata, not sprawling procedural code. AI agents produce less output, stay within context windows, and make fewer mistakes.
- **Immediate static feedback** — AI-generated metadata is validated instantly by `generateDomain`. Errors are structural and deterministic, giving AI agents a clear signal to self-correct without guesswork.
- **Constrained decision space** — the metadata schema limits what can be expressed, dramatically reducing the search space for valid solutions compared to freeform code generation.
- **Open source and well-documented** — AI models have trained on Skyve's public codebase, dev guide, and cookbook. Pattern recognition is strong; hallucination rates are low.
- **High leverage per token** — a 20-line document XML declaration produces a fully functional entity with persistence, UI, security, search indexing, and API exposure. The ratio of declared intent to delivered capability is orders of magnitude higher than traditional frameworks.

The result: AI agents can scaffold entire modules, add documents, wire up relations, and define security roles — then validate their own output by running `generateDomain` and inspecting the result. The tight declare → validate → deploy loop is a natural fit for agentic workflows.

### Security by Declaration, Not by Discipline

In traditional frameworks, security is opt-in — developers must remember to check permissions at every endpoint, filter queries by tenant, validate payloads against allowed fields, and prevent forced browsing. A single omission creates a vulnerability.

Skyve inverts this model. Security is **declarative, universal, and impossible to accidentally bypass**:

- **Role-based CRUD + scope** — each role declares exactly which documents it can Create, Read, Update, and Delete, scoped to Global, Customer, DataGroup, or User level. This is enforced transparently on every persistence operation, every query, and every UI interaction.
- **Action permissions** — custom actions are only executable if the user's role explicitly grants execute privilege. The UI won't render the button; the server won't accept the request.
- **Payload shaping** — REST and UI payloads are shaped by the framework according to the user's permissions. Fields the user cannot see are never serialised. Fields the user cannot modify are ignored on input. There is no over-posting vulnerability.
- **Access vectors and forced-browsing prevention** — `UserAccess` keys are computed from menus, roles, routes, and view metadata by `AccessProcessor`. `EXT.checkAccess()` rejects any request that doesn't match a declared access vector — meaning users cannot reach documents or actions by guessing URLs.
- **Row-level isolation** — `bizCustomer`, `bizDataGroupId`, and `bizUserId` fields on every bean enforce tenant and ownership boundaries at the persistence layer. Queries are automatically filtered; no developer intervention required.
- **Fail-closed by default** — REST endpoints are blocked (`ForbiddenFilter`) unless explicitly enabled. Security headers (CSP, X-Frame-Options, MIME-sniffing, Referrer-Policy) are applied to every response. CSRF tokens are rotated per request.

The net effect: a developer cannot accidentally ship an unsecured application. The only way to weaken security is to deliberately override the framework's protections — and even then, metadata validation will flag many unsafe configurations.

### The Business Component Model

Skyve organises applications as **modules** containing **documents**. A module is a self-contained reusable segment of capability (presented as a menu section). A document is a business concept — like an invoice, a timesheet, or a contact — that owns its attributes, views, actions, bizlets, queries, and security declarations in a single package.

This *business component* approach means everything related to a concept lives together. When requirements change, you modify one package — and Skyve's validation confirms the change is consistent across the entire application.

### Conversation-Based Interactions

Unlike stateless REST-centric frameworks, Skyve manages **conversations** — server-side state representing a user's in-progress interaction within a single browser tab. Conversations support n-level zoom (navigating into related documents), with proper transaction demarcation at every level. The first-in-wins optimistic locking model prevents conflicting concurrent edits, and the conversation cache ensures that partial work is never lost.

This model gives developers rich transactional semantics without manual state management — the framework handles serialisation, session binding, conflict detection, and cache lifecycle.

---

## 2. Module Layout

Skyve is structured as a multi-module Maven project (Java 17, version 10.0.0-SNAPSHOT). Dependencies flow strictly downstream: `skyve-core` → `skyve-ext` → `skyve-web` → `skyve-war`.

```mermaid
graph TD
    core["skyve-core<br/><i>Contracts, metadata model,<br/>type system, binding, generation</i>"]
    ext["skyve-ext<br/><i>Persistence (Hibernate), security,<br/>jobs, reporting, content integration</i>"]
    web["skyve-web<br/><i>JSF/PrimeFaces rendering,<br/>servlet filters, REST endpoints</i>"]
    war["skyve-war<br/><i>Deployable WAR assembly<br/>+ reference application</i>"]
    content["skyve-content<br/><i>Content store abstraction:<br/>Lucene, Elastic, text extraction</i>"]
    plugin["skyve-maven-plugin<br/><i>DomainGenerator, scaffold,<br/>report compilation mojos</i>"]
    tools["skyve-tools<br/><i>Offline tooling: backup,<br/>restore, document creator</i>"]
    jar["skyve-jar<br/><i>Standalone fat-JAR packaging</i>"]
    coverage["skyve-coverage<br/><i>JaCoCo multi-module<br/>coverage aggregation</i>"]

    core --> ext
    content --> ext
    ext --> web
    web --> war
    web --> jar
    plugin --> core
    tools --> core
    coverage -.-> core
    coverage -.-> ext
    coverage -.-> web
```

| Module | Package Roots | Purpose |
|--------|--------------|---------|
| `skyve-core` | `org.skyve`, `org.skyve.metadata`, `org.skyve.impl`, `org.skyve.domain`, `org.skyve.util` | Public API, metadata interfaces, domain types, binding, expression evaluation, domain generation |
| `skyve-ext` | `org.skyve.impl.persistence`, `org.skyve.impl.security`, `org.skyve.job`, `org.skyve.report` | Hibernate persistence, security providers, job scheduling, reporting engines |
| `skyve-web` | `org.skyve.impl.web`, `org.skyve.impl.web.faces` | JSF managed beans, servlet filters, REST endpoints, push infrastructure |
| `skyve-content` | `org.skyve.content` | Content manager SPI, Lucene/Elastic implementations, text extraction |
| `skyve-war` | Application code | WAR assembly with Spring Security configuration |
| `skyve-maven-plugin` | `org.skyve.impl.tools` | Maven mojos for domain generation, scaffolding, report compilation |
| `skyve-tools` | `org.skyve.impl.tools` | Backup/restore utilities, document creator |
| `skyve-jar` | — | Standalone packaging (shaded JAR) |
| `skyve-coverage` | — | JaCoCo aggregation (no production code) |

---

## 3. Metadata-Driven Architecture

All application behaviour derives from XML metadata that is validated, converted to runtime objects, and optionally used to generate Java source code. This eliminates boilerplate and ensures static validation before deployment.

```mermaid
flowchart LR
    XML["XML Metadata<br/>(customer, module, document,<br/>view, query, role, router)"]
    CMeta["ConvertibleMetaData<br/>JAXB binding layer"]
    Runtime["Runtime Model<br/>(Customer, Module, Document,<br/>View, Role objects)"]
    DomGen["DomainGenerator<br/>validate() → generate()"]
    Java["Generated Java<br/>(domain classes, enums,<br/>HBM mappings)"]
    App["Deployed Application<br/>(runtime metadata + generated code)"]

    XML --> CMeta
    CMeta -->|"convert()"| Runtime
    Runtime --> DomGen
    DomGen --> Java
    Java --> App
    Runtime --> App
```

### Static Validation Pipeline

Validation occurs at two levels before any application runs:

1. **Isolated file validation** — each `ConvertibleMetaData` implementation validates and converts itself:
   - `CustomerMetaData` → runtime `Customer`
   - `ModuleMetaData` → runtime `Module`
   - `DocumentMetaData` → runtime `Document`
   - `ViewMetaData` → runtime `ViewImpl`
   - `ActionMetaData`, `BizletMetaData` → self-validate only
   - `Router` → validated route configuration

2. **Cross-metadata validation** — `ProvidedRepository` validates dependencies between metadata files:
   - `validateCustomerForGenerateDomain()` — customer-level consistency
   - `validateModuleForGenerateDomain()` — module references and roles
   - `validateDocumentForGenerateDomain()` — document relations and attributes
   - `validateViewForGenerateDomain()` — view bindings against document model

`DomainGenerator.validateCustomer()` iterates all modules, documents, and views (including per-UX/UI variants) calling these four methods, ensuring complete consistency before code generation.

Additionally, `ValidateMetaDataJob` runs asynchronously on startup to validate all customers in the background.

---

## 4. Layered Architecture

```mermaid
graph LR
    subgraph Contracts["Layer 1: Contracts (skyve-core)"]
        direction TB
        A1[Metadata Interfaces]
        A2[Domain Types]
        A3[Binding & Expressions]
        A4[Extension Point APIs]
    end

    subgraph Services["Layer 2: Runtime Services (skyve-ext)"]
        direction TB
        B1[Hibernate Persistence]
        B2[Security Providers]
        B3[Job Scheduler]
        B4[Reporting Engines]
    end

    subgraph Delivery["Layer 3: Web Delivery (skyve-web)"]
        direction TB
        C1[JSF/Faces Pipeline]
        C2[SmartClient Pipeline]
        C3[REST Endpoints]
        C4[Servlet Filters]
    end

    subgraph Assembly["Layer 4: Application (skyve-war)"]
        direction TB
        D1[Spring Security Config]
        D2[Application Modules]
        D3[Customer Overrides]
    end

    Contracts --> Services
    Services --> Delivery
    Delivery --> Assembly
```

Each layer depends only on layers to its left. Application code in `skyve-war` interacts exclusively through public APIs defined in `skyve-core`, with runtime services injected via CDI or Spring.

---

## 5. Request Lifecycle

```mermaid
sequenceDiagram
    participant B as Browser
    participant SS as Spring Security
    participant FC as Filter Chain
    participant R as Router
    participant WC as WebContext
    participant FV as FacesView / Servlet
    participant P as Persistence
    participant DB as Database

    B->>SS: HTTP Request
    SS->>SS: Authenticate (DB/OAuth/SAML/SPNEGO)
    SS->>FC: Authenticated request
    FC->>FC: UTF8 → Logging → SkyveFaces → CacheControl → SecurityHeaders
    FC->>R: Route resolution (module/document/action)
    R->>WC: Restore or create conversation
    WC->>FV: Dispatch to rendering pipeline
    FV->>P: begin() transaction
    P->>DB: Execute queries / persist
    DB-->>P: Results
    P-->>FV: Domain beans
    FV-->>WC: Cache conversation state
    WC-->>B: Rendered response (HTML/JSON)
    Note over WC,P: commit() on success, rollback() on error
```

### Filter Chain (Registration Order)

| Order | Filter | Scope | Purpose |
|-------|--------|-------|---------|
| 1 | `DelegatingFilterProxy` | `/*` | Spring Security authentication/authorization |
| 2 | `UTF8CharacterEncodingFilter` | `/*` | Force UTF-8 encoding |
| 3 | `RequestLoggingAndStatisticsFilter` | `/*` | Request logging, per-user web stats |
| 4 | `SkyveFacesFilter` | `FacesServlet`, `*.jsp` | Login gating, unsecured-path checks |
| 5 | `CacheControlFilter` | `FacesServlet` | `no-cache, no-store, must-revalidate` |
| 6 | `ResponseHeaderFilter` (NeverExpires) | `*.js`, fonts, `*.wasm` | One-year public cache |
| 7 | `ResponseHeaderFilter` (PublicCache) | `*.css`, images | `Cache-Control: public` |
| 8 | `ResponseHeaderFilter` (SecurityHeaders) | `/*` | CSP, X-Frame, Referrer-Policy, MIME-sniffing |
| 9 | `ForbiddenFilter` (RestFilter) | `/rest/*` | Fail-closed REST by default |

### Routing

`Router` (loaded from `router.xml`) contains ordered per-UX/UI `Route` lists. A `RouteCriteria` tuple (`WebAction`, `ViewType`, module, document, query, customer, data-group, user) is matched against routes; `selectRoute()` returns the first match. URL parameters `a`, `m`, `d`, `q`, `i` resolve to action, module, document, query, and bean identity respectively.

---

## 5a. Core Class Relationships

```mermaid
classDiagram
    class MetaData {
        <<interface>>
    }
    class SerializableMetaData {
        <<interface>>
        +Serializable
    }
    class NamedMetaData {
        <<interface>>
        +getName() String
    }
    class ConvertibleMetaData~T~ {
        <<interface>>
        +convert(String) T
    }
    class Customer {
        <<interface>>
        +getModules()
        +getRoles()
        +getInterceptors()
    }
    class Module {
        <<interface>>
        +getDocuments()
        +getQueries()
        +getRoles()
    }
    class Document {
        <<interface>>
        +getAttributes()
        +getViews()
        +getBizlet()
    }
    class View {
        <<interface>>
        +getWidgets()
        +getActions()
    }
    class ProvidedRepository {
        +validateCustomerForGenerateDomain()
        +validateModuleForGenerateDomain()
        +validateDocumentForGenerateDomain()
        +validateViewForGenerateDomain()
    }
    class Persistence {
        <<interface>>
        +begin()
        +commit()
        +rollback()
        +save(Bean)
        +delete(Bean)
        +retrieve(Document, String)
    }
    class Bizlet~T~ {
        +newInstance(T)
        +preSave(T)
        +postSave(T)
        +preDelete(T)
        +validate(T, ValidationException)
    }
    class Interceptor {
        <<interface>>
        +beforeSave(Bean)
        +afterSave(Bean)
        +beforeDelete(Bean)
        +afterDelete(Bean)
    }

    MetaData <|-- SerializableMetaData
    SerializableMetaData <|-- NamedMetaData
    NamedMetaData <|-- Customer
    NamedMetaData <|-- Module
    NamedMetaData <|-- Document
    NamedMetaData <|-- View
    MetaData <|.. ConvertibleMetaData
    ProvidedRepository --> Customer : resolves
    ProvidedRepository --> Module : resolves
    ProvidedRepository --> Document : resolves
    Persistence --> Document : queries
    Bizlet --> Document : extends
    Customer --> Interceptor : declares
```

---

## 6. Metadata Model

| Metadata Type | Runtime Class | Validation Path | Description |
|---------------|--------------|-----------------|-------------|
| Customer | `CustomerImpl` | `validateCustomerForGenerateDomain()` | Tenant root: modules, roles, interceptors, converters, branding |
| Module | `ModuleImpl` | `validateModuleForGenerateDomain()` | Groups documents, queries, roles, menu; supports cross-module refs via `DocumentRef` |
| Document | `DocumentImpl` | `validateDocumentForGenerateDomain()` | Business entity: attributes, relations, views, actions, bizlets, unique constraints |
| View | `ViewImpl` | `validateViewForGenerateDomain()` | Screen declaration (edit/create/list); per-customer and per-UX/UI overrides |
| Role | Aggregated into `UserImpl` | Module-level validation | Document permissions with CRUD + scope; union semantics across assigned roles |
| Query | `MetaDataQueryDefinitionImpl` | Module-level validation | Projected tabular result set with filter criteria relative to a driving document |
| Router | `RouterImpl` | Isolated conversion | UX/UI selectors, route lists, unsecured URL prefixes |

### Resolution Order

`ProvidedRepository` resolves metadata with customer-override-awareness: `FileSystemRepository` populates customer override keys first, then vanilla module keys. This means any tenant can override any module's documents, views, or actions without modifying the base application.

```mermaid
flowchart TB
    Request["Metadata Request<br/>(customer + module + document)"]
    CustOverride{"Customer<br/>override exists?"}
    Override["Load customer-specific<br/>metadata file"]
    Default["Load default<br/>module metadata"]
    Runtime["Runtime metadata object"]

    Request --> CustOverride
    CustOverride -->|Yes| Override
    CustOverride -->|No| Default
    Override --> Runtime
    Default --> Runtime
```

---

## 7. Type System

### Scalar Attribute Types

| Skyve Type | Java Type | Notes |
|------------|-----------|-------|
| `text` | `String` | Plain text |
| `memo` | `String` | Long text |
| `markup` | `String` | Rich HTML content |
| `colour` | `String` | Colour code |
| `date` | `DateOnly` | Date without time |
| `time` | `TimeOnly` | Time without date |
| `dateTime` | `DateTime` | Date and time |
| `timestamp` | `Timestamp` | Full precision timestamp |
| `integer` | `Integer` | 32-bit integer |
| `longInteger` | `Long` | 64-bit integer |
| `decimal2` | `Decimal2` | Fixed 2-decimal-place arithmetic |
| `decimal5` | `Decimal5` | Fixed 5-decimal-place arithmetic |
| `decimal10` | `Decimal10` | Fixed 10-decimal-place arithmetic |
| `bool` | `Boolean` | Boolean |
| `enumeration` | Generated `Enum` | Code/description duality via `Enumeration` interface |
| `id` | `String` | UUID primary key |

### Special Storage Types

| Skyve Type | Java Type | Special Behaviour |
|------------|-----------|-------------------|
| `content` | `String` (content ID) | References blob in content store; text-extracted and indexed for federated search |
| `image` | `String` (content ID) | Same as content but typed for image rendering |
| `geometry` | `Geometry` (JTS) | First-class spatial type; persisted via Hibernate Spatial; indexed by spatial DB extensions |

### Relational Types

| Skyve Type | Java Type | Semantics |
|------------|-----------|-----------|
| `association` | `Bean` | Many-to-one reference (aggregation or composition) |
| `collection` | `List` | One-to-many or many-to-many (composition or aggregation) |
| `inverseOne` | `Bean` | Inverse of a to-one association |
| `inverseMany` | `List` | Inverse of a to-many collection |

### Extension Points

- **`Converter<T>`** — bidirectional type↔display-string conversion; includes optional `Format` mask and `Validator` post-parse.
- **`Validator<T>`** — appends `Message`s to `ValidationException`; invoked after converter parse.
- **`Decimal`** — abstract base enforcing fixed-scale arithmetic; subclasses `Decimal2`, `Decimal5`, `Decimal10`.
- **`Enumeration`** — contract for generated enums; stable `code` for persistence, localised `description` for UI.

---

## 8. Persistence & Data Model

### Supported Relation and Inheritance Strategies

```mermaid
graph TD
    subgraph Relations
        MTO["Many-to-One<br/>(association)"]
        OTM["One-to-Many<br/>(collection bags)"]
        MTM["Many-to-Many<br/>(join table aggregation)"]
        OTO["One-to-One<br/>(unique many-to-one)"]
        ARC["Arc / Many-to-Any<br/>(relate to anything)"]
        HIER["Hierarchical<br/>(parent_id + ordered children)"]
    end

    subgraph Inheritance
        JOINED["Joined Subclass<br/>(table-per-subclass)"]
        SINGLE["Single Table<br/>(discriminator column)"]
        MAPPED["Mapped Superclass<br/>(generation path only)"]
    end
```

| Strategy | Skyve Mapping | Database Artefact |
|----------|--------------|-------------------|
| Many-to-one aggregation | `<association type="aggregation">` | FK column on owning table |
| Many-to-one composition | `<association type="composition">` | FK column with cascade delete |
| One-to-many composition (joining) | `<collection type="composition" joining>` | Join table |
| One-to-many composition (owning) | `<collection type="composition">` | FK on child table |
| Many-to-many aggregation | `<collection type="aggregation">` | Dedicated join table |
| One-to-one | `<association>` with unique constraint | Unique FK |
| Arc (relate to anything) | `<any>` / `<many-to-any>` | Type discriminator + ID columns |
| Hierarchical | `parentId` + ordered children | Self-referencing FK |
| Joined inheritance | `<persistent strategy="joined">` | Per-subclass table with shared PK |
| Single-table inheritance | `<persistent strategy="single">` | Discriminator column |
| Mapped superclass | `<persistent strategy="mapped">` | No table; fields inherited into subclass tables |

### Persistence API

`Persistence` is thread-confined (one per request thread via `ThreadLocal`). Key operations:

- `begin()` / `commit()` / `rollback()` — transaction lifecycle
- `save(Bean)` / `delete(Bean)` / `retrieve(Document, String)` — CRUD
- `newDocumentQuery(Document)` — metadata-driven JPQL query builder
- `newSQL(String)` / `newBizQL(String)` — native SQL and Skyve query language
- `newNamedQuery(Module, String)` — execute metadata-defined queries

Query types:
- **`DocumentQuery`** — fluent metadata-driven query with joins, projections, grouping, ordering, paging.
- **`DocumentFilter`** — fluent predicate builder (null-aware, collection-size, spatial predicates).
- **`SQL`** — native SQL with Skyve-type-aware parameter binding; supports scalar/tuple/bean/dynamic results and streaming iterables.
- **`BizQL`** — Skyve object query language (JPQL-like over domain beans) with parameters, paging, and DML.

Results are lazy `Map` instances exposing the domain interface until a real bean is required — avoiding unnecessary instantiation.

### Dialect Extensions & Schema Evolution

`SkyveDialect` extends Hibernate dialects with spatial conversion and schema-evolution hooks. `DDLDelegate` supplements Hibernate's DDL with alter-column coercion when type, length, or precision changes.

| Dialect | Database | Notable Capabilities |
|---------|----------|---------------------|
| `AbstractH2SpatialDialect` | H2 (GeoDB) | Test-oriented spatial; `NULLS DISTINCT` unique constraints (H2 2.2+) |
| `MySQL8InnoDBSpatialDialect` | MySQL 8 | Spatial index DDL tuning; `LONGVARCHAR` false-positive suppression |
| `PostgreSQL10SpatialDialect` | PostgreSQL (PostGIS) | `lower()` `text_pattern_ops` index exporter |
| `SQLServer2012SpatialDialect` | SQL Server | Geometry conversion; `TRANSACTION_SNAPSHOT` isolation |

---

## 9. Security Architecture

```mermaid
flowchart TD
    Auth["Authentication<br/>(Spring Security)"]
    Principal["Principal Resolution<br/>(WebUtil → UserImpl)"]
    Session["Session/Thread Binding<br/>(Persistence)"]
    AccessProc["AccessProcessor<br/>(menu + role + router + view)"]
    CheckAccess["EXT.checkAccess()<br/>(UserAccess + uxui)"]
    RowFilter["Row-Level Filtering<br/>(DocumentPermissionScope)"]
    CustOverride["Customer Override<br/>(FileSystemRepository)"]

    Auth --> Principal
    Principal --> Session
    Session --> AccessProc
    AccessProc --> CheckAccess
    CheckAccess --> RowFilter
    RowFilter --> CustOverride

    Auth -.- DB["Database Auth<br/>(SkyveLegacyAuthenticationProvider)"]
    Auth -.- OAuth["OAuth 2.0<br/>(Google, Facebook, GitHub, Microsoft Entra)"]
    Auth -.- SAML["SAML 2.0<br/>(SpringSecurityConfig)"]
    Auth -.- SPNEGO["SPNEGO/Kerberos<br/>(ProvidedRepositoryFactory)"]
```

### Security Components

| Component | Location | Responsibility |
|-----------|----------|---------------|
| `User` | skyve-core | Authenticated principal: identity, customer, roles, permissions, access checks |
| `UserImpl` | skyve-core | Stores resolved permissions, action/content permissions, executes `canReadBean`/`canAccess` |
| `Role` | skyve-core | Module-declared permission group; union semantics across multiple assigned roles |
| `DocumentPermission` | skyve-core | CRUD + scope (global/customer/dataGroup/user/none); merge semantics for aggregation |
| `DocumentPermissionScope` | skyve-core | Row visibility hierarchy enforcing data-group and user ownership boundaries |
| `UserAccess` | skyve-core | Immutable access-request key (singular, aggregate, report, content, dynamic image) |
| `AccessProcessor` | skyve-core | Builds per-user access vectors from menu, role, router, and view metadata |
| `BasicAuthFilter` | skyve-web | REST HTTP Basic auth; validates credentials; establishes persistence user context |
| `SkyveSpringSecurity` | skyve-web | Spring Security wiring: JDBC user details + OAuth client registrations |
| `SkyveLegacyAuthenticationProvider` | skyve-ext | Direct-SQL credential authentication (legacy database auth) |
| `SkyveRememberMeTokenRepository` | skyve-ext | JDBC remember-me token persistence |

### Multi-Tenancy Model

Row-level isolation is enforced via `Bean` ownership fields: `bizCustomer`, `bizDataGroupId`, `bizUserId`. `DocumentPermissionScope` controls which rows a user can see based on their relationship to these fields. `MetaDataQueryDefinitionImpl` automatically substitutes `{DATAGROUPID}` tokens to restrict query results to the user's data group.

### OWASP Protections

- CSRF token rotation (managed in conversation cache)
- CSP, X-Frame-Options, Referrer-Policy, X-Content-Type-Options headers (SecurityHeadersFilter)
- Input encoding via `org.owasp.encoder`
- Fail-closed REST (ForbiddenFilter blocks `/rest/*` by default)
- Session fixation prevention (conversation-session binding in `StateUtil`)

---

## 10. UI Rendering Pipelines

```mermaid
graph TD
    ViewMeta["View Metadata<br/>(XML)"]

    subgraph Faces["JSF / PrimeFaces"]
        FV["FacesView<br/>(@ViewScoped)"]
        FVR["FacesViewRenderer"]
        PF["PrimeFaces<br/>Component Tree"]
    end

    subgraph SC["SmartClient"]
        DV["DesktopView<br/>(request-scoped)"]
        SCVR["SmartClientViewRenderer"]
        MDS["MetaDataServlet<br/>(JSON bootstrap)"]
    end

    subgraph Vue["Vue / PrimeVue"]
        LG["ListGrid Component"]
        SS["SnapshotService"]
        SM["smartlist endpoint"]
    end

    subgraph Flutter["Flutter"]
        SRC["SkyveRestClient"]
        CVP["containerViewProvider"]
        SVM["SkyveViewModel"]
        LW["LoaderWidget"]
    end

    ViewMeta --> FVR
    FVR --> PF
    FV --> PF

    ViewMeta --> SCVR
    SCVR --> MDS
    DV --> MDS

    ViewMeta --> SM
    SM --> LG

    ViewMeta --> CVP
    CVP --> SVM
    SVM --> LW
    SRC --> LW
```

### Pipeline Characteristics

| Pipeline | Entry Point | Rendering Model | State Management |
|----------|-------------|-----------------|------------------|
| **JSF/Faces** | `FacesView` (`@Named("skyve")`, `@ViewScoped`) | Server-side component tree built by `FacesViewRenderer` from metadata | Conversation cached via `StateUtil`; `SkyveFacesPhaseListener` restores/caches per request |
| **SmartClient** | `DesktopView` (request-scoped) | `SmartClientViewRenderer` generates JavaScript; `MetaDataServlet` serves JSON | Conversation via `SmartClientEditServlet`/`SmartClientListServlet` |
| **Vue** | `SKYVE.listgrid` in `main.js` | PrimeVue `ListGrid` mounted into server-provided container; POSTs to `smartlist` | Snapshots persisted via `SnapshotService` (`type=vue`) |
| **Flutter** | `SkyveRestClient` | `containerViewProvider` fetches view JSON; `SkyveViewModel` interprets into widgets | Client-side; `LoaderWidget` fetches `smartedit` payloads |

### Isomorphic Rendering

All pipelines render from the same view metadata. The `UxUiSelector` chooses the active renderer based on `UserAgentType` detection and servlet context. Router metadata declares per-UX/UI route variants, enabling device-specific view selection from a single definition.

### Conversation State Model

Each browser tab/popup is a `WebContext` — a server-side conversation holding the current bean, cached beans, pending messages, and persistence state. `AbstractWebContext` carries `sessionId`, bean map, and the derived `webId` (conversation UUID + current bean ID). `StateUtil` serializes conversations into an EHCache region; session ownership is validated on restore to prevent cross-session hijacking.

---

## 11. Content Management & Search

```mermaid
flowchart LR
    Upload["File Upload<br/>(content attribute)"]
    TE["TextExtractor<br/>(Tika: PDF, Office, etc.)"]
    FS["FileSystemContentManager<br/>(blob + metadata sidecar)"]
    LI["LuceneContentManager<br/>(full-text index)"]
    EI["ElasticContentManager<br/>(Elasticsearch index)"]
    BCI["Bean Content Indexing<br/>(textual fields from domain)"]
    FS2["Federated Search<br/>(google() API)"]
    UI["Search UI<br/>(combined results)"]

    Upload --> TE
    TE --> FS
    FS --> LI
    FS --> EI
    BCI --> LI
    BCI --> EI
    LI --> FS2
    EI --> FS2
    FS2 --> UI
```

### Content Architecture

- **`ContentManager`** — SPI with `put(BeanContent)`, `put(AttachmentContent)`, `getAttachment(String)`, `removeBean(String)`, `removeAttachment(String)`, `google(String, int)` for federated search.
- **`AbstractContentManager`** — base with filesystem/metadata helpers; metadata ties content to `bizCustomer`, `bizModule`, `bizDocument`, `bizId`, `attributeName`.
- **`FileSystemContentManager`** — default store: assigns `contentId`, sniffs MIME via `TextExtractor`, writes bytes + sidecar metadata.
- **`LuceneContentManager`** — extends filesystem; indexes both `BeanContent` and `AttachmentContent` into a single Lucene `CONTENT` field.
- **`ElasticContentManager`** — Elasticsearch-backed implementation (two-index search shape).
- **`DelegatingContentManager`** — facade forwarding to the configured implementation.
- **`TikaTextExtractor`** — PF4J plugin using Apache Tika for PDF, Office, and other format extraction.

### Transactional Content Relations

Content is stored transactionally alongside database operations:
- `HibernateContentPersistence` opens a `ContentManager` and calls `putBeanContent`/`removeBeanContent` so index updates track persistence work.
- `AbstractHibernatePersistence` builds `BeanContent` from fields marked `IndexType.textual` or `IndexType.both`, running markup through `TextExtractor`.
- `AttachmentContent` carries full bean linkage (`bizCustomer`, `bizModule`, `bizDocument`, `bizDataGroupId`, `bizUserId`, `bizId`, `attributeName`).

### Federated Search

`google(String, int)` searches across both structured bean data and file content, returning `SearchResult` DTOs with `contentId`, `bizId`, excerpt, score, and document coordinates. The UI presents combined results from both indexes transparently.

---

## 12. Jobs & Background Tasks

| Concept | Class | Scheduling | Persistence Mode | UI Context |
|---------|-------|-----------|------------------|------------|
| **Job** | `Job` / `CancellableJob` / `IteratingJob<T>` | Cron, one-shot, ad-hoc via UI | Async timeouts (`setAsyncThread(true)`) | None — runs under a designated user |
| **ViewBackgroundTask** | `ViewBackgroundTask<T>` | Triggered from UI action | Async timeouts | Full — `execute(bean)` against cached conversation |

### Job Infrastructure

- **`JobScheduler`** — scheduling facade for one-shot, dated, cron, scheduled reports, background tasks, restore orchestration, and content GC.
- **`QuartzJobScheduler`** — Quartz-backed implementation registering module-declared jobs at startup; triggers via `runOneShotJob()` / `runBackgroundTask()`.
- **`AbstractSkyveJob`** — Quartz bridge wrapping transaction lifecycle and persisting execution history.
- **`JobMetaData`** — module XML descriptor carrying display name, class, and owning module.
- **`NaturalCronExpressionParser`** — converts human-readable phrases (e.g., "every weekday at 9am") to validated six-field cron expressions.

### ViewBackgroundTask vs Job

A `ViewBackgroundTask` is a short-running asynchronous process kicked off from a UI interaction. It operates against the same persistence instance and conversation cache as the originating request — same L1 cache, same local UI mutations. A `Job` is a long-running scheduled process with its own transaction lifecycle, independent of any UI conversation.

---

## 13. Reporting

```mermaid
flowchart TD
    subgraph Jasper["Jasper Reports"]
        JR["JasperReportUtil"]
        SDS["SkyveDataSource<br/>(fields → bindings)"]
        SDE["SkyveDocumentExecuter<br/>(document query language)"]
        JRXML["JRXML templates"]
    end

    subgraph FM["FreeMarker"]
        FRU["FreemarkerReportUtil"]
        RT["ReportTemplate<br/>(persisted markup)"]
        RD["ReportDataset<br/>(BizQL/SQL/class)"]
    end

    subgraph Compile["Build-Time"]
        CJM["CompileJasperReportMojo<br/>(Maven)"]
    end

    JRXML --> CJM
    CJM --> JR
    JR --> SDS
    JR --> SDE
    RT --> FRU
    RD --> FRU
```

### Jasper Integration

- `JasperReportUtil` — runs bean, SQL, or metadata-driven reports; injects `RESOURCE_DIR`/`SUBREPORT_DIR`; resolves compiled reports via repository metadata.
- `SkyveDataSource` — binds Jasper fields to Skyve bindings; field descriptions yield formatted values; `THIS` and `USER` expose current bean/user.
- `SkyveDocumentExecuter` — custom query executer for document-language JRXML.
- `CompileJasperReportMojo` — Maven mojo compiling `.jrxml` to `.jasper` at build time.

### FreeMarker Integration

- `FreemarkerReportUtil` — template loading, HTML-to-PDF conversion, download support.
- `ReportTemplate` — persisted report template with markup, parameters, and dataset definitions.
- `ReportDataset` — typed dataset metadata (BizQL, SQL, constant, class-backed via `BeanReportDataset`).
- `ReportParameter` — typed parameter with `setReportInputValue()` for execution-time overrides.

Reports are declared in module metadata and resolved by `ProvidedRepository.getReportFileName()` using customer/document/report name with Jasper/FreeMarker suffix conventions.

---

## 14. Import / Export

### BizPort (Full Graph Exchange)

- **`BizPortWorkbook`** — workbook root for bulk Excel exchange; owns sheets by module/document key with validations, dropdowns, and FK constraints.
- **`StandardGenerator`** — derives workbook structure from document metadata; recursively exports object graphs via collection/join sheets + FK columns.
- **`StandardLoader`** — reconstructs beans from document sheets; links associations via FK columns; links collections from collection sheets.
- **`BizExportAction`** / **`BizImportAction`** — metadata action extension points.

### Quick Import/Export (Tabular)

- **`AbstractDataFileLoader`** — simpler metadata-driven tabular import mapping columns to bindings with referential integrity support.
- **`CSVLoader`** — CSV implementation with text/date/numeric coercion.
- **`DataFileField`** — per-column mapping rule (binding, required flag, converter, load action: SET_VALUE, LOOKUP_EQUALS, LOOKUP_LIKE, CONFIRM_VALUE).
- **`POISheetGenerator`** — tabular export writing bindings/query results to styled `.xlsx`.

### Backup / Restore

- **`BackupJob`** / **`RestoreJob`** — cancellable jobs producing/consuming versioned ZIP archives with schema handling and content restore.
- **`BackupUtil`** — platform-independent backup/restore (backup from SQL Server, restore to H2).
- **`ExternalBackup`** / **`AzureBlobStorageBackup`** — off-site storage integrations.
- **`ExportedReference`** — tracks cross-module references for dependency ordering during restore.

---

## 15. Spatial

Geometry is a **first-class attribute type** in Skyve — not an add-on. Spatial capabilities are woven through the type system, persistence layer, query API, and UI rendering.

```mermaid
flowchart LR
    Attr["Geometry Attribute<br/>(JTS Geometry)"]
    Dialect["Skyve Hibernate Dialect<br/>(per-database spatial)"]
    DB["Native Spatial Column<br/>(PostGIS / GeoDB / etc.)"]
    Filter["DocumentFilter<br/>(spatial predicates)"]
    MapModel["MapModel / MapFeature"]
    UI["Map Widget<br/>(viewport query)"]

    Attr --> Dialect
    Dialect --> DB
    DB --> Filter
    Filter --> MapModel
    MapModel --> UI
```

### Spatial API

- **`DocumentFilter`** — fluent spatial predicates: `within`, `contains`, `crosses`, `disjoint`, `intersects`, `overlaps`, `touches`.
- **`MapModel<T>`** — SPI returning map features for a viewport geometry.
- **`DefaultMapModel`** — binds geometry field, clips by map bounds, emits `MapItem`/`MapFeature`.
- **`DocumentQueryMapModel`** — drives map directly from metadata query.
- **`ReferenceMapModel`** — maps collection/association from current bean into features.
- **`GeometryConverter`** — converts JTS Geometry values to/from WKT text.

### Database Spatial Support

| Dialect Delegate | Database | Capabilities |
|-----------------|----------|--------------|
| `PostgreSQLSpatialDialectDelegate` | PostgreSQL/PostGIS | EWKB conversion, PostGIS typing, DDL checks |
| `MySQLSpatialDialectDelegate` | MySQL | WKB conversion, index adaptations |
| `SQLServerSpatialDialectDelegate` | SQL Server | Geometry encoding/decoding, uniqueness handling |
| `AbstractH2SpatialDialect` | H2 (GeoDB) | Test-oriented spatial base |
| `MySQL5SpatialFunctions` / `MySQL8SpatialFunctions` | MySQL | Registered spatial function sets (distance, buffer, intersection, union) |

---

## 16. Extension Points

```mermaid
flowchart TD
    subgraph Lifecycle["Bean Lifecycle (Bizlet)"]
        NI["newInstance()"]
        PS["preSave()"]
        POS["postSave()"]
        PD["preDelete()"]
        V["validate()"]
        PR["postRender()"]
    end

    subgraph Intercept["Interceptor Chain"]
        BI["beforeSave()"]
        AI["afterSave()"]
        BD["beforeDelete()"]
        AD["afterDelete()"]
    end

    subgraph Plugin["Plugin Architecture (PF4J)"]
        AI2["AddIn base class"]
        AIM["AddInManager"]
        P4J["PF4JAddInManager"]
    end

    subgraph DI["Dependency Injection"]
        CDI["SkyveCDIProducer"]
        SI["Spring @Autowired"]
    end

    NI --> PS --> POS
    PD --> V
    BI --> BD
    AI --> AD
```

| Extension Point | Mechanism | Declaration |
|----------------|-----------|-------------|
| **Bizlet** | Subclass `Bizlet<T>` per document; lifecycle callbacks from `newInstance` through `postRender` | Convention: `{module}.{document}.{Document}Bizlet` class |
| **Interceptor** | Implement `Interceptor`; before methods can short-circuit; after methods run in reverse order | Declared in customer XML metadata |
| **Observer** | Implement `Observer` for startup, shutdown, login, logout, backup, restore events | Declared in customer XML metadata |
| **Converter** | Implement `Converter<T>` for custom type↔display conversion | Referenced in attribute metadata |
| **Validator** | Implement `Validator<T>` for custom validation rules | Referenced in converter or attribute metadata |
| **AddIn (Plugin)** | Extend `AddIn` base class; discovered via PF4J `AddInManager` at runtime | Packaged in separate Maven module; placed in add-ins directory |
| **CDI/Spring** | `SkyveCDIProducer` exposes `Persistence`, `Repository`, `Caching`, `JobScheduler`, `AddInManager` | Standard `@Inject` / `@Autowired` |

### CDI Proxies

`PersistenceInjectable` and `RepositoryInjectable` are stateless, serializable proxies that delegate injected calls to `CORE` at use time — ensuring CDI-injected services always resolve to the current thread's active instance.

---

## 17. Caching

| Cache Region | Implementation | Purpose |
|--------------|---------------|---------|
| Conversations | `ConversationCacheConfig` → EHCache | Serialized `WebContext` per browser tab (idle-expiring) |
| CSRF Tokens | `DefaultCaching` | Per-session CSRF token rotation |
| GeoIP | `DefaultCaching` | Cached geolocation lookups |
| Session | `DefaultCaching` | Session-scoped application data |
| App | `DefaultCaching` | Application-scoped shared data |
| Hibernate L2 | `HibernateCacheConfig` | Entity and collection caching (declared in document metadata via `<persistent>` cache element) |

### Cache API

- **`Caching`** — management API for creating, retrieving, destroying, and inspecting cache regions (EHCache and JCache).
- **`CacheConfig`** — abstract descriptor declaring name, sizing, expiry, key/value types.
- **`EHCacheConfig`** — Ehcache-specific: disk/persistence options, heap/off-heap sizing.
- **`JCacheConfig`** — provider-neutral shared caches.
- **Conversation caching** — `StateUtil` serializes `WebContext` into the conversations region keyed by conversation UUID; validates session ownership on restore.
- **Document metadata** — `<persistent>` element declares a named cache region; `<collection>` elements can specify shared cache names.
- **`SingletonCachedBizlet`** — memoises singleton document `bizId`s; relies on Hibernate L2 cache for actual bean retrieval.
- **`SkyveContextListener`** — initializes conversation cache sizing/expiry from `state.conversations` deployment configuration.

---

## 18. Web Services & APIs

### REST Endpoints

- **`RestService`** (`@Path("/api")`) — generic CRUD REST endpoint for document instances (JSON retrieval, creation, update, deletion). Blocked by `ForbiddenFilter` by default; deployments opt-in by swapping filter configuration.
- **`JaxRsActivator`** — JAX-RS `Application` subclass activating endpoint registration.
- **`MetaDataServlet`** — serves document/view/menu metadata as JSON for JavaScript clients.
- **`DocsServlet`** — forwards `/docs` to Swagger UI page.

### Shaped Payloads

`JSON` utility class in `org.skyve.util` marshals beans with optional property-name projection, enabling shaped payloads that expose only the fields needed by a particular client.

### Push Infrastructure

- **`PushMessage`** — server-to-client command envelope for growls, message displays, rerenders, and named JavaScript function invocations.
- **`SseApplication`** — JAX-RS `/sse` bootstrap for server-sent events.
- **`SseClientHandler`** — `PushMessageReceiver` streaming messages over SSE with bounded queueing, keep-alives, reconnection IDs, and per-user/global connection caps.

### JavaScript API & Injection

- **`Inject`** — view metadata element inserting custom script at the tag location during rendering.
- **`InjectBinding`** — injects bound field values into surrounding layout/script.
- **`HTMLResources`** — tenant extension hook for injecting customer-specific CSS.

### Content Remoting

- **`RestRemoteContentManagerServer`** — JAX-RS endpoint at `/rest/content` for inter-Skyve-server attachment operations (explicitly not internet-facing).
- **`RestRemoteContentManagerClient`** — HTTP client forwarding content manager calls to another Skyve server.
- **`JDBCRemoteContentManagerServer`** — JDBC-callable function bridge for remote content operations.

---

## 19. Scaffolding & Tooling

### Maven Plugin Goals

| Mojo | Purpose |
|------|---------|
| `DomainGenerator` | Validates all metadata, generates domain classes, enums, and HBM mappings |
| `CompileJasperReportMojo` | Compiles `.jrxml` templates to `.jasper` |
| `SystemDocumentationMojo` | Generates system documentation with PlantUML directives |
| Scaffold mojos | Project generation and module scaffolding |

### SAIL (Skyve Automated Integration Lifecycle)

- Generated from view metadata — produces browser-automation test scripts.
- Supports Selenese or WebDriver output formats.
- Validates that rendered UI matches metadata expectations.

### Unit Test Generation

- **CRUD domain testing** — generated tests for create, read, update, delete on every persistent document.
- **Action tests** — generated test stubs for each declared action.
- **Test data generation** — `DataBuilder` utility producing valid random bean instances respecting type constraints.
- **Data store seeding** — test infrastructure for populating the database with representative data.

### Project Generation

Skyve generates complete project scaffolding including:
- Maven POM structure
- Metadata directory layout (customer, module, document, view)
- Spring/CDI configuration
- Deployment descriptors
- Initial domain generation output

### Runtime Engineering

- **Proxy framework** — dynamic proxy generation for framework services.
- **Runtime compiling** — in-memory class loading for hot-deployed metadata changes.
- **`CachedRepository`** — extends repository contract with explicit metadata cache eviction for hot-reload and admin overrides.

---

## 20. Key Abstractions Reference

| Class/Interface | Module | Package | Description |
|----------------|--------|---------|-------------|
| `Bean` | core | `org.skyve.domain` | Base domain contract with ownership fields (`bizCustomer`, `bizUserId`, `bizDataGroupId`) |
| `PersistentBean` | core | `org.skyve.domain` | Extends Bean with persistence identity (`bizId`, `bizVersion`, `bizLock`) |
| `Customer` | core | `org.skyve.metadata.customer` | Tenant root: modules, roles, interceptors, converters, branding |
| `Module` | core | `org.skyve.metadata.module` | Groups documents, queries, roles, menu |
| `Document` | core | `org.skyve.metadata.model.document` | Business entity definition: attributes, relations, views, actions |
| `View` | core | `org.skyve.metadata.view` | UI screen declaration (edit/create/list) |
| `Persistence` | core | `org.skyve.persistence` | Transaction lifecycle, CRUD, query factory |
| `AbstractHibernatePersistence` | ext | `org.skyve.impl.persistence.hibernate` | Core Hibernate-backed persistence implementation |
| `ProvidedRepository` | core | `org.skyve.impl.metadata.repository` | Primary metadata resolution API with customer-override awareness |
| `DomainGenerator` | core | `org.skyve.impl.generate` | Validates metadata and generates domain source code |
| `Binder` / `BindUtil` | core | `org.skyve.util` / `org.skyve.impl.bind` | Dotted-path binding resolution for static and dynamic beans |
| `ExpressionEvaluator` | core | `org.skyve.impl.bind` | Pluggable expression evaluation (binding, EL, i18n, role, stash) |
| `ContentManager` | content | `org.skyve.content` | Content store SPI: put, get, remove, search |
| `JobScheduler` | core | `org.skyve.job` | Job scheduling facade (cron, one-shot, ad-hoc) |
| `Job` | core | `org.skyve.job` | Abstract background process extension point |
| `Bizlet<T>` | core | `org.skyve.metadata.model.document` | Document lifecycle extension (newInstance → postRender) |
| `Interceptor` | core | `org.skyve.metadata.controller` | Customer-registered before/after lifecycle hook |
| `Converter<T>` | core | `org.skyve.domain.types` | Bidirectional type↔display conversion |
| `Router` | core | `org.skyve.impl.metadata.repository.router` | UX/UI route selector from `router.xml` |
| `FacesView` | web | `org.skyve.impl.web.faces.views` | `@ViewScoped` JSF managed bean for Skyve interactions |
| `DocumentQuery` | core | `org.skyve.persistence` | Fluent metadata-driven JPQL query builder |
| `SQL` / `BizQL` | core | `org.skyve.persistence` | Native SQL and Skyve object query wrappers |
| `MapModel<T>` | core | `org.skyve.metadata.view.model.map` | Spatial map feature provider for viewport queries |
| `Caching` | core | `org.skyve.cache` | Cache management API (EHCache, JCache) |
| `WebContext` | core | `org.skyve.web` | Per-conversation server state for a browser tab |
| `UserAccess` | core | `org.skyve.metadata.user` | Immutable access-request key for router/view security |
| `AddInManager` | core | `org.skyve.addin` | PF4J plugin discovery and lookup API |
| `EXT` | ext | `org.skyve` | Extended runtime facade (request, JDBC, content, integration APIs) |
| `PushMessage` | web | `org.skyve.push` | Server-to-client command envelope (growl, rerender, execute) |
