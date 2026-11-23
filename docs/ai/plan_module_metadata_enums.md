# Plan: Typed module metadata references (PR #323 follow-through)

## Summary

PR #323 (`322-generate-module-level-metadata-class`, Will) makes `generateDomain` emit one enum per module for its roles, documents and metadata queries, for example `AdminRole.AUDIT_MANAGER`. Each enum implements a small reference interface: `ModuleRole`, `ModuleDocument` or `ModuleQuery`. The only Skyve API that accepts an enum is `User.isInRole(ModuleRole)`, and the PR deprecates `User.isInRole(String, String)`.

This plan turns that into one consistent rule across the public API:

> **Strings stay the primitive. Every common public API that takes a module name and a metadata name gets an overload that takes the typed reference and delegates to the string form. Nothing string-based is deprecated.**

Strings must stay first class because a name is not always known when the code compiles:

- **Dynamic domains.** Documents and modules defined or loaded at runtime (dynamic beans, `OnDemandRepository`, the fluent metadata API) have no generated enum.
- **Customer overrides.** PR #323 built enums from vanilla metadata only. This plan adds customer enums (see [Customer overrides](#customer-overrides)), but customer code that must work across customers, or that works with a customer's metadata at runtime, still needs names.
- **Names held as data.** Skyve's own role checks are almost all variables. Of the 9 `isInRole` call sites in `skyve-*/src/main`, 7 pass variables or parse `{role:module.Role}` expressions. Only `DefaultUxUiSelector` and `SailRecording` (`ROLE_MODULE_NAME`, `ROLE_NAME`) use fixed names.

Modules touched: `skyve-core` (interfaces, generator, `User`, `Persistence`, `UserAccess`, `DataBuilder`, `ChartBuilder`), `skyve-ext` (`EXT`), `skyve-war` (regenerated enums, literal call sites converted).

## Problems in the PR as it stands

1. **`isInRole(String, String)` is deprecated.** That penalises dynamic and override code, and every legitimate caller inside Skyve. Remove `@Deprecated` and reword the Javadoc. The string method is the primitive; the typed overloads are a compile-checked shortcut for names known at build time.
2. **`ModuleQuery.toQuery()` can throw `ClassCastException`.** `Module.getMetadataQueries()` returns every `QueryDefinition`, so metadata, BizQL and SQL queries all land in `<Module>Query`. `toQuery()` then casts to `MetaDataQueryDefinition` via `getNullSafeMetaDataQuery`, which fails for a BizQL or SQL query. skyve-war declares no BizQL or SQL module queries, so nothing in this repo exercises it, but applications do.
3. **The interfaces have no null annotations.** Since the null-safety work every public `skyve-core` type is annotated. `moduleName()`, `roleName()` and the others should be `@Nonnull`.
4. **The `toX()` methods behave inconsistently.** `toRole()` returns `getRole(...)`, which is `@Nullable`. `toQuery()` uses the null-safe lookup and throws. `toDocument()` throws via `getDocument`. All three use the current user's customer and offer no way to pass one.
5. **Referenced documents aren't documented.** `<document ref="Contact" moduleRef="admin"/>` in `whosin.xml` puts `CONTACT` in `WhosinDocument` with `moduleName()` = `"whosin"`. That is a valid address: `Module.getDocument` and the permission keys both resolve through the declaring module. But `toDocument().getOwningModuleName()` is `"admin"`, and the Javadoc should say so.

## Design

### Reference interfaces (`skyve-core`)

| Interface | Package | Name accessor | Conversion | Generated enum |
|---|---|---|---|---|
| `ModuleRole` | `org.skyve.metadata.user` | `roleName()` | `toRole()` → `Role` | `<Module>Role` |
| `ModuleDocument` | `org.skyve.metadata.model.document` | `documentName()` | `toDocument()` → `Document` | `<Module>Document` |
| `ModuleQuery` | `org.skyve.metadata.module.query` | `queryName()` | `toQuery()` → `MetaDataQueryDefinition` | `<Module>Query` (metadata queries only) |
| `ModuleBizQL` *(new)* | `org.skyve.metadata.module.query` | `queryName()` | `toBizQL()` → `BizQLDefinition` | `<Module>BizQL` |
| `ModuleSQL` *(new)* | `org.skyve.metadata.module.query` | `queryName()` | `toSQL()` → `SQLDefinition` | `<Module>SQL` |
| `ModuleJob` *(new)* | `org.skyve.metadata.module` | `jobName()` | `toJob()` → `JobMetaData` | `<Module>Job` |

These are the Skyve terms. A "module role" is already a role declared in a module, as opposed to a customer role (`ModuleRoleMetaData`, `FluentModuleRole`). I avoided `*Reference` because `Reference` is the document attribute type.

Common rules:

- **A shared parent interface, `ModuleMember`,** declares `@Nonnull String moduleName()` and `default @Nonnull Module toModule()`. All six interfaces extend it. Generic code (logging, `RequestKey`, tests) can then take any of them.
- **Every conversion has two forms:** `toX()` uses `CORE.getCustomer()`, and `toX(@Nonnull Customer customer)` uses the given customer (jobs, startup, tests). Both are `@Nonnull` and throw `MetaDataException` when the name doesn't resolve, using the existing `getNullSafeRole` and `getNullSafeMetaDataQuery` and null-safe equivalents for BizQL, SQL and jobs.
- **Accessors are `@Nonnull`.** The generated enums stay annotation-free like the rest of the generated domain, so ECJ null analysis takes the contract from the interface.

### Generator (`OverridableDomainGenerator.generateModuleMetadataEnums`)

- **Split queries by type:** `MetaDataQueryDefinition` → `<Module>Query`, `BizQLDefinition` → `<Module>BizQL`, `SQLDefinition` → `<Module>SQL`.
- **Add `<Module>Job`** from `module.getJobs()`.
- **Keep the current behaviour:**
  - an empty kind generates no enum;
  - enum names dodge document names in the package case-insensitively;
  - a duplicate constant after mangling raises `MetaDataException`;
  - metadata names are escaped as Java string literals.

  The six suffixes can't collide with each other.
- **Javadoc:** each enum's class Javadoc says which metadata it lists: vanilla, or the named customer's override. `<Module>Document` adds that referenced documents are addressed through this module.

### Typed overloads

Each overload delegates to the existing string method in one line, as a `default` method on interfaces and a plain method on classes. No new behaviour, no implementation changes.

**Tier 1: everyday application code (in scope)**

| Type | New overloads |
|---|---|
| `User` | `isInRole(ModuleRole)` (in PR), `isInRole(Role)` (in PR), `getScope(ModuleDocument)` |
| `Persistence` | `newDocumentQuery(ModuleDocument)`, `retrieve(ModuleDocument, String bizId)`, `retrieveAndLock(ModuleDocument, String bizId)`, `newNamedDocumentQuery(ModuleQuery)`, `newNamedBizQL(ModuleBizQL)`, `newNamedSQL(ModuleSQL)` |
| `UserAccess` | `singular(ModuleDocument)`, `documentAggregate(ModuleDocument)`, `queryAggregate(ModuleQuery)` |
| `DataBuilder` | `build(ModuleDocument)`, `factoryBuild(ModuleDocument)` |
| `ChartBuilder` | `with(ModuleDocument)`, `withQuery(ModuleQuery)` |
| `EXT` | `standardBizExport(ModuleDocument)` |

**Tier 2: same rule, less common (recommended, cheap)**

`Persistence.sharedCacheBean`, `evictSharedCacheBeans`, `evictSharedCachedBean` and the three `*SharedCacheCollection*` methods; `SingletonCachedBizlet.clear(ModuleDocument)`; `NumberGenerator.next`, `nextInt`, `nextLong(ModuleDocument, String fieldName)`; `Tag.countDocument(ModuleDocument)`.

**Left as strings (out of scope)**

| API | Why |
|---|---|
| Fluent metadata API (`FluentCustomerRole.findRole/removeRole`, `FluentView.findReportAccess`, …) | It builds metadata and is the main dynamic-domain path. |
| `Repository` / `OnDemandRepository` SPI | Implementer-facing; keyed by customer name as well. |
| `UserAccess` with a third name (`modelAggregate`, `report`, `dynamicImage`, `content`, `previousComplete`) | The third name has no enum, so a typed first half buys little. |
| `RequestKey`, `SheetKey`, `Customer.getConstantDomainValues` | Internal or framework plumbing, not application calls. (`Util.getCompleteSuggestions` and `Binder.validateMessage` were here too; they got typed overloads on 2026-11-23, with `DynamicBean`, `ReferenceListModel`, `Util.getDocumentUrl` and `ArchiveConfig.findArchiveDocConfig`.) |
| `JobScheduler` | It already takes `JobMetaData`. `ModuleJob.toJob()` is the bridge. |

No overload takes a generated enum class directly. Everything goes through the interfaces, so a hand-written implementation (an app's own enum, or a test double) also works.

### Using it in Skyve itself (`skyve-war`)

Convert only literal-string call sites, where the compile-time check pays off. For example:

- `module.getJob("jBulkUserCreation")` → `AdminJob.J_BULK_USER_CREATION.toJob()`. Also `DeleteTestData`, `GenerateTestData`, `UserBizlet` and `BulkDocumentAction`.
- `isInRole(Startup.MODULE_NAME, "SecurityAdministrator")` → `isInRole(AdminRole.SECURITY_ADMINISTRATOR)`.

Leave variable-based calls alone. ~~Leave the 93 `newDocumentQuery(X.MODULE_NAME, X.DOCUMENT_NAME)` calls alone too.~~ Superseded 2026-11-23 (Mike): every `X.MODULE_NAME, X.DOCUMENT_NAME` pair in hand-written admin main code now uses `AdminDocument.X`. They are already compile-checked through the domain class constants, and converting them is churn. The document enum is most useful for documents with no domain class (dynamic or non-generated documents) and for generic code that takes a `ModuleDocument`.

## Implementation steps

1. **On the PR branch, fix what's there.** Remove the deprecation and reword the `isInRole(String, String)` Javadoc. Add `@Nonnull` to the interfaces. Add `ModuleMember`. Make the `toX()`/`toX(Customer)` methods consistent.
2. **Generator.** Split queries into three enums, add `ModuleBizQL`, `ModuleSQL` and `ModuleJob` and the `<Module>Job` enum, and update the enum Javadoc.
3. **Overloads.** Add the Tier 1 and Tier 2 overloads, with Javadoc per `docs/ai/javadoc-standards.md` and `{@link}` to the string form.
4. **Regenerate.** Run the domain generator directly, per [the mirror recipe](#verification), and commit the regenerated enums.
5. **skyve-war.** Convert the literal call sites.
6. **Docs and PR.** Add a short "Typed metadata references" section to the developer docs: when to use enums, when strings are required, and vanilla-only coverage. Update the PR #323 description.

## Tests

- **`OverridableDomainGeneratorMoreTest`:**
  - a module with metadata, BizQL and SQL queries generates three separate enums with the right interfaces;
  - `<Module>Job` is generated;
  - an empty kind generates no enum;
  - the name dodge works for the new suffixes.
- **One test per interface** (`ModuleRoleTest` and so on, using a mocked customer and module):
  - `toX(Customer)` resolves;
  - a missing name throws `MetaDataException`;
  - `toX()` uses `CORE.getCustomer()`.
- **Overload delegation** (`UserImplTest`, a `Persistence` test double, `UserAccessTest`, …): each overload calls the string form with `moduleName()` and the name. Use plain `@Test` with `assertAll`, not `@TestFactory`, because of the surefire rerun setting.
- **A referenced document:** `WhosinDocument.CONTACT.toDocument()` resolves to `admin.Contact`.
- **Gates:**
  - `skyve-core` and `skyve-war` unit tests pass;
  - ECJ batch null analysis shows no new warnings;
  - SonarQube shows no new issues.

## Decisions (Mike, 2026-11-17)

1. **Breadth:** Tier 1 and Tier 2.
2. **Queries:** split into `<Module>Query`, `<Module>BizQL` and `<Module>SQL`.
3. **Jobs:** `<Module>Job` is in this round.
4. **`ModuleMember`:** added as the common parent.
5. **`isInRole(Role)`:** kept.
6. **Who:** extend Will's PR #323 (`322-generate-module-level-metadata-class`).
7. **Customer overrides:** generated too; see the next section.

## Customer overrides

### How overrides behave today

- **A customer module override replaces the module definition.** `customers/<c>/modules/<m>/<m>.xml` is loaded instead of `modules/<m>/<m>.xml` for that customer (`MutableCachedRepository.getModule`). It does not merge. skyve-war's `test` customer overrides module `test`: it lists 30 documents (29 of the 36 vanilla documents plus a new `Test`) and adds the role `TestRole`, which vanilla `test` does not declare. So a customer's module can both add and remove roles, documents, queries and jobs.
- **Whether a customer overrides a module** is `repository.vtable(customerName, "modules/" + moduleName)` starting with `customers/`.
- **`OverridableDomainGenerator.generate()` generates customer output** (`<Doc>Ext` classes, new customer documents, `customers/<c>/modules/orm.hbm.xml`) only when the directory `<generatedSrcPath>/customers/<c>/modules/` already exists. That gate has been there since the first commit. No project under `~/_` has such a directory, so customer domain generation is effectively dormant.
- **`replaceGenerated` only cleans `<generatedSrcPath>/modules/*/domain`.** It writes every file with `CREATE_NEW`, so anything already generated under `customers/` would make a second run fail.

### Design

- **What to generate.** For each customer `c` and each module `m` that `c` overrides, generate the same set of enums from the customer's effective module: `customer.getModule(m)`, with documents, roles, metadata, BizQL and SQL queries and jobs. Generate them into `<generatedSrcPath>/customers/<c>/modules/<m>/domain/`, package `customers.<c>.modules.<m>.domain`, with the same simple names as vanilla (`TestRole`, `TestDocument`, …).
  - **Full set, not additions only.** The override is a full replacement, so the customer enum is exactly the metadata that resolves for that customer. Customer code that refers to something the override removed then fails to compile.
  - **Same simple names, different package.** This mirrors how a customer's new document is generated in the customer package under its own name. The typed APIs take the interfaces, so either enum works anywhere.
  - **Collision dodge** against the customer module's document names, using the same rules as vanilla.
  - **Javadoc** names the customer: "Compile-time references to the roles declared in the test module for customer test."
- **No dependency on the dormant gate, and no accidental opt-in.** Customer enums are generated whenever the customer overrides the module, whether or not `<generatedSrcPath>/customers/<c>/modules/` exists. The enums need no ORM and no extension classes. Writing them creates that directory, though, which would switch the existing gate on. So when the generator creates the directory itself, it also writes `customers/<c>/modules/metadata-enums-only.txt`. The gate (`populateDataStructures` and `generate`) is: the directory exists and has no marker. A project that created the directory itself stays opted in whatever else is in it. Deleting the marker opts a customer in.
  - **Why a marker.** A first version guessed from the directory's contents, "holds nothing but generated enums". Review showed that guess fails three ways:
    - An opted-in developer who clears the gated output by hand loses the opt-in, and their directory gets deleted.
    - A `.DS_Store` switches the gate on, and the run after that crashes.
    - A hand-edited enum makes every run crash.
- **Overwrite owned paths.** This run's customer enum paths and the marker may overwrite existing files. Everything else keeps `CREATE_NEW`.
- **Pre-existing bug, not fixed here.** With customer domain generation opted in, a second run fails with `FileAlreadyExistsException` on `orm.hbm.xml`. Because the vanilla `modules/*/domain` directories were already cleared, that leaves the vanilla generated tree missing its files (191 in skyve-war).
- **Clean regeneration.** `replaceGenerated` also deletes the generated enum files under `<generatedSrcPath>/customers/*/modules/*/domain/`. It deletes a customer module's generated directory that no longer corresponds to an override, but only when that directory holds nothing but generated metadata enums, so files from the gated customer domain generation are never touched. Generating twice in a row must succeed and give identical output.
- **Not covered.** Customer roles (`customer.xml` `<roles>`) group module roles for assignment. They are not checked by `isInRole`, so they get no enum.

### Tests

- A customer override that adds a role, removes a document and adds a document generates customer enums with exactly the effective set, in the customer package.
- A customer without module overrides generates no customer enums.
- A customer document named like the enum candidate triggers the dodge.
- Two generator runs in a row over a temp directory succeed, and an override that disappears between runs leaves no stale customer enum.
- skyve-war: `customers/test/modules/test/domain/TestRole.java` contains `TEST_ROLE`, and `TestDocument` contains `TEST` and lacks the removed vanilla documents.

## Verification

Build and test in a worktree or rsync mirror, not the shared checkout. Regenerate by running `org.skyve.impl.generate.DomainGenerator` directly after syncing `src/main/java` metadata into `target/classes`. `mvn skyve:generateDomain` would use the stale `~/.m2` jars.

## Later decisions (Mike, 2026-11-17)

- **Customer domain generation always runs.** The opt-in directory and the `metadata-enums-only.txt` marker are dropped. Customer output is cleaned every run, and `orm.hbm.xml` is written only when it maps something.
- **Customer roles** get a `<Customer>CustomerRole` enum in `customers.<c>.domain`. `AssignableRole.assignedRoleName()` is common to module and customer roles, and `ModulesUtil.configureGroup(name, AssignableRole, AssignableRole...)` takes them.
- **Generated `Ext` classes are renamed** to `<Doc>Vanilla` (the concrete vanilla class when narrowing makes `Doc` abstract) and `<Doc>Override` (a customer's class).
- **A narrowed document with a hand-written Extension** is layered `Doc` ← `DocVanilla` ← `DocExtension`. The generator checks that the Extension's `extends` clause matches whether the document is narrowed. The check is an error with validation on and a warning with validation off, so developers can still generate while fixing code drift. Every other generation-time refusal is an error regardless.
- **Overrides inside an `extends` hierarchy:**
  - the whole hierarchy is copied into the customer mapping from its root, and leaf overrides get `<Doc>Override` classes;
  - a non-leaf override that needs its own class, and narrowing a base that has subclasses, are refused with a `MetaDataException`;
  - supporting them later, via `SubOverride extends Sub` with copied base-override fields plus a generated properties interface, is to be tried once H2 fixtures exist.
- **skyve-war `test` customer fixtures** with H2 tests running as that customer are to exercise every override mechanism at run time. They must fail on `af6e5b12ce` and pass after the fixes.
- **New customer documents with `extends`** are refused, whatever the strategy, including a customer's own new hierarchy. The class generated for a new customer document always extends `AbstractPersistentBean`, never its base, and the `mapped` case mapped inherited properties the class lacks. This is a candidate to support later, alongside non-leaf overrides.
