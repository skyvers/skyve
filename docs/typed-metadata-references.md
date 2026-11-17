# Typed Metadata References

`generateDomain` writes an enum for each kind of metadata a module declares, next to the module's domain classes. Each constant names one role, document, query or job, so code can refer to it without spelling out the module name and the metadata name as strings. A renamed or removed role, document, query or job then fails the build instead of failing, or quietly returning `false`, at run time.

```java
if (user.isInRole(AdminRole.SECURITY_ADMINISTRATOR)) { ... }

DocumentQuery q = CORE.getPersistence().newNamedDocumentQuery(AdminQuery.Q_CONTACTS);

JobMetaData job = AdminJob.J_BULK_USER_CREATION.toJob(customer);
EXT.getJobScheduler().runOneShotJob(job, bean, user);
```

The string forms are unchanged and are not deprecated. See [When to use strings](#when-to-use-strings).

## The generated enums

For a module named `admin`, in package `modules.admin.domain`:

| Enum | Lists | Implements | Name accessor | Resolves to |
|---|---|---|---|---|
| `AdminRole` | roles | `org.skyve.metadata.user.ModuleRole` | `roleName()` | `toRole()` → `Role` |
| `AdminDocument` | documents, including those the module references from other modules | `org.skyve.metadata.model.document.ModuleDocument` | `documentName()` | `toDocument()` → `Document` |
| `AdminQuery` | metadata (document) queries | `org.skyve.metadata.module.query.ModuleQuery` | `queryName()` | `toQuery()` → `MetaDataQueryDefinition` |
| `AdminBizQL` | BizQL queries | `org.skyve.metadata.module.query.ModuleBizQL` | `queryName()` | `toBizQL()` → `BizQLDefinition` |
| `AdminSQL` | SQL queries | `org.skyve.metadata.module.query.ModuleSQL` | `queryName()` | `toSQL()` → `SQLDefinition` |
| `AdminJob` | jobs | `org.skyve.metadata.module.ModuleJob` | `jobName()` | `toJob()` → `JobMetaData` |

- **Common parent.** All six interfaces extend `org.skyve.metadata.module.ModuleMember`, which adds `moduleName()` and `toModule()`.
- **Constant names** come from the metadata name, upper-cased with underscores between words. So `SecurityAdministrator` becomes `SECURITY_ADMINISTRATOR`, and `qContacts` becomes `Q_CONTACTS`.
- **Duplicate constants are refused.** If two names in a module give the same constant, such as `Read Only` and `ReadOnly`, generation fails and asks you to rename one.
- **Empty kinds are skipped.** A module with no jobs gets no `AdminJob`, and so on.
- **Clashing class names are dodged.** If a document in the module already has the enum's class name, the suffix is repeated, for example `AdminRoleRole`.

### Resolving to metadata

- **Two forms.** `toRole()`, `toDocument()` and the rest use the current user's customer, as `CORE.getCustomer()` does. Each also has a form that takes a `Customer`, for code with no current user, such as startup code, jobs and tests.
- **Missing names throw.** Both forms throw `MetaDataException` when the name does not resolve for that customer.

### Referenced documents

A module that references another module's document, such as `<document ref="Contact" moduleRef="admin" />` in `whosin`, gets a constant for it: `WhosinDocument.CONTACT`. It addresses the document through the referencing module, exactly as `("whosin", "Contact")` does. So `moduleName()` is `whosin`, while `toDocument().getOwningModuleName()` is `admin`.

## APIs that take them

Each of these sits beside the string form it replaces and calls it.

| Type | Methods |
|---|---|
| `User` | `isInRole(ModuleRole)`, `getScope(ModuleDocument)` |
| `Persistence` | `newDocumentQuery`, `retrieve` and `retrieveAndLock` with a `ModuleDocument`; `newNamedDocumentQuery(ModuleQuery)`, `newNamedBizQL(ModuleBizQL)`, `newNamedSQL(ModuleSQL)`; the shared cache methods (`sharedCacheBean`, `sharedCacheCollection`, `evictSharedCache*`) with a `ModuleDocument` |
| `UserAccess` | `singular(ModuleDocument)`, `documentAggregate(ModuleDocument)`, `queryAggregate(ModuleQuery)` |
| `DataBuilder` | `build(ModuleDocument)`, `factoryBuild(ModuleDocument)` |
| `ChartBuilder` | `with(ModuleDocument)`, `withQuery(ModuleQuery)` |
| `EXT` | `standardBizExport(ModuleDocument)` |
| `NumberGenerator` | `next`, `nextInt` and `nextLong` with a `ModuleDocument` |
| `SingletonCachedBizlet` | `clear(ModuleDocument)` |
| `Tag` | `countDocument(ModuleDocument)` |

They take the interfaces, not the generated enums. So your own implementation works too, such as an enum of your own or a test double. For a job, resolve it with `toJob()` and pass the `JobMetaData` to `JobScheduler`.

## Customer overrides

A customer module override (`customers/<customer>/modules/<module>/<module>.xml`) replaces the module's definition for that customer. It can add and remove roles, documents, queries and jobs. For each module a customer overrides, `generateDomain` writes the same enums for that customer's version of the module. They go in `customers.<customer>.modules.<module>.domain` with the same class names. For example, `customers.test.modules.test.domain.TestRole` lists `TEST_ROLE`, which only the `test` customer's override declares.

- **Customer code** can import the customer enums. A reference to metadata that the override removed then fails to compile.
- **Code shared across customers** should use the vanilla enums, and only for metadata every customer keeps.
- **Clean regeneration.** Customer enums are regenerated on every run, and those of an override you remove are deleted.
- **They don't switch on customer domain generation.** The generated customer `Ext` classes and ORM mapping are still produced only when you have opted in by creating `src/generated/java/customers/<customer>/modules/` yourself. If that directory holds nothing but generated enums, it doesn't count as opting in.

## When to use strings

Use the string forms when the name isn't fixed when the application is built:

- **Dynamic domains.** Documents and modules defined at run time, and metadata built with the fluent API.
- **Code that works with a customer's metadata at run time.** For example, code that iterates a customer's roles or documents.
- **Names held as data.** For example, a role name read from configuration or from a `{role:...}` expression.

## Testing with mocks

The typed overloads are `default` methods. A Mockito `mock(User.class)` doesn't run them: an unstubbed `isInRole(AdminRole.X)` returns `false`, even when `isInRole("admin", "X")` is stubbed. Stub the overload the code under test calls. With Skyve's Mockito setup, `mock(User.class, CALLS_REAL_METHODS)` can't run an interface's default methods. To exercise the delegation itself, mock an `abstract` class that implements the interface, with `CALLS_REAL_METHODS`.
