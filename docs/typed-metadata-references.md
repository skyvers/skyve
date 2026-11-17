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

### Customer domain generation

For every customer, `generateDomain` writes into `customers/<customer>/modules/`:

- **A `<Document>Override` class** in `<module>/domain/` for each document override that adds properties (attributes or conditions), overrides a narrowed document (see below) or has the customer's own hand-written extension.
- **A class for each new customer document**, in the same directory.
- **The customer ORM mapping** `orm.hbm.xml`, for the persistent documents the customer overrides or adds. Its entity names are the customer name, module name and document name run together, such as `testtestOverrideAdds`. A customer whose overrides are all transient documents gets no mapping file.
- **The module's enums**, for every module override.

A customer with no module or document overrides and no customer roles gets no output at all.

### Narrowed documents

When a customer's document override leaves out an attribute of the vanilla document, the document is narrowed:

- **The vanilla class becomes abstract** and loses that property.
- **A concrete `<Document>Vanilla` class** carries the dropped property. Customers without an override use it.
- **Every customer that overrides the document gets its own `<Document>Override`**, even if its override adds nothing, and its mapping maps that class.

### Hand-written extensions

- **A vanilla extension.** When the vanilla document has a hand-written `<Document>Extension`, a customer's `<Document>Override` extends that extension, and Skyve uses the `<Document>Override` for the customer.
- **A customer's own extension.** A customer with its own `customers/<customer>/modules/<module>/<Document>/<Document>Extension.java` always gets a `<Document>Override`, even if its override adds nothing. The `<Document>Override` is then abstract, the customer's extension extends it, and Skyve and the customer's mapping use the customer's extension.
- **A narrowed document with an extension.** The hierarchy is `<Document>` <- `<Document>Vanilla` <- `<Document>Extension`. The generated `<Document>Vanilla` is abstract, like any generated class with an extension, and declares the dropped properties. The extension must extend it. Customers without an override get the extension, and the vanilla mapping maps it. A customer's `<Document>Override` inherits the dropped properties too, so it declares only the properties the vanilla document doesn't have.
- **The `extends` check.** Generation checks the vanilla extension's `extends` clause: it must name `<Document>Vanilla` while an override narrows the document, and the domain class otherwise. With validation on, a wrong clause stops generation. With validation off (`generateDomain` with validation disabled, or `false` as the generator's first argument) it is printed as a warning and generation carries on, so you can regenerate while you fix the extension. An extension that extends a class of your own in between isn't checked.

### Generated classes and mappings

- **Generated class names aren't dodged.** Skyve finds the `<Document>Vanilla` and `<Document>Override` classes by name, so when one of them is generated, a document in the same module named like it, in any letter case, stops generation and asks you to rename it.
- **Customer classes refer to customer classes.** In a new customer document's class or a `<Document>Override`, a reference or inverse to a document for which the customer has a `<Document>Override` is typed as that `<Document>Override`, so the accessors can keep an inverse that only the customer's override declares up to date. Vanilla classes, and properties a `<Document>Override` inherits, keep the vanilla types.
- **A longer length widens the shared column.** When a customer's override gives an attribute a longer `<length>` than the vanilla document, the vanilla mapping uses the longest length, as the customers share the table.

### Cleaning customer output

- **What is deleted.** Before writing, `generateDomain` deletes every file directly in `customers/<customer>/modules/<module>/domain/` and `customers/<customer>/domain/`, and `customers/<customer>/modules/orm.hbm.xml`, all below the generated source path. It then deletes the directories this leaves empty, up to `customers/<customer>/` and an empty `customers/`. So output for an override or role you remove disappears, and repeated runs give the same result.
- **Keep your own files elsewhere.** Files of your own in those directories are deleted too. Everything else under `customers/` is left alone, so a project whose generated source path is its source path keeps its customer metadata, resources and module overrides.
- **Blocked paths stop generation.** Before deleting anything, generation checks where it will write. It stops if something other than a generated file is in the way of a customer file, if a directory on the way is a file, or if `customers/` or a customer, `modules`, module or `domain` directory on the way is a symbolic link. Links are never followed, and a file left over from a document renamed only in letter case is replaced.

### Overrides in an inheritance hierarchy

When a customer overrides a document that extends another document, or that other documents extend, the customer mapping copies the whole hierarchy from the document at its top, with the customer's entity names, as the vanilla mapping is built. Each document in it maps the customer's version of the document: its `<Document>Override` if it has one, otherwise the vanilla class (or the `<Document>Vanilla` class of a narrowed document). Documents copied into a customer's mapping because an overridden document refers to them, is their parent or child, or is referred to by them, map their classes the same way.

Only a document that no other document extends can have a `<Document>Override`, because a generated subclass can't extend both the vanilla class and a customer class of its base. So generation stops, naming the customer, the document and what to do instead, when:

- a customer's override of a document that other documents extend adds properties - add them to the vanilla document, or to the overrides of the documents that extend it;
- a customer's override leaves out an attribute of a document that other documents extend - keep the attribute in the override and hide it in the customer's views;
- a new customer document - one with no vanilla document - extends another document, whatever its strategy, as the class generated for a new customer document can't extend another document's class - add it to the vanilla module, or declare the attributes it needs in it instead of extending.

## Customer roles

A customer can declare customer roles in `customer.xml` under `<roles>`. Each is a named bundle of module roles. For a customer that declares any, `generateDomain` writes `<Customer>CustomerRole` in `customers.<customer>.domain`. For example, `customers.test.domain.TestCustomerRole.TEST_MANAGER` names the `test` customer's `TestManager` role.

- **`CustomerRoleReference`** (`org.skyve.metadata.customer`) is the interface the enum implements. It has `customerName()`, `roleName()`, `toCustomer()` and `toRole()` → `CustomerRole`, which throw `MetaDataException` if the customer or the role doesn't exist.
- **`AssignableRole`** (`org.skyve.metadata.user`) is the common parent of `ModuleRole` and `CustomerRoleReference`. Its `assignedRoleName()` is the name a group or user role assignment stores: `module.Role` for a module role, such as `admin.BasicUser`, and the bare name for a customer role, such as `TestManager`.
- **`ModulesUtil.configureGroup(String, AssignableRole, AssignableRole...)`** configures a group with module and customer roles by reference. For example, `configureGroup("Managers", AdminRole.BASIC_USER, TestCustomerRole.TEST_MANAGER)`. It sits in the deprecated `ModulesUtil` for now, and moves when `ModulesUtil`'s group helpers get a home that isn't deprecated.
- **`isInRole` doesn't take customer roles.** Skyve expands a customer role into its module roles when the user signs in, so a user is only ever checked against module roles. Use `CustomerRoleReference` to assign roles, and `ModuleRole` to check them.

## When to use strings

Use the string forms when the name isn't fixed when the application is built:

- **Dynamic domains.** Documents and modules defined at run time, and metadata built with the fluent API.
- **Code that works with a customer's metadata at run time.** For example, code that iterates a customer's roles or documents.
- **Names held as data.** For example, a role name read from configuration or from a `{role:...}` expression.

## Testing with mocks

The typed overloads are `default` methods. A Mockito `mock(User.class)` doesn't run them: an unstubbed `isInRole(AdminRole.X)` returns `false`, even when `isInRole("admin", "X")` is stubbed. Stub the overload the code under test calls. With Skyve's Mockito setup, `mock(User.class, CALLS_REAL_METHODS)` can't run an interface's default methods. To exercise the delegation itself, mock an `abstract` class that implements the interface, with `CALLS_REAL_METHODS`.

## Upgrading

Work through this list when you upgrade a project to this version.

- **Regenerate the domain.** The generated classes and mappings change, so regenerate before you build.
- **Rename `*Ext` references.** The classes generated for customer overrides used to be named `<Document>Ext`. They are now `<Document>Vanilla` (the concrete vanilla class of a narrowed document, in `modules.<module>.domain`) and `<Document>Override` (a customer's override class, in `customers.<customer>.modules.<module>.domain`). Rename any hand-written references to the old classes, such as a customer extension's `extends` clause or a cast.
- **Customer domain generation now always runs.** It used to run only when you had created `src/generated/java/customers/<customer>/modules/` yourself. Now a customer override of a persistent document gets `customers/<customer>/modules/orm.hbm.xml`, and its beans are persisted under the customer's entity name, such as `testtestOverrideAdds`, instead of the vanilla `testOverrideAdds`. Review:
  - hand-written HQL and BizQL that names entities;
  - second-level cache regions configured by entity name;
  - the schema, as a customer's override can add columns to the shared table, and a longer `<length>` in an override widens the shared column.
- **Narrowing breaks vanilla code that uses the dropped properties.** If a customer's override leaves out a vanilla attribute, the vanilla class becomes abstract and loses that attribute's accessors. Vanilla code that calls them, or `new <Document>()`, no longer compiles. Create beans with the generated `<Document>.newInstance()`, which makes the customer's bean class, and reach a dropped property through `<Document>Vanilla` or a binding, for the beans that have it.
- **An extension of a narrowed document must extend `<Document>Vanilla`.** Generation tells you, as an error with validation on and a warning with it off.
- **New generation refusals.** Generation now stops, saying what to do, when:
  - a document is named like a generated `<Document>Vanilla` or `<Document>Override` class, in any letter case;
  - a customer's override of a document that other documents extend needs its own class;
  - a customer's override narrows a document that other documents extend;
  - a new customer document extends another document;
  - a customer output path is blocked by a file that isn't generated, or runs through a symbolic link.
- **Customer output is deleted on every run.** Move any files of your own out of the generated customer `domain/` directories and `orm.hbm.xml`.
- **`DataBuilder` checks factory beans for an overriding customer.** For a customer that overrides the document, a factory method that returns the vanilla class is now used, but a bean it returns that isn't of the customer's bean class is discarded with a warning, and a random bean is built instead.
- **Metadata lookups throw `MetaDataException`.**
  - `Module.getDocument` throws `MetaDataException` instead of `IllegalArgumentException` for a document the module doesn't declare or reference.
  - `Module.getJob` throws `MetaDataException` for a name that isn't a job's, instead of `ClassCastException`.
  - `Module.getMetaDataQuery`, `getBizQL`, `getSQL` and `getRole` return `null` for a name that isn't of their kind, instead of throwing `ClassCastException`. `getBizQL` and `getSQL` are now `@Nullable`; the new `getNullSafeBizQL` and `getNullSafeSQL` throw `MetaDataException` instead.
  - `Persistence.newNamedBizQL` and `newNamedSQL` throw `MetaDataException`, naming the query and module, for a name that isn't a BizQL or SQL query's.
- **Eviction loads metadata again.** `evictCachedMetaData` now puts the evicted keys back, so evicted metadata is loaded again on next access instead of being missing.
