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
- **Customer domain generation always runs.** For every customer, `generateDomain` writes a `<Document>Override` class in `customers/<customer>/modules/<module>/domain/` for each document override that adds properties (attributes or conditions), a class there for each new customer document, and the customer ORM mapping `customers/<customer>/modules/orm.hbm.xml` for the persistent documents it overrides or adds. It used to do this only when you had created `src/generated/java/customers/<customer>/modules/` yourself. A customer whose overrides are all transient documents gets no mapping file, and a customer with no module or document overrides and no customer roles gets no output at all. A module override always gets the module's enums in `customers/<customer>/modules/<module>/domain/`.
- **An override that drops an attribute changes the vanilla class.** When a customer's document override leaves out an attribute of the vanilla document, the vanilla class loses that property and becomes abstract, with a generated concrete `<Document>Vanilla` class, carrying the dropped property, that customers without an override use. Every customer that overrides the document then gets its own `<Document>Override`, even if its override adds nothing, and its mapping maps that class. This was already the case for projects that had opted in, and now applies to every project with such an override.
- **Generated class names aren't dodged.** Skyve finds the `<Document>Vanilla` and `<Document>Override` classes by name, so a document named like one of them, in any letter case, stops generation and asks you to rename it.
- **Hand-written extensions.** When the vanilla document has a hand-written `<Document>Extension`, a customer's `<Document>Override` extends that extension, and Skyve uses the `<Document>Override` for the customer. The `<Document>Override` is abstract only if the customer has its own `customers/<customer>/modules/<module>/<Document>/<Document>Extension.java`, which then extends it and is used instead. An override that drops an attribute from a document with a hand-written extension doesn't work yet: customers without an override get the extension, which lacks the dropped property.
- **A longer length widens the shared column.** When a customer's override gives an attribute a longer `<length>` than the vanilla document, the vanilla mapping uses the longest length, as the customers share the table. This was already the case for projects that had opted in, and now applies to every project with such an override.
- **Customer output is cleaned on every run.** Before writing, `generateDomain` deletes every file directly in `customers/<customer>/modules/<module>/domain/` and `customers/<customer>/domain/`, and `customers/<customer>/modules/orm.hbm.xml`, all below the generated source path. It also deletes a `metadata-enums-only.txt` that an earlier version wrote. It then deletes the directories this leaves empty, up to `customers/<customer>/` and an empty `customers/`. So output for an override or role you remove disappears, and repeated runs give the same result. Don't keep your own files in those directories, as they are deleted too. Everything else under `customers/` is left alone, and linked directories are not followed, so a project whose generated source path is its source path keeps its customer metadata, resources and module overrides. If something other than a generated file is in the way of a customer file, such as a directory, generation stops before deleting anything.
- **Overrides in an inheritance hierarchy.** When a customer overrides a document that extends another document, or that other documents extend, the customer mapping copies the whole hierarchy from the document at its top, with the customer's entity names, as the vanilla mapping is built. Each document in it maps the customer's version of the document: its `<Document>Override` if it has one, otherwise the vanilla class (or the `<Document>Vanilla` class of a narrowed document). Only a document that no other document extends can have a `<Document>Override`, because a generated subclass can't extend both the vanilla class and a customer class of its base. So generation stops, naming the customer, the document and what to do instead, when:
  - a customer's override of a document that other documents extend adds properties - add them to the vanilla document, or to the overrides of the documents that extend it;
  - a customer's override leaves out an attribute of a document that other documents extend - keep the attribute in the override and hide it in the customer's views;
  - a new customer document extends a document with a `single` or `joined` strategy - add it to the vanilla module, or make it extend a `mapped` document or no document.

  A new customer document may extend a `mapped` document.

## Customer roles

A customer can declare customer roles in `customer.xml` under `<roles>`. Each is a named bundle of module roles. For a customer that declares any, `generateDomain` writes `<Customer>CustomerRole` in `customers.<customer>.domain`. For example, `customers.test.domain.TestCustomerRole.TEST_MANAGER` names the `test` customer's `TestManager` role.

- **`CustomerRoleReference`** (`org.skyve.metadata.customer`) is the interface the enum implements. It has `customerName()`, `roleName()`, `toCustomer()` and `toRole()` → `CustomerRole`, which throw `MetaDataException` if the customer or the role doesn't exist.
- **`AssignableRole`** (`org.skyve.metadata.user`) is the common parent of `ModuleRole` and `CustomerRoleReference`. Its `assignedRoleName()` is the name a group or user role assignment stores: `module.Role` for a module role, such as `admin.BasicUser`, and the bare name for a customer role, such as `TestManager`.
- **`ModulesUtil.configureGroup(String, AssignableRole, AssignableRole...)`** configures a group with module and customer roles by reference. For example, `configureGroup("Managers", AdminRole.BASIC_USER, TestCustomerRole.TEST_MANAGER)`.
- **`isInRole` doesn't take customer roles.** Skyve expands a customer role into its module roles when the user signs in, so a user is only ever checked against module roles. Use `CustomerRoleReference` to assign roles, and `ModuleRole` to check them.

## When to use strings

Use the string forms when the name isn't fixed when the application is built:

- **Dynamic domains.** Documents and modules defined at run time, and metadata built with the fluent API.
- **Code that works with a customer's metadata at run time.** For example, code that iterates a customer's roles or documents.
- **Names held as data.** For example, a role name read from configuration or from a `{role:...}` expression.

## Testing with mocks

The typed overloads are `default` methods. A Mockito `mock(User.class)` doesn't run them: an unstubbed `isInRole(AdminRole.X)` returns `false`, even when `isInRole("admin", "X")` is stubbed. Stub the overload the code under test calls. With Skyve's Mockito setup, `mock(User.class, CALLS_REAL_METHODS)` can't run an interface's default methods. To exercise the delegation itself, mock an `abstract` class that implements the interface, with `CALLS_REAL_METHODS`.
