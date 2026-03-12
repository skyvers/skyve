# Skyve Maven Plugin

This project provides the Maven plugin used to build, validate and upgrade Skyve projects.

## Usage

Run plugin targets using the `skyve` goal prefix:

```bash
mvn skyve:<target>
```

Examples:

```bash
mvn skyve:generateDomain
mvn skyve:assemble -DskyveDir=/path/to/skyve -Dcustomer=skyve
```

## Available Targets

| Target | Description | Notes |
| --- | --- | --- |
| `skyve:assemble` | Assemble/update a project from a Skyve source directory (or template project) and perform pre-assemble cleanup. | Requires `-DskyveDir` and `-Dcustomer`. Optional `-DtemplateDir`. |
| `skyve:clearBeforeAssemble` | Run the cleanup step used before assemble. | Requires `-Dcustomer`. |
| `skyve:compileJasperReport` | Compile one or more `.jrxml` reports to `.jasper`. | Prompts for report name. |
| `skyve:flutter-init` | Generate a baseline Flutter client from a Skyve project. | Prompts for missing `targetDir`, `uxui`, `projectName`, `customer`. Supports `overwrite` and `modocWhitelist`. |
| `skyve:generateDefaultQueries` | Generate default query definitions for a module/customer. | Uses config or prompts for customer/module. |
| `skyve:generateDomain` | Validate metadata and generate domain/test code. | Requires `generateDomainConfig` in plugin configuration. |
| `skyve:generateEditView` | Generate scaffolded edit views for a document. | Uses config or prompts for customer/module/document. |
| `skyve:newAction` | Create a new server-side action class under a document. | Prompts for module/document/action names. |
| `skyve:newDocument` | Create a new document folder/metadata and register it in module metadata. | Prompts for module/document names. |
| `skyve:newModule` | Create a new module and add it to customer metadata when available. | Prompts for module name and customer (if needed). |
| `skyve:newScaffoldedDocument` | Create a new document plus scaffolded extension, bizlet, factory, service and edit view. | Requires `generateDomainConfig`; may prompt for customer. |
| `skyve:newService` | Create a service class for a new document. | Prompts for module/document names. |
| `skyve:script` | Apply a Skyve script file to generate/update modules/documents. | Requires `-DskyveDir` and `-Dcustomer`. Optional `-DscriptPath` (default `script/skyve.md`). |
| `skyve:systemDocumentation` | Generate system documentation PDF (`target/system-documentation.pdf`). | Optional `customer` and `excludedModules`. |
| `skyve:touch` | Touch deployment marker file for local app server redeploy. Useful for using a deployment scanner to point at this applications `deployments` directory when using an IDE which does not support publishing to Wildfly. | Optional `touchFile`; default is `deployments/<artifactId>.war.dodeploy`. |
