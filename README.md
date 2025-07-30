[![Logo][skyve-logo]][skyve-url]

Welcome to Skyve!

This repository is the Java implementation of the Skyve framework specification.

### Contents

* [What is Skyve](#what-is-skyve)
* [Contributing](#contributing)
* [Getting Started](#getting-started)
  * [Skyve Tutorial](#skyve-tutorial)
  * [User Guide](#user-guide)
  * [Development Guide](#development-guide)
  * [Skyve Cookbook](#skyve-cookbook) 
* [Creating a new Skyve project](#creating-a-new-skyve-project)
  * [Before you start](#before-you-start)
  * [Overview](#overview)
  * [Detailed Instructions](#detailed-instructions) 
 * [Quick start](#quick-start)
   * [Core Concepts](#core-concepts)
   * [Simple Example](#simple-example)
 * [Skyve Maven commands](#skyve-maven-commands)
 * [Updating Skyve version](#updating-skyve-version)
   * [Preserving customisations](#preserving-customisations)

## What is Skyve?

Skyve is an open-source low-code platform that gives you access to all of the key capabilities needed to build sophisticated, robust and scalable cloud solutions. 

Skyve is platform/operating-system independent, works with all common database types, and is accessible through all common browsers and devices.

By incorporating and integrating a range of other open-source technologies to handle persistence, rich UI, security, navigation, reporting, jobs, content, spatial, mobile integration; Skyve provides a platform with all the technology specific areas required to support the Skyve standard for enterprise applications.

Skyve also provides sophisticated validation and a high-level API so that you can build powerful enterprise SaaS solutions.

At any time, branch out into "traditional" development without restriction, but will all the benefits of the API and integrated platform.

Skyve supports spatial concepts natively with MySQL (and MariaDB), SQL Server, Postgres and H2 - Oracle is in beta. Otherwise, pretty much anything supported by Hibernate should work (but we haven't tested them all!).

For more details on the framework and its capabilities, please check out the platform homepage - [www.skyve.org](https://skyve.org/).

Skyve is created by [Biz Hub Australia](https://www.bizhub.com.au/) and we offer a range of support agreements if required. Or use it for free, no obligation.

## Contributing

We welcome new contributions to the project, however small. Please refer to our [contribution guidelines](/CONTRIBUTING.md) for information about contributing to Skyve.

## Questions

If you've found a bug or want to request a new feature, please create a [GitHub Issue](https://github.com/skyvers/skyve/issues/new/choose).

If you need help getting started with or extending Skyve, please reach out on [Slack](https://join.slack.com/t/skyveframework/shared_invite/enQtNDMwNTcyNzE0NzI2LTRkMWUxZDBlZmFlMmJkMjQzYWMzYWQxMmQzYWQ1ZTdlODNkNjRlYzVhYjFmMmQ4NTlhYWY4MjNhMGVkZGNlMjY). Someone from the community or a Skyve developer will be happy to assist. Or use the [skyve tag on StackOverflow](https://stackoverflow.com/tags/skyve) and ask for help there.

## Getting Started

The [Skyve platform website](https://skyve.org/) which hosts training videos, links to a comprehensive development guide, Skyve tutorial as well as a cookbook.

### Skyve Tutorial

[This tutorial](https://skyvers.github.io/Aged-care/) walks through building a sample application using Foundry and no-code, setting up your development environment and building upon it with low-code, and finally some more advanced logic with code.

### User Guide

The user guide is available at [https://skyvers.github.io/skyve-user-guide/](https://skyvers.github.io/skyve-user-guide/). The user guide is for end users of Skyve applications and describes how to navigate around the user interface and make use of the built in functions that ship with Skyve.

### Development Guide

The development guide is available at [github.com/skyvers/skyve-dev-guide](https://github.com/skyvers/skyve-dev-guide). This contains detailed documentation covering the architecture and guiding principles of the framework, as well as explaining all the features and how to get started.

### Skyve Cookbook

The Skyve Cookbook is available at [github.com/skyvers/skyve-cookbook](https://github.com/skyvers/skyve-cookbook). This contains code samples of advanced usage such as REST API configuration and troubleshooting advice.

### Architecure Document

This document is a work in progress and can be found [here](/docs/architecture.md)

## Creating a new Skyve project

We recommend starting your Skyve experience with Skyve Foundry - https://foundry.skyve.org/foundry.

Skyve Foundry provides a point and click project creator: you can quickly declare the data model for your project, deploy and test your project using Skyve Foundry's Free Trial server.

Once your foundry project is created, you can use the collaboration feature to connect Skyve Foundry to a secure git code repository, then continue making changes using both Skyve Foundry, or in your own preferred Integrated Development Environment (IDE) (Eclipse, IntelliJ etc) on your local development infrastructure.

Using Skyve Foundry to create your project will not prevent you deploying the application you create to your own infrastructure if preferred, but it will enable you to quicky deploy and test your application. You can also export your project at any time.

### Overview

1. Create your project with Skyve Foundry, or by using the Skyve project creator.
 a. for Skyve Foundry - create a new git repository project (via github or gitlab etc) and set the in repository URL on the collaboration tab in Skyve Foundry, then push your project to the repository (video steps are available at https://youtu.be/G3OQu5PeUn8)
 b. for the project creator, create a new Skyve project download and receive the link to the file via email  (video steps are available at https://youtu.be/DkdJ7ROYoxc)
2. Import the project as a maven project and run the Generate Domain run configuration.
3. Configure your application instance properties `.json` file and data source `-ds.xml` files and place them into the Wildfly deployment folder. In the `.json` settings file, you will need to (at a minimum) specify the location for Skyve to store 'content' and specify an environment identifier string to be able to get it with a bootstrap credential (also specified in the `.json` file)
4. Sign into your application at `localhost:8080/<projectName>` with your bootstrap credentials and begin using the no-code application.

### Detailed Instructions

### Before you start

These instructions assume the use of Eclipse with the JBoss Server Tools plugin installed, and Wildfly as the application server.

* Install a Java 11+ JDK for your operating system.
* Install Eclipse or an alternative Java based Integrated Development Environment.
* Install JBoss Wildfly - Our instructions are for Wildfly 20+ - you may be able to use older versions and other application servers if you're familiar with configuration.

### Creating your project using Skyve Foundry
* Go to https://foundry.skyve.org/foundry and register for an account
* Sign in with your account
* On the Describe tab, enter a name and description for your application
* On the Design tab, create documents and attributes
* On the Customise tab, upload your logo and icons and select from available themes
* On the Deploy tab, enter a password for a setup user account, then deploy your application
* On the Collaboration tab, enter the URL for your git code repository and push your project to the repository. You can then pull your project to your local development environment.
Video instructions are available using the "How to get started" icon in the title bar of Skyve Foundry

#### Creating your project using the Skyve project creatorr
* Go to https://foundry.skyve.org/foundry/project.xhtml
* Enter a valid email address
* Enter project name
* Enter a customer name (should be a single string like myOrganisationName, or set it to _skyve_ if unsure)
	* Customer is a core concept for Skyve applications to support multi-tenant SaaS applications. Because Skyve is intended and designed for multi-tenancy, data is assumed to exist within a customer (i.e. tenant) context. To understand more about the Customer concept and multi-tenant applications, see https://github.com/skyvers/skyve-dev-guide#multi-tenant--mass-customisation).
* Choose your preferred database type (and dialect) 
	* H2 is a file based database perfect for quick prototyping or getting started if you're not sure
	* Other dialects will require installation of the respective database engine or access to a network DB server of that type.
* If you have a Skyve Script file, supply it here. Skyve Script is a simple markup standard for declaring domain models and is sufficient for creating Skyve no-code applications.
* Press the Create Project button

#### Import the project
* After a few seconds, an email will be sent to the nominated email address with a link to download the project artefacts as a single zip file. Click the link to download the file.
* Once the file has downloaded, unzip the contents to your development workspace location.
* In Eclipse, choose File -> Import and choose _Existing Maven Projects_ and follow the wizard selecting the directory you unzipped from the email.
* To build your project and prepare it to run, from the Run menu choose Run Configurations. Under Maven Build, choose the _Generate Domain_ run configuration for your project name. For your project to run, Skyve must generate required domain classes and maven will ensure that all related open-source components are included.
* Once domain generation is completed, your application is ready to deploy.

#### Configure the application server and database
* To configure Wildfly to deploy your application:
	* Copy the project configuration files to `wildfly/standalone/deployments/`
		* The project root contains a data source file (`projectName-ds.xml`) and a json instance settings file (`projectName.json`).
		* The datasource file declares the data source connection name to the nominated database engine. The JDBC connection string and associated credentials settings must be valid for the selected database engine. Skyve will create all required tables, so an empty database is required. For an H2 database, this will specify the path to the database file's location.
		* The json settings file contains the settings specific to the application instance and includes credentials for a boostrap user to get your started.
		* You will need to configure the `content: { directory:` path within your JSON to be a valid directory on your filesystem. This is where any uploaded files will be stored and indexed by Elastic Search. Create a new directory in your file system at this path, Skyve will not create this for you.
    * You will need to create an `addins` directory either within your content directory (where Skyve will look by default), or configure `addins: { directory:` within your `projectName.json` to the location of the addins directory.
    * Copy `skyve-content.zip` from your project target directory into your addins directory (if not present in target, this can be triggered by performing a maven compile or maven install)
	* If you selected a database engine other than H2, you will need to create a schema (MySQL) or database name (MSSQL) matching your specified projectName. If you want to use a different database or schema name, you will need to modify the `projectName-ds.xml` file in the deployments folder accordingly.
		- You may also need to configure Wildfly appropriately - for example, for MSSQL, you'll need to place the `sqljdbc42.jar` and `sqljdbc_auth.dll` into `\wildfly\modules\system\layers\base\com\microsoft\sqlserver\main\`
	* To deploy your application, right-click the Wildfly server node in the Eclipse server window and add your project. Then start the server using the start tool on the Server window toolbar.

#### Sign in
* Open your preferred browser and navigate to `localhost:8080/<projectName>`.
* Sign in with the credentials specified in the `boostrap` stanza of the json settings file (ensure that you have specified an environment identifier string  - e.g. `identifier: "dev"`
* Once signed in, use the Security Admin section of the admin module to create a user group with required roles, and create users as required.

Note that the bootstrap user only has effect if there is no user with the same name, so if you already had a bootstrap user in your database, you can either:
• truncate your database and start again, OR
• add the role to your user via the admin module, OR
• create a security group with the roles you need, and assign membership to your user

For either of the last two, you'll need to sign-out and then sign back in again for the change to permissions to take effect.

## Quick start

### Core Concepts

Creating applications with Skyve does involve some new concepts and does have a learning curve.Before we start building an application, it is useful to have at least a basic understanding of what an application consists of. 

A Skyve Java no-code application can be created in its simplest form by specifying only metadata (in the form of XML files). All Skyve applications include an _admin_ module, and additional modules are defined for specific application functionality. A high level metadata structure follows the following pattern

* `src/main/java/`
  * modules
    * admin
    * newModule
      * Document
        * `Document.xml`
        * `DocumentBizlet.java`
        * actions
          * `Action.java`
        * views
          * `edit.xml`

#### Modules

Modules define a grouping of application functionality and correspond to top-level menu items. Each module directory contains a `module.xml`, as well as sub-directories per Document.

#### Documents

Skyve uses the term document to indicate the business-focused nature of application objects. These can be thought of as entities, or at a simplistic level correspond to a database table (not always the case). Documents can appear as child level menu-items of the parent module, and are used to define new views within your application.

#### Views

Skyve will automatically generate a user interface screen (view) for each Document. View files are defined to override the generated view if customisation is required. View files are located within the _views_ directory of the parent Document.

See the [Skyve Development Guide](https://github.com/skyvers/skyve-dev-guide) for much more detailed explanations of these concepts.

### Simple Example

In this example, we are going to walk through creating a new module and document. This assumes the [Creating a new Skyve Project](#creating-a-new-skyve-project) steps have been followed, and you have a new Skyve project in your IDE, building and deploying.

**Add a new module**

Our project comes with an _admin_ module which includes a lot of core system functionality, but we will add a new module to store our custom domain.

Run the following maven command against your project: 
```
mvn skyve:addModule
```
When prompted for a module name, enter `todo`.

You may need to refresh your IDE workspace, but you should be able to see a new todo package, and a `todo.xml`. This command also added the todo module to our customer.xml file. Inpsect `todo.xml` and see that it is pretty empty for now, so lets add a basic definition to include the document we're about to add.

```xml
<?xml version="1.0" encoding="UTF-8"?>
<module xmlns="http://www.skyve.org/xml/module" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" title="todo" name="todo" xsi:schemaLocation="http://www.skyve.org/xml/module ../../schemas/module.xsd" prototype="true">
    <homeRef>list</homeRef>
    <homeDocument>ToDo</homeDocument>
    <documents>
        <document ref="ToDo"/>
    </documents>
    <roles>
        <role name="Maintainer">
            <description>Create, edit and delete permission within the ToDo module.</description>
            <privileges>
                <document name="ToDo" permission="CRUDC"/>
            </privileges>
        </role>
    </roles>
    <menu>
        <list document="ToDo" name="All ToDos">
            <role name="Maintainer"/>
        </list>
    </menu>
</module>
```

This basic module definition specifies the home document (landing page), which documents are in the module, the roles and their permissions per document, and the menu. 

Note - this example module has `prototype="true"` set - this means Skyve will make some basic assumptions to simplify the experience for the user - for more information see  [skyvers.github.io/skyve-dev-guide/modules/#prototype-mode](https://skyvers.github.io/skyve-dev-guide/modules/#prototype-mode).

**Add a new document**

Run the following maven command against your project: 
```
mvn skyve:addDocument
```
When prompted for a module name, enter `todo`, and for a document name, enter `ToDo`.

Refresh your workspace again if required, then you should see a ToDo directory under your todo package, and a `ToDo.xml`. Skyve convention is to use camel case (or lowercase) for module names, and title case for document names.

If you inspect our new `ToDo.xml` you will see that it is pretty empty, so lets give it some attributes to store data in.

```xml
<?xml version="1.0" encoding="UTF-8"?>
<document xmlns="http://www.skyve.org/xml/document" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" name="ToDo" xsi:schemaLocation="http://www.skyve.org/xml/document ../../../schemas/document.xsd">
	<persistent name="TODO_ToDo"/>
    <singularAlias>ToDo</singularAlias>
    <pluralAlias>ToDos</pluralAlias>
    <bizKey expression="{description}"/>
    <attributes>
    	<text name="description" required="true">
    		<displayName>Description</displayName>
    		<length>200</length>
    	</text>
    	<boolean name="complete">
    		<displayName>Complete</displayName>
    		<defaultValue>false</defaultValue>
    	</boolean>
    </attributes>
</document>
```

This basic document definition specifies the persistent name (database table), the business key, singular and plural aliases (for showing in list and single edit views), and the attributes which make up this document.

**Generate Domain**

Run the following maven command against your project: 
```
mvn skyve:generateDomain
```
This will validate all your project metadata and warn you if there are any errors. If everything is ok, it will generate application code based on the metadata, and you will see a build success message.

You should now be able to start your server and deploy your application to test it.

Once deployed, you will need to give your user the role "Maintainer" - you can either do this by adding the role to your user, or by creating a security group with that role, and assigning that group membership to your user - then sign out - the new role permissions will only take effect once you sign out and sign back in again.

When you sign back in, you will see a todo module menu, with a child menu item of All ToDos. Clicking on All ToDos, then clicking the `+` button, you should be able to create a new ToDo. This is the Skyve generated view, which contains our two Document attributes, _description_ and _complete_.

## Skyve Maven commands
New projects created from the website come with pre-configured maven run configurations for Eclipse. These are standard maven goals using the Skyve maven plugin and can be run from the command line or another IDE. The 6 main goals are described here:

### Generate Domain
```
mvn skyve:generateDomain
```
Generate domain validates and compiles the metadata (XML files) in your project and checks that the application domain is in a valid state. Errors for the developer to fix are written to your console, and if generate is successful, the domain will be compiled to produce Java domain files and unit tests.

### Generate Edit View
```
mvn skyve:generateEditView
```
Generate edit view requires to additional parameters, a `module` and `document` key value pair. If no edit.xml is specified for a document, Skyve will create a scaffolded view automatically using the attributes specified in the document. When customising a view, it is useful to start from the scaffolded view and extend it, this command will write a `generatedEdit.xml` file to the package specified by the module and document parameters.

### Generate Default Queries
```
mvn skyve:generateDefaultQueries
```
Similar to having a scaffolded edit view for new documents, when documents are shown in a list from a menu or in a lookupDescription, the /default query/ will be used which defines which columns are shown. This maven command can write out all the default queries to a file in the project root so any queries can be tweaked and included in your module.xml.

### Skyve Script
```
mvn skyve:script
```
This will look for a file called `skyve.md` inside a script directory in your project root. Any modules and documents found inside this file will be generated and added to your project. For more user feedback, this can also be performed via the UI from admin -> Document Creator.

### Update Resources
```
mvn clean compile war:exploded
```
Depending on how you configure your Wildfly, if you are not publishing changes during development into `wildfly/standalone/deployments`, you can use this maven command to update your local `/deployments/` directory with the compiled project. Your Wildfly deployment scanner can then be set to watch this location.

### Local Deploy
```
mvn compile war:exploded skyve:touch
```
This refreshes your project's `/deployments' directory and creates a 'projectName.dodeploy' file telling Wildfly to restart the module. This is used when there are any Java or module changes which are cannot be hot-reloaded.

### Add Module
```
mvn skyve:newModule
```
This will prompt you for the new module name, then create a new module directory and module.xml with the specified name. It will also update your customer.xml with the new module. Note: the new module will not pass generate domain, some required fields will be missing (such as the default view).

### Add Document
```
mvn skyve:newDocument
```
This will prompt you for a module name, and the new document name, then create the new document directory and document.xml in the correct location within your project structure. This will also register the new document with the module.

### Add Scaffolded Document
```
mvn skyve:newScaffoldedDocument
```
This performs everything Add Document does, as well as creating an edit view, Bizlet, Factory, Extension and Service classes in the correct location within your project structure.

### Add Action
```
mvn skyve:newAction
```
This will prompt you for a module name, document name and action name and create an empty action ready to add business logic to. Note: the action will need to be manually added to any edit views and privileges within your module as required.

## Updating Skyve version

To update your project with a specific Skyve version, you will need to pull/check-out the Skyve project (from https://github.com/skyvers/skyve.git) prior to the following steps, ensuring you pull the specific Skyve version you're after. If in doubt, pull Skyve and check which version is retrieved. All Skyve releases are tagged, so it is typically safest to checkout the last tagged commit.

⚠️ **Warning:** before continuing, make sure your project is under source control, and all files are committed locally. Upgrading a project can change lots of files, and will update your admin module and web resources. Any local changes you have made will be overwritten and need to be merged back in manually (see [preserving customisations](#preserving-customisations)).

### Configuring the assemble target

These instructions apply to projects created using the [Creating a new Skyve Project](#creating-a-new-skyve-project) process above. If you created your project manually, these steps may differ.

- If using Eclipse, create a new Run Configuration target, setting the base directory to your project's workspace, and setting the goal to `skyve:assemble`. Once setup in your pom this can also be run from the command line with `mvn skyve:assemble`.
- In your project's `pom.xml`, update the `skyve.version` property to match the version of Skyve you pulled/checked out
    - **Note:** confirm that your Skyve checkout matches the version you are upgrading to, this is typically the last tagged commit, not the latest changes on the master branch
    - Find the Skyve plugin (search for artifactId `skyve-maven-plugin`) and configure the `<skyveDir></skyveDir>` setting with a relative or absolute path to the Skyve project local drive location (where you pulled to)
    - Set your `<customer></customer>` to match the customer in your project
    - Save your `pom.xml`
- Run the assemble target you have just created, resolving any reported issues
- When successful, run your project's generate domain target, resolving any reported issues
- When successful, run your project's unit tests, checking the upgrade did not interfere with any expected behaviour
- When successful, run your project's generated tests
- Deploy your project locally and sanity check everything still works correctly
- When satisified, commit the changes to your project

### Preserving customisations

Skyve applications can, and are encouraged to be, customised to make them your own. This can include making changes to the admin module,  changing or customising themes or adding custom client-side logic.

During an assemble operation, the following directories and their contents will be overwritten:

- `src/main/java/modules/admin/*` - The admin module files
- `src/main/java/modules/*.java` - Any Java files in the root of the module package
- `src/main/java/resources/*` - Resource files
- `src/main/java/schemas/*` - Schema files
- `src/main/java/router/*` - Router files
- `src/main/webapp/*` - Web application files including:
  - `*.xhtml` and `*.jsp` files
  - `desktop/*` directory
  - `external/*` directory
  - `pages/*` directory
  - `WEB-INF/*` directory including:
    - `beans.xml`
    - `faces-config.xml`
    - `jboss-classloading.xml`
    - `jboss-deployment-structure.xml`
    - `undertow-handlers.conf`
    - `web.xml`
    - `resources/skyve/*`
- `src/test/java/util/*` - Test utility files

If you have made customisations to any of these files and directories, make sure to:
1. Commit to source control or back up your changes before running the assemble operation
2. Review the changes after assembly
3. Re-apply your customisations if necessary

[skyve-logo]: https://images.squarespace-cdn.com/content/5bac80be16b6407444b95a0c/1604036522472-OMV89LAE4GYP8JV9OC1C/skyve-logo-black.png?content-type=image%2Fpng
[skyve-url]: https://www.skyve.org/