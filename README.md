# Skyve

This repository is the Java implementation of the Skyve framework specification.

### Contents

* [What is Skyve](#what-is-skyve)
* [Getting Started](#getting-started)
  * [Development Guide](#development-guide)
  * [Skyve Cookbook](#skyve-cookbook) 
* [Creating a new Skyve project](#creating-a-new-skyve-project)
  * [Before you start](#before-you-start)
  * [Overview](#overview)
  * [Detailed Instructions](#detailed-instructions) 
 * [Skyve Maven commands](#skyve-maven-commands)
 * [Example Security Domains](#example-security-domains)

## What is Skyve?

Skyve is an open-source low-code platform that gives you access to all of the key capabilities needed to build sophisticated, robust and scalable cloud solutions. 

Skyve is platform/operating-system independent, works with all common database types, and is accessible through all common browsers and devices.

By incorporating and integrating a range of other open-source technologies to handle persistence, rich UI, security, navigation, reporting, jobs, content, spatial, mobile integration; Skyve provides a platform with all the technology specific areas required to support the Skyve standard for enterprise applications.

Skyve also provides sophisticated validation and a high-level API so that you can build powerful enterprise SaaS solutions today.

At any time, branch out into "traditional" development without restriction, but will all the benefits of the API and integrated platform.

Skyve supports spatial concepts natively with MySQL, SQL Server and H2 - Oracle and Postgres coming soon. Otherwise, pretty much anything supported by Hibernate should work (but we haven’t tested them all!).

For more details on the framework and its capabilities, please check out the platform homepage - [www.skyve.org](www.skyve.org).

Skyve is created by [Biz Hub Australia](http://www.bizhub.com.au/) and we offer a range of support agreements if required. Or use it for free, no obligation.

## Getting Started

In addition to the [Skyve platform website](www.skyve.org) which hosts a fully functional demo and training videos, there is also a comprehensive development guide as well as a cookbook.

### Development Guide

The development guide is available at [github.com/skyvers/skyve-dev-guide](https://github.com/skyvers/skyve-dev-guide). This contains detailed documentation covering the architecture and guiding principles of the framework, as well as explaining all the features and how to get started.

### Skyve Cookbook

The Skyve Cookbook is available at [github.com/skyvers/skyve-cookbook](https://github.com/skyvers/skyve-cookbook). This contains code samples of advanced usage such as REST API configuration and troubleshooting advice.

## Creating a new Skyve project

### Before you start:
Install Eclipse or an alternative Java based Integrated Development Environment.

Install JBoss Wildfly - Our instructions are for Wildfly-10.1.0.Final. You may use other versions and other application servers if you're familiar with configuration. 

These instructions assume the use of Eclipse with the JBoss Server Tools plugin installed, and Wildfly as the application server.

### Overview:
1. Use the Project Creator to create a Skyve project download and receive the link to the file via email.
2. Import the project as a maven project and run the Generate Domain run configuration.
3. Configure your application server security domain, create an empty database, and deploy your application.
4. Log into your application at localhost:8080/<projectName> with your bootstrap credentials and begin using the no-code application.

### Detailed Instructions:

#### Creating the Project
* Go to https://apps.bizhub.com.au/apps/project.xhtml
* Enter a valid email address
* Enter project name (should be a single string like projectName)
* Enter a customer name (should be a single string like myOrganisationName
	* Customer is a core concept for Skyve applications to support multi-tenant SaaS applications. Because Skyve is intended and designed for multi-tenancy, data is assumed to exist within a customer (i.e. tenant) context. To understand more about the Customer concept and multi-tenant applications, see https://github.com/skyvers/skyve-dev-guide#multi-tenant--mass-customisation).
* Choose your preferred database type (and dialect) 
	* H2 is a file based database perfect for quick prototyping or getting started if you're not sure
	* Other dialects will require installation of the respective database engine or access to a network DB server of that type.
* If you have a Skyve Script file, supply it here. Skyve Script is a simple markup standard for declaring domain models and is sufficient for creating Skyve no-code applications.
* Press the Create Project button

#### Import the project
* After a few seconds, an email will be sent to the nominated email address with a link to download the project artefacts as a single zip file. Click the link to download the file.
* Once the file has downloaded, unzip the contents to your development workspace location.
* In Eclipse, choose File -> Import and choose Existing Maven Projects and follow the wizard selecting the directory you unzipped from the email.
* To build your project and prepare it to run, from the Run menu choose Run Configurations. Under Maven Build, choose the /Generate Domain/ run configuration for your project name. For your project to run, Skyve must generate required domain classes and Maven will ensure that all related open-source components are included.
* Once domain generation is completed, your application is ready to deploy.

#### Configure the application server and database
* To configure Wildfly to deploy your application:
	* Copy the contents of the deployments folder to `wildfly/standalone/deployments/`
		* The deployments folder contains a data source file (projectName-ds.xml) and a json instance settings file (projectName.json).
		* The datasource file declares the data source connection name to the nominated database engine. The JDBC connection string and associated credentials settings must be valid for the selected database engine. Skyve will create all required tables, so an empty database is required.
		* The json settings file contains the settings specific to the application instance and includes credentials for a boostrap user to get your started.
		* You may need to configure the content directory path within your JSON to be a valid directory on your filesystem. This is where an uploaded files will be stored and indexed by Elastic Search.
	* Declare a Wildfly security domain matching the name of your application (case sensitive), and referencing the skyve Jndi name.
		* To declare a Wildfly security domain, open the file `wildfly/standalone/configuration/standalone.xml` and add a security domain stanza within the `<security-domains>`.
		* The `principalsQuery` and `rolesQuery` needs to use the correct string concatentation syntax for the database engine you are using. An example [H2](#h2-security-domain) and [SQL Server](#sql-server-security-domain) stanza is included at the bottom.
	* If you selected a database engine other than H2, you'll need to create a schema (MySQL) or database name (MSSQL) matching your specified projectName. If you want to use a different database or schema name, you'll need to modify the projectName-ds.xml file in the deployments folder accordingly.
	* To deploy your application, right-click the Wildfly server node in the Eclipse server window and add your project. Then start the server using the start tool on the Server window toolbar.

#### Log in
* Open your preferred browser and navigate to `localhost:8080/<projectName>`.
* Log in with the credentials specified in the boostrap stanza of the json settings file.
* Once logged in, use the Security Admin section of the admin module to create a user group with required roles, and create users as required.

## Skyve Maven commands
New projects created from the website come with pre-configured maven run configurations for Eclipse. These are standard maven goals using the Skyve maven plugin and can be run from the command line or another IDE. The 6 main goals are described here:

#### Generate Domain
`mvn skyve:generateDomain`
Generate domain validates and compiles the metadata (XML files) in your project and checks that the application domain is in a valid state. Errors for the developer to fix are written to your console, and if generate is successful, the domain will be compiled to produce Java domain files and unit tests.

#### Generate Edit View
`mvn skyve:generateEditView`
Generate edit view requires to additional parameters, a `module` and `document` key value pair. If no edit.xml is specified for a document, Skyve will create a scaffolded view automatically using the attributes specified in the document. When customising a view, it is useful to start from the scaffolded view and extend it, this command will write a `generatedEdit.xml` file to the package specified by the module and document parameters.

#### Generate Default Queries
`mvn skyve:generateDefaultQueries`
Similar to having a scaffolded edit view for new documents, when documents are shown in a list from a menu or in a lookupDescription, the /default query/ will be used which defines which columns are shown. This maven command can write out all the default queries to a file in the project root so any queries can be tweaked and included in your module.xml.

#### Skyve Script
`mvn skyve:script`
This will look for a file called `skyve.md` inside a script directory in your project root. Any modules and documents found inside this file will be generated and added to your project. For more user feedback, this can also be performed via the UI from admin -> Document Creator.

#### Update Resources
`mvn clean compile war:exploded`
Depending on how you configure your Wildfly, if you are not publishing changes during development into `wildfly/standalone/deployments`, you can use this maven command to update your local `/deployments/` directory with the compiled project. Your Wildfly deployment scanner can then be set to watch this location.

#### Local Deploy
`man compile war:exploded skyve:touch`
This refreshes your project’s `/deployments’ directory and creates a ‘projectName.dodeploy’ file telling Wildfly to restart the module. This is used when there are any Java or module changes which are cannot be hot-reloaded.

## Example Security Domains
A security domain is required to be added to your Wildfly installation to tell the application server how to authenticate users to the application. This is performed by editing the `wildfly/standalone/
configuration/standalone.xml` file. The `hashAlgorithm` property should match the property in your project .json file.

#### H2 Security Domain
```xml
<security-domain name="<projectName>" cache-type="default">
	<authentication>
		<login-module code="Database" flag="required">
			<module-option name="dsJndiName" value="java:/<projectName>DB"/>
			<module-option name="principalsQuery" value="select password from ADM_SecurityUser where bizCustomer || '/' || userName = ?;"/>
			<module-option name="rolesQuery" value="select 'Nobody', 'Roles' from ADM_SecurityUser where bizCustomer || '/' || userName = ?"/>
			<module-option name="hashAlgorithm" value="SHA1"/>
			<module-option name="hashEncoding" value="base64"/>
		</login-module>
	</authentication>
</security-domain>
```

#### SQL Server Security Domain
```xml
<security-domain name="<projectName>" cache-type="default">
	<authentication>
		<login-module code="Database" flag="required">
			<module-option name="dsJndiName" value="java:/<projectName>DB"/>
			<module-option name="principalsQuery" value="select password from ADM_SecurityUser where bizCustomer + '/' + userName = ?;"/>
			<module-option name="rolesQuery" value="select 'Nobody', 'Roles' from ADM_SecurityUser where bizCustomer + '/' + userName = ?"/>
			<module-option name="hashAlgorithm" value="SHA1"/>
			<module-option name="hashEncoding" value="base64"/>
		</login-module>
	</authentication>
</security-domain>
```
