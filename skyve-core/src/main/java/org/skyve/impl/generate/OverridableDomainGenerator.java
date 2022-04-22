package org.skyve.impl.generate;

import java.io.BufferedWriter;
import java.io.File;
import java.io.IOException;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.math.BigInteger;
import java.nio.charset.StandardCharsets;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.StandardOpenOption;
import java.nio.file.attribute.BasicFileAttributes;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.stream.Collectors;

import org.apache.commons.lang3.ArrayUtils;
import org.skyve.domain.Bean;
import org.skyve.domain.ChildBean;
import org.skyve.domain.HierarchicalBean;
import org.skyve.domain.PersistentBean;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.customer.ExportedReference;
import org.skyve.impl.metadata.model.document.AbstractInverse;
import org.skyve.impl.metadata.model.document.AbstractInverse.InverseRelationship;
import org.skyve.impl.metadata.model.document.CollectionImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.model.document.field.Enumeration;
import org.skyve.impl.metadata.model.document.field.Enumeration.EnumeratedValue;
import org.skyve.impl.metadata.model.document.field.Field;
import org.skyve.impl.metadata.model.document.field.Field.IndexType;
import org.skyve.impl.metadata.model.document.field.LengthField;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.SortDirection;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.Extends;
import org.skyve.metadata.model.Persistent;
import org.skyve.metadata.model.Persistent.ExtensionStrategy;
import org.skyve.metadata.model.document.Association;
import org.skyve.metadata.model.document.Association.AssociationType;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.metadata.model.document.Collection;
import org.skyve.metadata.model.document.Collection.CollectionType;
import org.skyve.metadata.model.document.Collection.Ordering;
import org.skyve.metadata.model.document.Condition;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.Interface;
import org.skyve.metadata.model.document.Inverse;
import org.skyve.metadata.model.document.Reference;
import org.skyve.metadata.model.document.Reference.ReferenceType;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.Module.DocumentRef;
import org.skyve.metadata.repository.ProvidedRepository;
import org.skyve.util.test.SkyveFactory;

/**
 * Run through all vanilla modules and create the base class data structure and the extensions (if required).
 * Run through each customer and generate impls and package hibernate.cfg.xmls.
 * Constrain base classes.
 * Generate base classes.
 */
public final class OverridableDomainGenerator extends DomainGenerator {
	private static class DomainClass {
		private boolean isAbstract = false;
		// attribute name -> attribute type
		private TreeMap<String, AttributeType> attributes = new TreeMap<>();
	}

	/**
	 * Map<persistentName, Map<propertyName, length>>>
	 */
	private TreeMap<String, TreeMap<String, Integer>> persistentPropertyLengths = new TreeMap<>();

	/**
	 * Map<moduleName, Map<documentName, Map<attribueName, AttributeType>>>
	 */
	private TreeMap<String, TreeMap<String, DomainClass>> moduleDocumentVanillaClasses = new TreeMap<>();

	/**
	 * Map<moduleName.documentName, Map<moduleName.documentName, Document>>
	 */
	private TreeMap<String, TreeMap<String, Document>> modocDerivations = new TreeMap<>();

	/**
	 * Set of moduleName.documentName documents already defined in overridden ORM
	 */
	private Set<String> visitedOverriddenORMDocumentsPerCustomer = new TreeSet<>();

	/**
	 * Set of moduleName.documentName documents that should be overridden
	 */
	private Set<String> overriddenORMDocumentsPerCustomer = new TreeSet<>();

	OverridableDomainGenerator(boolean write,
								boolean debug,
								boolean multiTenant,
								ProvidedRepository repository,
								DialectOptions dialectOptions,
								String srcPath,
								String generatedSrcPath,
								String testPath,
								String generatedTestPath,
								String[] excludedModules) {
		super(write, debug, multiTenant, repository, dialectOptions, srcPath, generatedSrcPath, testPath, generatedTestPath, excludedModules);
	}

	@Override
	public void generate() throws Exception {
		populateDataStructures();

		// generate the domain classes for vanilla modules
		for (String moduleName : repository.getAllVanillaModuleNames()) {
			Module module = repository.getModule(null, moduleName);
			generateVanilla(module);
		}

		// generate for customer overrides
		for (String customerName : repository.getAllCustomerNames()) {
			Customer customer = repository.getCustomer(customerName);
			String modulesPath = new StringBuilder(256).append(generatedSrcPath)
														.append(ProvidedRepository.CUSTOMERS_NAMESPACE)
														.append(customerName)
														.append('/')
														.append(ProvidedRepository.MODULES_NAME)
														.append('/').toString();
			File customerModulesDirectory = new File(modulesPath);
			if (customerModulesDirectory.exists() && customerModulesDirectory.isDirectory()) {
				generateOverridden(customer, modulesPath);
				visitedOverriddenORMDocumentsPerCustomer.clear();
				generateOverridden(customer, modulesPath);
			}

			// clear out overrides as we have finished with this customer
			overriddenORMDocumentsPerCustomer.clear();
			visitedOverriddenORMDocumentsPerCustomer.clear();
		}

		// delete generated and generatedTest directories
		if (write) {
			replaceGenerated(repository.getAllVanillaModuleNames());
		}
	}

	private void populateDataStructures() throws Exception {
		// Populate Base Data Structure with Vanilla definitions
		for (String moduleName : repository.getAllVanillaModuleNames()) {
			final Module module = repository.getModule(null, moduleName);

			// Populate the properties map
			final TreeMap<String, DomainClass> documentClasses = new TreeMap<>();
			moduleDocumentVanillaClasses.put(moduleName, documentClasses);

			new ModuleDocumentVisitor() {
				@Override
				public void accept(Document document) throws Exception {
					if (document.isDynamic()) {
						return;
					}

					validateDocumentAttributeNames(document);
					populatePropertyLengths(null, module, document, null);
					String documentName = document.getName();
					DomainClass domainClass = new DomainClass();
					domainClass.attributes = generateDocumentPropertyNames(document);
					documentClasses.put(documentName, domainClass);

					populateModocDerivations(module, document, null);
				}
			}.visit(null, module);
		}

		// TODO how do I handle overridden extended documents?

		// Restrict the base class definitions based on customer overrides
		for (String customerName : repository.getAllCustomerNames()) {
			final Customer customer = repository.getCustomer(customerName);
			String modulesPath = new StringBuilder(256).append(generatedSrcPath)
														.append(ProvidedRepository.CUSTOMERS_NAMESPACE)
														.append(customerName)
														.append('/')
														.append(ProvidedRepository.MODULES_NAME)
														.append('/').toString();
			File customerModulesDirectory = new File(modulesPath);
			if (customerModulesDirectory.exists() && customerModulesDirectory.isDirectory()) {
				for (final Module module : customer.getModules()) {
					final String moduleName = module.getName();
					final TreeMap<String, DomainClass> vanillaDocumentClasses = moduleDocumentVanillaClasses.get(moduleName);

					new ModuleDocumentVisitor() {
						@Override
						public void accept(Document document) throws Exception {
							if (document.isDynamic()) {
								return;
							}

							String documentName = document.getName();
							String key = new StringBuilder(64).append(ProvidedRepository.MODULES_NAMESPACE).append(moduleName).append('/').append(documentName).toString();
							String newKey = repository.vtable(customerName, key);
							if ((newKey != null) && newKey.startsWith(ProvidedRepository.CUSTOMERS_NAMESPACE)) {
								populatePropertyLengths(customer, module, document, null);

								// Refine the moduleDocumentProperties, if the vanilla class exists
								// NB this will not exist if there is a customer document included that is not an override of a
								// vanilla one
								DomainClass vanillaDocumentClass = (vanillaDocumentClasses == null) ?
																		null :
																		vanillaDocumentClasses.get(documentName);
								if (vanillaDocumentClass != null) {
									TreeMap<String, AttributeType> vanillaDocumentProperties = vanillaDocumentClass.attributes;
									TreeMap<String, AttributeType> thisDocumentProperties = generateDocumentPropertyNames(document);
									Iterator<String> i = vanillaDocumentProperties.keySet().iterator();
									while (i.hasNext()) {
										String vanillaPropertyName = i.next();
										if (! thisDocumentProperties.containsKey(vanillaPropertyName)) {
											i.remove();
											vanillaDocumentClass.isAbstract = true;
										}
									}
								}
							}
						}

					}.visit(customer, module);
				}
			}
		}
	}

	// create a map of derived classes for use by the ORM generator and ARC processing.
	private void populateModocDerivations(Module module,
											Document document,
											ExtensionStrategy strategyToAssert) {
		Extends inherits = document.getExtends();
		Persistent persistent = document.getPersistent();
		ExtensionStrategy strategy = (persistent == null) ? null : persistent.getStrategy();
		boolean mapped = (persistent == null) ? false : ExtensionStrategy.mapped.equals(strategy);
		Document baseDocument = (inherits != null) ? module.getDocument(null, inherits.getDocumentName()) : null;

		if ((baseDocument != null) && baseDocument.isDynamic() && (! document.isDynamic())) {
			throw new MetaDataException("Document " + document.getName() + " cannot extend dynamic document " + baseDocument.getName() + " as it is not a dynamic document");
		}
		
		if (persistent != null) {
			if ((strategyToAssert != null) && (! mapped) && (! strategyToAssert.equals(strategy))) {
				throw new MetaDataException("Document " + document.getName() +
												((strategy == null) ? " has no extension strategy" : " uses extension strategy " + strategy) +
												" which conflicts with other extensions in the hierarchy using strategy " + strategyToAssert);
			}
		}

		if ((inherits != null) && (persistent != null)) {
			if (baseDocument == null) {
				throw new MetaDataException("Document " + document.getName() +
												" extends document " + inherits.getDocumentName() +
												" which does not exist in module " + module.getName());
			}

			Persistent basePersistent = baseDocument.getPersistent();
			boolean baseMapped = (basePersistent == null) ? false : ExtensionStrategy.mapped.equals(basePersistent.getStrategy());

			if ((persistent.getName() == null) && (! mapped)) {
				throw new MetaDataException("Document " + document.getName() + " cannot be transient when it is extending document " + baseDocument.getName());
			}
			if ((basePersistent == null) || (basePersistent.getName() == null)) {
				if (! baseMapped) {
					throw new MetaDataException("Document " + document.getName() + " cannot extend transient document " + baseDocument.getName());
				}
			}

			if (ExtensionStrategy.joined.equals(strategy)) {
				Document baseUnmappedDocument = baseDocument;
				Persistent baseUnmappedPersistent = basePersistent;
				if (baseMapped) {
					baseUnmappedDocument = repository.findNearestPersistentUnmappedSuperDocument(null, module, document);
					baseUnmappedPersistent = (baseUnmappedDocument == null) ? null : baseUnmappedDocument.getPersistent();
				}
				if ((baseUnmappedDocument != null) &&
						(persistent.getName() != null) &&
						persistent.getPersistentIdentifier().equals((baseUnmappedPersistent == null) ? 
																		null : 
																		baseUnmappedPersistent.getPersistentIdentifier())) {
					throw new MetaDataException("Document " + document.getName() + " extends document " +
													baseUnmappedDocument.getName() + " with a strategy of " + strategy +
													" but the persistent identifiers are the same.");
				}
			}
			if (ExtensionStrategy.single.equals(strategy)) {
				Document baseUnmappedDocument = baseDocument;
				Persistent baseUnmappedPersistent = basePersistent;
				if (baseMapped) {
					baseUnmappedDocument = repository.findNearestPersistentUnmappedSuperDocument(null, module, document);
					baseUnmappedPersistent = (baseUnmappedDocument == null) ? null : baseUnmappedDocument.getPersistent();
				}
				if ((baseUnmappedDocument != null) &&
						(persistent.getName() != null) &&
						(! persistent.getPersistentIdentifier().equals((baseUnmappedPersistent == null) ? null : baseUnmappedPersistent.getPersistentIdentifier()))) {
					throw new MetaDataException("Document " + document.getName() + " extends document " +
													baseUnmappedDocument.getName() + " with a strategy of " + strategy +
													" but the persistent identifiers are different.");
				}
			}

			putModocDerivation(document, baseDocument);
			Module baseModule = repository.getModule(null, baseDocument.getOwningModuleName());
			populateModocDerivations(baseModule,
										baseDocument,
										(strategyToAssert != null) ? 
											strategyToAssert : 
											(ExtensionStrategy.mapped.equals(strategy) ? null : strategy));
		}
	}

	private void putModocDerivation(Document document, Document baseDocument) {
		String baseModoc = baseDocument.getOwningModuleName() + '.' + baseDocument.getName();
		TreeMap<String, Document> derivations = modocDerivations.get(baseModoc);
		if (derivations == null) {
			derivations = new TreeMap<>();
			modocDerivations.put(baseModoc, derivations);
		}
		derivations.put(document.getOwningModuleName() + '.' + document.getName(), document);
	}

	private void generateVanilla(final Module module) throws Exception {
		final String moduleName = module.getName();

		// Determine the domain folder
		final String packagePath = new StringBuilder(128).append(ProvidedRepository.MODULES_NAMESPACE)
															.append(moduleName)
															.append('/')
															.append(ProvidedRepository.DOMAIN_NAME).toString();

		// Determine the generated test folder
		final String modulePath = ProvidedRepository.MODULES_NAMESPACE + moduleName;
		final Path domainTestPath = Paths.get(generatedTestPath, packagePath);

		// Make a orm.hbm.xml file
		Path mappingFilePath = Paths.get(generatedSrcPath, packagePath, moduleName + "_orm.hbm.xml");
		if (debug) {
			UtilImpl.LOGGER.fine("Mapping file is " + mappingFilePath);
		}

		final StringBuilder filterDefinitions = new StringBuilder(1024);
		StringBuilder mappingFileContents = new StringBuilder(4096);
		createMappingFileHeader(mappingFileContents);

		new ModuleDocumentVisitor() {
			@Override
			public void accept(Document document) throws Exception {
				if (document.isDynamic()) {
					return;
				}
				
				String documentName = document.getName();
				TreeMap<String, DomainClass> domainClasses = moduleDocumentVanillaClasses.get(moduleName);
				DomainClass domainClass = (domainClasses == null) ? null : domainClasses.get(documentName);

				// Generate base
				Path javaFilePath = Paths.get(generatedSrcPath, packagePath, documentName + ".java");
				StringBuilder javaFileContents = new StringBuilder(4096);
				Extends inherits = document.getExtends();
				generateJava(null,
								module,
								document,
								javaFileContents,
								packagePath.replaceAll("\\\\|\\/", "."),
								documentName,
								(inherits != null) ? inherits.getDocumentName() : null,
								false);
				javaFileContents.trimToSize();
				if (write) {
					generation.put(javaFilePath, javaFileContents);
				}

				if ((domainClass != null) && domainClass.isAbstract) {
					// Generate extension
					javaFilePath = Paths.get(generatedSrcPath, packagePath, documentName + "Ext.java");
					javaFileContents = new StringBuilder(4096);
					generateJava(null,
									module,
									document,
									javaFileContents,
									packagePath.replaceAll("\\\\|\\/", "."),
									documentName,
									documentName,
									true);
					javaFileContents.trimToSize();
					if (write) {
						generation.put(javaFilePath, javaFileContents);
					}
				}
				
				generateORM(mappingFileContents,
								module,
								document,
								ProvidedRepository.MODULES_NAME + '.',
								(domainClass != null) && domainClass.isAbstract,
								null,
								filterDefinitions);

				// if this is not an excluded module, generate tests
				if ((excludedModules == null) || 
						(! Arrays.asList(excludedModules).contains(moduleName.toLowerCase()))) {
					// check if this document is annotated to skip domain tests
					File factoryFile = new File(new StringBuilder(256).append(srcPath)
																		.append(modulePath)
																		.append('/')
																		.append(documentName)
																		.append('/')
																		.append(documentName)
																		.append("Factory.java").toString());
					SkyveFactory annotation = retrieveFactoryAnnotation(factoryFile);

					// generate domain test for persistent documents that are not mapped, or not children
					Persistent persistent = document.getPersistent();
					if (persistent != null &&
							(! ExtensionStrategy.mapped.equals(persistent.getStrategy())) &&
							(document.getParentDocumentName() == null)) {
						boolean skipGeneration = false;

						if (annotation != null && !annotation.testDomain()) {
							skipGeneration = true;
						}

						if (! skipGeneration) {
							Path testFilePath = Paths.get(domainTestPath.toString(), documentName + "Test.java");
							// don't generate a test if the developer has created a domain test in this location in the test directory
							if (testAlreadyExists(testFilePath)) {
								if (debug) {
									System.out.println(new StringBuilder(256).append("Skipping domain test generation for ")
																				.append(packagePath.replaceAll("\\\\|\\/", "."))
																				.append('.')
																				.append(documentName)
																				.append(", file already exists in ")
																				.append(testPath));
								}
							}
							else {
								// generate the domain test
								StringBuilder testFileContents = new StringBuilder(2048);
								generateDomainTest(testFileContents, modulePath, packagePath.replaceAll("\\\\|\\/", "."), documentName);
								testFileContents.trimToSize();
								if (write) {
									generation.put(testFilePath, testFileContents);
								}
							}
						}
						else {
							if (debug) {
								System.out.println(new StringBuilder(256).append("Skipping domain test generation for ")
																			.append(packagePath.replaceAll("\\\\|\\/", "."))
																			.append('.')
																			.append(documentName).toString());
							}
						}
					}

					// generate action tests
					generateActionTests(moduleName, packagePath, modulePath, document, documentName, annotation);
				}
			}
		}.visit(null, module);

		createMappingFileFooter(mappingFileContents, filterDefinitions);
		mappingFileContents.trimToSize();
		if (write) {
			generation.put(mappingFilePath, mappingFileContents);
		}
	}

	private SkyveFactory retrieveFactoryAnnotation(final File factoryFile) {
		SkyveFactory annotation = null;

		if (factoryFile.exists()) {
			String className = factoryFile.getPath().replaceAll("\\\\|\\/", ".")
													.replace(srcPath.replaceAll("\\\\|\\/", "."), "");

			if (debug) UtilImpl.LOGGER.fine("Found factory " + className);
			className = className.replaceFirst("[.][^.]+$", "");

			// scan the classpath for the class
			try {
				Class<?> c = Thread.currentThread().getContextClassLoader().loadClass(className);
				if (c.isAnnotationPresent(SkyveFactory.class)) {
					annotation = c.getAnnotation(SkyveFactory.class);
				}
			}
			catch (Exception e) {
				System.err.println("Could not find factory class for: " + e.getMessage());
			}
		}

		return annotation;
	}

	private void generateOverridden(final Customer customer, final String modulesPath)
	throws Exception {
		// Make the orm.hbm.xml file
		Path mappingFilePath = Paths.get(modulesPath + "orm.hbm.xml");
		StringBuilder mappingFileContents = new StringBuilder(2048);

		final StringBuilder filterDefinitions = new StringBuilder(1024);
		createMappingFileHeader(mappingFileContents);

		for (final Module module : customer.getModules()) {
			final String moduleName = module.getName();
			if (debug) System.out.println("Module " + moduleName);
			// clear out the domain folder
			final String packagePath = new StringBuilder(256).append(ProvidedRepository.CUSTOMERS_NAMESPACE)
																.append(customer.getName())
																.append('/')
																.append(ProvidedRepository.MODULES_NAMESPACE)
																.append(moduleName)
																.append('/')
																.append(ProvidedRepository.DOMAIN_NAME).toString();

			// Get the module's document domain classes
			final TreeMap<String, DomainClass> documentClasses = moduleDocumentVanillaClasses.get(moduleName);

			new ModuleDocumentVisitor() {
				@Override
				public void accept(Document document) throws Exception {
					if (document.isDynamic()) {
						return;
					}
					
					String documentName = document.getName();
					String key = new StringBuilder(64).append(ProvidedRepository.MODULES_NAMESPACE).append(moduleName).append('/').append(documentName).toString();
					String documentPackagePath = repository.vtable(customer.getName(), key);
					if ((documentClasses != null) && (documentPackagePath != null) && documentPackagePath.startsWith(ProvidedRepository.CUSTOMERS_NAMESPACE)) {
						// this is either an override or a totally new document.
						// for an override, baseDocumentName != null
						// for a new document definition, baseDocumentName == null
						String vanillaDocumentName = documentClasses.containsKey(documentName) ? documentName : null;

						// bug out if this document is an override but doesn't add any more properties
						if (vanillaDocumentName != null) { // overridden document
							TreeMap<String, AttributeType> extraProperties = getOverriddenDocumentExtraProperties(document);
							if (extraProperties.isEmpty()) { // no extra properties in override
								generateOverriddenORM(mappingFileContents, customer, module, document, filterDefinitions);
								return;
							}
						}

						// Generate extension
						StringBuilder javaFileName = new StringBuilder(256);
						javaFileName.append(generatedSrcPath).append(packagePath).append('/').append(documentName);
						if (vanillaDocumentName != null) {
							javaFileName.append("Ext");
						}
						javaFileName.append(".java");
						Path javaFilePath = Paths.get(javaFileName.toString());
						StringBuilder javaFileContents = new StringBuilder(4096);
						generateJava(customer,
										module,
										document,
										javaFileContents,
										packagePath.replaceAll("\\\\|\\/", "."),
										documentName,
										vanillaDocumentName,
										true);
						javaFileContents.trimToSize();
						if (write) {
							generation.put(javaFilePath, javaFileContents);
						}
						generateOverriddenORM(mappingFileContents, customer, module, document, filterDefinitions);
					}
				}
			}.visit(customer, module);
		}

		createMappingFileFooter(mappingFileContents, filterDefinitions);
		mappingFileContents.trimToSize();
		if (write) {
			generation.put(mappingFilePath, mappingFileContents);
		}
	}

	private TreeMap<String, AttributeType> getOverriddenDocumentExtraProperties(Document overriddenDocument) {
		final TreeMap<String, DomainClass> vanillaDocumentClasses = moduleDocumentVanillaClasses.get(overriddenDocument.getOwningModuleName());

		TreeMap<String, AttributeType> documentProperties = vanillaDocumentClasses.get(overriddenDocument.getName()).attributes;
		TreeMap<String, AttributeType> extraProperties = generateDocumentPropertyNames(overriddenDocument);
		Iterator<String> i = extraProperties.keySet().iterator();
		while (i.hasNext()) {
			String extraPropertyName = i.next();
			if (documentProperties.containsKey(extraPropertyName)) {
				i.remove();
			}
		}

		return extraProperties;
	}

	private static void createMappingFileHeader(StringBuilder contents) {
		contents.append("<?xml version=\"1.0\"?>\n");
		contents.append("<!DOCTYPE hibernate-mapping PUBLIC \"-//Hibernate/Hibernate Mapping DTD 3.0//EN\" \"http://www.hibernate.org/dtd/hibernate-mapping-3.0.dtd\">\n");
		contents.append("<hibernate-mapping default-access=\"field\">\n\n");
		contents.append("\t<typedef name=\"OptimisticLock\" class=\"org.skyve.impl.domain.types.OptimisticLockUserType\" />\n");
		contents.append("\t<typedef name=\"Decimal2\" class=\"org.skyve.impl.domain.types.Decimal2UserType\" />\n");
		contents.append("\t<typedef name=\"Decimal5\" class=\"org.skyve.impl.domain.types.Decimal5UserType\" />\n");
		contents.append("\t<typedef name=\"Decimal10\" class=\"org.skyve.impl.domain.types.Decimal10UserType\" />\n");
		contents.append("\t<typedef name=\"DateOnly\" class=\"org.skyve.impl.domain.types.DateOnlyUserType\" />\n");
		contents.append("\t<typedef name=\"DateTime\" class=\"org.skyve.impl.domain.types.DateTimeUserType\" />\n");
		contents.append("\t<typedef name=\"TimeOnly\" class=\"org.skyve.impl.domain.types.TimeOnlyUserType\" />\n");
		contents.append("\t<typedef name=\"Timestamp\" class=\"org.skyve.impl.domain.types.TimestampUserType\" />\n");
		contents.append("\t<typedef name=\"Enum\" class=\"org.skyve.impl.domain.types.EnumUserType\" />\n");
	}

	private static void createMappingFileFooter(StringBuilder contents, StringBuilder filterDefinitions) {
		contents.append(filterDefinitions).append("</hibernate-mapping>");
	}

	/*
	For child indexed collection
	
	<class name="Parent">
	<id name="id" column="parent_id"/>
	....
	<list name="children">
	    <key column="parent_id" not-null="true"/>
	    <list-index column="bizOrdinal"/>
	    <one-to-many class="Child"/>
	</list>
	</class>
	<class name="Child">
	<id name="id" column="child_id"/>
	<property name="ordinal" column="bizOrdinal">
	....
	<many-to-one name="parent" class="Parent" column="parent_id" insert="false" update="false" />
	</class>
	
	For aggregate or composition...
	
	<class name="Parent">
	<id name="id" column="parent_id"/>
	....
	<list name="children">
	    <key column="parent_id" not-null="true"/>
	    <list-index column="bizOrdinal"/>
	    <one-to-many class="Child"/>
	</list>
	</class>
	<class name="Child">
	<id name="id" column="child_id"/>
	....
	<many-to-one name="parent" class="Parent" column="parent_id" insert="false" update="false" />
	</class>
	
	For subclasses
	
	1 table per hierarchy
		<subclass name="modules.sdc.domain.Batch" entity-name="sdcBatch" discriminator-value="A">
		</subclass>
	joined tables
		<joined-subclass name="modules.sdc.domain.Bundle" table="SDC_Bundle" entity-name="sdcBundle">
			<key column="bizId" />
		</joined-subclass>
	*/
	private void generateORM(StringBuilder contents,
								Module module,
								Document document,
								String packagePathPrefix,
								boolean forExt,
								Customer customer,
								StringBuilder filterDefinitions)
	throws Exception {
		generateORM(contents, module, document, packagePathPrefix, forExt, false, customer, filterDefinitions, "");
	}

	private void generateORM(StringBuilder contents,
								Module module,
								Document document,
								String packagePathPrefix,
								boolean forExt, // indicates if we are processing a customer override
								boolean recursive,
								Customer customer,
								StringBuilder filterDefinitions,
								String indentation) { // indents subclass definitions within the class definition
		Extends inherits = document.getExtends();
		if ((! recursive) && // not a recursive call
				(inherits != null)) { // this is a sub-class
			return;
		}

		Persistent persistent = document.getPersistent();
		String customerName = (customer == null) ? null : customer.getName();
		String documentName = document.getName();
		String moduleName = module.getName();
		String baseDocumentName = (inherits == null) ? null : inherits.getDocumentName();
		if (repository.findNearestPersistentUnmappedSuperDocument(null, module, document) == null) {
			baseDocumentName = null;
		}
		String indent = indentation;
		ExtensionStrategy strategy = (persistent == null) ? null : persistent.getStrategy();
		if (recursive && ExtensionStrategy.mapped.equals(strategy)) {
			indent = indentation.substring(0, indentation.length() - 1);
		}

		String entityName = null;

		if (debug) {
			System.out.println(new StringBuilder(256).append("Generate ORM for ")
														.append(packagePathPrefix)
														.append(moduleName)
														.append('.')
														.append(ProvidedRepository.DOMAIN_NAME)
														.append('.')
														.append(documentName).toString());
		}

		// class defn
		if ((persistent != null) && (persistent.getName() != null)) { // persistent document
			// check table name length if required
			if (identifierIsTooLong(persistent.getName())) {
				throw new MetaDataException("Persistent name " + persistent.getName() + 
												" in document " + document.getName() + 
												" in module " + module.getName() +
												" is longer than the allowed data store identifier character limit of " +
												dialectOptions.getDataStoreIdentifierCharacterLimit());
			}
			if (baseDocumentName != null) {
				if (ExtensionStrategy.joined.equals(strategy)) {
					contents.append(indent).append("\t<joined-subclass name=\"");
				}
				else if (ExtensionStrategy.single.equals(strategy)) {
					contents.append(indent).append("\t<subclass name=\"");
				}
			}
			else {
				contents.append(indent).append("\t<class name=\"");
			}
			if ((baseDocumentName == null) || (! ExtensionStrategy.mapped.equals(strategy))) {
				String extensionPath = new StringBuilder(256).append(srcPath)
																.append(packagePathPrefix.replace('.', '/'))
																.append(moduleName)
																.append('/')
																.append(documentName)
																.append('/')
																.append(documentName)
																.append("Extension.java").toString();
				if (new File(extensionPath).exists()) {
					if (debug) System.out.println("    Generate ORM using " + extensionPath);
					contents.append(packagePathPrefix).append(moduleName).append('.').append(documentName).append('.').append(documentName).append("Extension");
				}
				else {
					contents.append(packagePathPrefix).append(moduleName).append('.').append(ProvidedRepository.DOMAIN_NAME).append('.').append(documentName);
					if (forExt) {
						contents.append("Ext");
					}
				}
				contents.append("\" ");
			}
			
			if ((baseDocumentName == null) || ExtensionStrategy.joined.equals(strategy)) {
				contents.append("table=\"").append(persistent.getName());
				String schemaName = persistent.getSchema();
				if (schemaName != null) {
					contents.append("\" schema=\"").append(schemaName);
				}
				String catalogName = persistent.getCatalog();
				if (catalogName != null) {
					contents.append("\" catalog=\"").append(catalogName);
				}
				contents.append("\" ");
			}

			if (ExtensionStrategy.single.equals(strategy)) {
				contents.append("discriminator-value=\"");
				String discriminator = persistent.getDiscriminator();
				if (discriminator == null) {
					contents.append(moduleName).append(documentName);
				}
				else {
					contents.append(discriminator);
				}
				contents.append("\" ");
			}

			if ((baseDocumentName == null) || (! ExtensionStrategy.mapped.equals(strategy))) {
				contents.append("entity-name=\"");
				StringBuilder entityNameBuilder = new StringBuilder(64);
				if (customerName != null) {
					entityNameBuilder.append(customerName);
				}
				entityNameBuilder.append(moduleName).append(documentName);
				entityName = entityNameBuilder.toString();
				contents.append(entityName).append("\">\n");
			}
			if ((baseDocumentName != null) && ExtensionStrategy.joined.equals(strategy)) {
				contents.append(indent).append("\t\t<key column=\"bizId\" />\n");
			}
			else if (baseDocumentName == null) {
				String cacheName = persistent.getCacheName();
				if (cacheName != null) {
					contents.append(indent).append("\t\t<cache usage=\"read-write\" region=\"");
					contents.append(cacheName).append("\" />\n");
				}
				
				// map inherited properties
				contents.append(indent).append("\t\t<id name=\"bizId\" length=\"36\" />\n");

				if (ExtensionStrategy.single.equals(strategy)) {
					contents.append(indent).append("\t\t<discriminator column=\"bizDiscriminator\" type=\"string\" />\n");
				}

				contents.append(indent).append("\t\t<version name=\"bizVersion\" unsaved-value=\"null\" />\n");
				// bizLock length of 271 is 17 for the timestamp (yyyyMMddHHmmssSSS) + 254 (email address max length from RFC 5321)
				contents.append(indent).append("\t\t<property name=\"bizLock\" type=\"OptimisticLock\" length=\"271\" not-null=\"true\" />\n");
				// bizKey must be nullable as the Hibernate NOT NULL constraint check happens before
				// HibernateListener.preInsert() and HibernateListener.preUpdate() are fired - ie before bizKey is populated.
				// HibernateListener checks for null bizKeys manually.
				boolean shouldIndex = shouldIndex(null);
				contents.append(indent).append("\t\t<property name=\"").append(Bean.BIZ_KEY).append("\" length=\"");
				contents.append(dialectOptions.getDataStoreBizKeyLength());
				if (shouldIndex) {
					contents.append("\" index=\"");
					contents.append(generateDataStoreName(DataStoreType.IDX, persistent.getName(), Bean.BIZ_KEY));
				}
				contents.append("\" not-null=\"true\" />\n");

				contents.append(indent).append("\t\t<property name=\"").append(Bean.CUSTOMER_NAME).append("\" length=\"50");
				if (multiTenant && shouldIndex) {
					contents.append("\" index=\"");
					contents.append(generateDataStoreName(DataStoreType.IDX, persistent.getName(), Bean.CUSTOMER_NAME));
				}
				contents.append("\" not-null=\"true\" />\n");

				contents.append(indent).append("\t\t<property name=\"bizFlagComment\" length=\"1024\" />\n");
				contents.append(indent).append("\t\t<property name=\"bizDataGroupId\" length=\"36\" />\n");
				if (shouldIndex) {
					contents.append(indent).append("\t\t<property name=\"").append(Bean.USER_ID).append("\" length=\"36\" index=\"");
					contents.append(generateDataStoreName(DataStoreType.IDX, persistent.getName(), Bean.USER_ID)).append("\" not-null=\"true\" />\n");
				}
				else {
					contents.append(indent).append("\t\t<property name=\"").append(Bean.USER_ID).append("\" length=\"36\" not-null=\"true\" />\n");
				}
			}

			// map the parent property, if parent document is persistent
			String parentDocumentName = document.getParentDocumentName();
			if (parentDocumentName != null) {
				Document parentDocument = document.getParentDocument(null);
				if (parentDocument.getPersistent() != null) {
					if (parentDocumentName.equals(documentName)) { // hierarchical
						contents.append(indent).append("\t\t<property name=\"").append(HierarchicalBean.PARENT_ID).append("\" length=\"36\"");
						if (shouldIndex(((DocumentImpl) document).getParentDatabaseIndex())) {
							contents.append(" index=\"").append(generateDataStoreName(DataStoreType.IDX, persistent.getName(), HierarchicalBean.PARENT_ID)).append('"');
						}
						contents.append(" />\n");
					}
					else {
						// Add bizOrdinal ORM
						if (document.isOrdered()) {
							contents.append(indent).append("\t\t<property name=\"bizOrdinal\" />\n");
						}

						// Add parent reference ORM
						String parentModuleName = module.getDocument(null, parentDocumentName).getOwningModuleName();
						contents.append(indent).append("\t\t<many-to-one name=\"").append(ChildBean.PARENT_NAME).append("\" entity-name=\"");
						// reference overridden document if applicable
						if (overriddenORMDocumentsPerCustomer.contains(parentModuleName + '.' + parentDocumentName)) {
							contents.append(customerName);
						}
						contents.append(parentModuleName).append(parentDocumentName);
						contents.append("\" column=\"parent_id\" insert=\"false\" update=\"false\"");
						if (shouldIndex(((DocumentImpl) document).getParentDatabaseIndex())) {
							contents.append(" index=\"").append(generateDataStoreName(DataStoreType.IDX, persistent.getName(), ChildBean.PARENT_NAME)).append('"');
						}
						contents.append(" foreign-key=\"");
						contents.append(generateDataStoreName(DataStoreType.FK, persistent.getName(), ChildBean.PARENT_NAME));
						contents.append("\" />\n");
					}
				}
			}

			generateAttributeMappings(contents, customer, module, document, persistent, null, new TreeSet<String>(), null, forExt, indent);
		}

		TreeMap<String, Document> derivations = modocDerivations.get(moduleName + '.' + documentName);
		if (derivations != null) {
			// Do subclasses before joined-subclasses
			for (Document derivation : derivations.values()) {
				Persistent derivationPersistent = derivation.getPersistent();
				ExtensionStrategy derivationStrategy = (derivationPersistent == null) ? null : derivationPersistent.getStrategy();
				if (ExtensionStrategy.single.equals(derivationStrategy)) {
					Module derivedModule = repository.getModule(customer, derivation.getOwningModuleName());
					generateORM(contents,
									derivedModule,
									derivation,
									packagePathPrefix,
									forExt,
									true,
									customer,
									filterDefinitions,
									indent + "\t");
				}
			}
			// Do joined-subclasses after subclasses
			for (Document derivation : derivations.values()) {
				Persistent derivationPersistent = derivation.getPersistent();
				ExtensionStrategy derivationStrategy = (derivationPersistent == null) ? null : derivationPersistent.getStrategy();
				if (ExtensionStrategy.joined.equals(derivationStrategy)) {
					Module derivedModule = repository.getModule(customer, derivation.getOwningModuleName());
					generateORM(contents,
									derivedModule,
									derivation,
									packagePathPrefix,
									forExt,
									true,
									customer,
									filterDefinitions,
									indent + "\t");
				}
			}
			// Take care of subclasses with no derivation strategy (persistent subclass with only mapped superclasses)
			for (Document derivation : derivations.values()) {
				Persistent derivationPersistent = derivation.getPersistent();
				ExtensionStrategy derivationStrategy = (derivationPersistent == null) ? null : derivationPersistent.getStrategy();
				if ((derivationStrategy == null) || ExtensionStrategy.mapped.equals(derivationStrategy)) {
					Module derivedModule = repository.getModule(customer, derivation.getOwningModuleName());
					generateORM(contents,
									derivedModule,
									derivation,
									packagePathPrefix,
									forExt,
									true,
									customer,
									filterDefinitions,
									indent + "\t");
				}
			}
		}

		if ((persistent != null) && (persistent.getName() != null)) { // persistent document
			if (baseDocumentName != null) {
				if (ExtensionStrategy.joined.equals(strategy)) {
					contents.append(indent).append("\t</joined-subclass>\n");
				}
				else if (ExtensionStrategy.single.equals(strategy)) {
					contents.append(indent).append("\t</subclass>\n");
				}
			}
			else {
				generateFilterStuff(entityName, contents, filterDefinitions, indent);
				contents.append(indent).append("\t</class>\n\n");
			}
		}
	}

	private void generateAttributeMappings(StringBuilder contents,
											Customer customer,
											Module module,
											Document document,
											Persistent persistent,
											String columnPrefix,
											Set<String> columnNames,
											String owningDocumentName,
											boolean forExt,
											String indentation) {
		Extends inherits = document.getExtends();
		if (inherits != null) {
			Document baseDocument = module.getDocument(customer, inherits.getDocumentName());
			Persistent basePersistent = baseDocument.getPersistent();
			if ((basePersistent != null) && ExtensionStrategy.mapped.equals(basePersistent.getStrategy())) {
				Module baseModule = repository.getModule(customer, baseDocument.getOwningModuleName());
				generateAttributeMappings(contents,
											customer,
											baseModule,
											baseDocument,
											persistent,
											columnPrefix,
											columnNames,
											owningDocumentName,
											forExt,
											indentation);
			}
		}

		String customerName = (customer == null) ? null : customer.getName();
		String moduleName = module.getName();
		String documentName = document.getName();

		// map the document defined properties
		for (Attribute attribute : document.getAttributes()) {
			if (attribute instanceof Collection) {
				Collection collection = (Collection) attribute;

				String collectionName = collection.getName();
				String referencedDocumentName = collection.getDocumentName();
				Document referencedDocument = module.getDocument(customer, referencedDocumentName);
				Persistent referencedPersistent = referencedDocument.getPersistent();
				String referencedModuleName = referencedDocument.getOwningModuleName();

				// check that persistent collections don't reference transient documents.
				if (collection.isPersistent()) {
					if (referencedPersistent == null) {
						throw new MetaDataException(String.format("The Collection %s in document %s.%s is persistent but the target [documentName] of %s is a transient document.", 
																	collectionName, moduleName, documentName, referencedDocumentName));
					}
					// ignore collections of dynamic documents
					if (referencedDocument.isDynamic()) {
						continue;
					}
				}
				// ignore transient attributes
				else {
					continue;
				}

				StringBuilder orderBy = null;
				// Add order by clause to hibernate ORM only if the bindings are simple and the ordering clause has columns
				if ((! ((CollectionImpl) collection).isComplexOrdering()) &&
						(! collection.getOrdering().isEmpty())) {
					orderBy = new StringBuilder(64);
					for (Ordering ordering : collection.getOrdering()) {
						String byBinding = ordering.getBy();
						String columnName = byBinding;
						// Determine the database column name - if an association the add '_id' to make the FK column name
						Attribute byAttribute = referencedDocument.getAttribute(byBinding);
						if (byAttribute instanceof Association) {
							columnName += "_id";
						}
						orderBy.append(columnName);
						if (SortDirection.descending.equals(ordering.getSort())) {
							orderBy.append(" desc, ");
						}
						else {
							orderBy.append(" asc, ");
						}
					}
					orderBy.setLength(orderBy.length() - 2);
				}

				CollectionType type = collection.getType();
				boolean mapped = ExtensionStrategy.mapped.equals(referencedPersistent.getStrategy());

				if (type == CollectionType.child) {
					if (mapped) {
						throw new MetaDataException("Collection " + collectionName + 
														" in document " + moduleName + '.' + documentName + " referencing document " +
														referencedDocument.getOwningModuleName() + '.' + referencedDocumentName +
														" cannot be a child collection as the target document is a mapped document." +
														" Use a composed collection instead.");
					}
					contents.append(indentation).append("\t\t<bag name=\"").append(collectionName);
					if (Boolean.TRUE.equals(collection.getOrdered())) {
						contents.append("\" order-by=\"").append(Bean.ORDINAL_NAME);
					} 
					else if (orderBy != null) {
						contents.append("\" order-by=\"").append(orderBy);
					}
					String cacheName = collection.getCacheName();
					if (cacheName != null) {
						throw new MetaDataException("Collection " + collectionName + 
														" in document " + moduleName + '.' + documentName + " referencing document " +
														referencedDocument.getOwningModuleName() + '.' + referencedDocumentName +
														" cannot be cached as it is a child collection. Use a composed collection instead or remove the caching.");
					}
					contents.append("\" cascade=\"all-delete-orphan\">\n");
					contents.append(indentation).append("\t\t\t<key column=\"parent_id\" />\n");

					contents.append(indentation).append("\t\t\t<one-to-many entity-name=\"");
					// reference overridden document if applicable
					if (overriddenORMDocumentsPerCustomer.contains(referencedModuleName + '.' + referencedDocumentName)) {
						contents.append(customerName);
					}
					contents.append(referencedModuleName).append(referencedDocumentName).append("\" />\n");
					contents.append(indentation).append("\t\t</bag>\n");
				} 
				else {
					if (Boolean.TRUE.equals(collection.getOrdered())) {
						contents.append(indentation).append("\t\t<list name=\"").append(collectionName);
					} 
					else {
						contents.append(indentation).append("\t\t<bag name=\"").append(collectionName);
					}

					String catalog = persistent.getCatalog();
					if (catalog != null) {
						contents.append("\" catalog=\"").append(catalog);
					}
					String schema = persistent.getSchema();
					if (schema != null) {
						contents.append("\" schema=\"").append(schema);
					}
					String collectionTableName = new StringBuilder(256).append(persistent.getName())
																		.append('_')
																		.append(collectionName).toString();

					// check collection table name length if required
					if (identifierIsTooLong(collectionTableName)) {
						throw new MetaDataException("Collection table name of " + collectionTableName +
														" for collection " + collectionName + 
														" in document " + documentName + 
														" in module " + moduleName +
														" is longer than the allowed data store identifier character limit of " +
														dialectOptions.getDataStoreIdentifierCharacterLimit());
					}
					
					contents.append("\" table=\"").append(collectionTableName);

					if (CollectionType.aggregation.equals(type)) {
						contents.append("\" cascade=\"persist,save-update,refresh,merge\">\n");
					}
					else if (CollectionType.composition.equals(type)) {
						contents.append("\" cascade=\"all-delete-orphan\">\n");
					}
					else {
						throw new IllegalStateException("Collection type " + type + " not supported.");
					}
					String cacheName = collection.getCacheName();
					if (cacheName != null) {
						contents.append(indentation).append("\t\t\t<cache usage=\"read-write\" region=\"");
						contents.append(cacheName).append("\" />\n");
					}

					if (shouldIndex(collection.getOwnerDatabaseIndex())) {
						contents.append(indentation).append("\t\t\t<key foreign-key=\"");
						contents.append(generateDataStoreName(DataStoreType.FK, collectionTableName, PersistentBean.OWNER_COLUMN_NAME));
						contents.append("\">\n");
						contents.append("\t\t\t\t<column name=\"").append(PersistentBean.OWNER_COLUMN_NAME);
						contents.append("\" index=\"");
						contents.append(generateDataStoreName(DataStoreType.IDX, 
																collectionTableName, 
																PersistentBean.OWNER_COLUMN_NAME));
						contents.append("\" />\n");
						contents.append(indentation).append("\t\t\t</key>\n");
					} 
					else {
						contents.append(indentation).append("\t\t\t<key column=\"").append(PersistentBean.OWNER_COLUMN_NAME);
						contents.append("\" foreign-key=\"");
						contents.append(generateDataStoreName(DataStoreType.FK, collectionTableName, PersistentBean.OWNER_COLUMN_NAME));
						contents.append("\" />\n");
					}
					if (Boolean.TRUE.equals(collection.getOrdered())) {
						contents.append(indentation).append("\t\t\t<list-index column=\"").append(Bean.ORDINAL_NAME).append("\"/>\n");
					}

					if (mapped) {
						contents.append(indentation).append("\t\t<many-to-any meta-type=\"string\" id-type=\"string\">\n");
						Map<String, Document> arcs = new TreeMap<>();
						populateArcs(referencedDocument, arcs);
						for (Entry<String, Document> entry : arcs.entrySet()) {
							Document derivedDocument = entry.getValue();
							String derivedModuleName = derivedDocument.getOwningModuleName();
							String derivedDocumentName = derivedDocument.getName();

							contents.append(indentation).append("\t\t\t<meta-value value=\"").append(entry.getKey());
							contents.append("\" class=\"");
							// reference overridden derived document if applicable
							if (overriddenORMDocumentsPerCustomer.contains(derivedModuleName + '.' + derivedDocumentName)) {
								contents.append(customerName);
							}
							contents.append(derivedModuleName).append(derivedDocumentName).append("\" />\n");
						}

						// check type column name length if required
						if (identifierIsTooLong(collectionName + "_type")) {
							throw new MetaDataException("Collection name " + collectionName + 
															" in document " + documentName + 
															" in module " + moduleName +
															" is longer than the allowed data store identifier character limit of " +
															dialectOptions.getDataStoreIdentifierCharacterLimit() + "(" + collectionName + "_type)");
						}

						// Even though it would be better index wise to put the bizId column first
						// This doesn't work - hibernate returns nulls for the association getter call.
						// So sub-optimal but working if type column is first.
						// Notice that an index is applied unless explicitly false as this type of reference is not constrained by a FK.
						contents.append(indentation).append("\t\t\t<column name=\"");
						contents.append(collectionName).append("_type\"");
						if (! Boolean.FALSE.equals(collection.getElementDatabaseIndex())) {
							contents.append(" index=\"");
							contents.append(generateDataStoreName(DataStoreType.IDX, collectionTableName, PersistentBean.ELEMENT_COLUMN_NAME));
							contents.append('"');
						}
						contents.append(" />\n");
						contents.append(indentation).append("\t\t\t<column name=\"");
						contents.append(collectionName).append("_id\" length=\"36\"");
						if (! Boolean.FALSE.equals(collection.getElementDatabaseIndex())) {
							contents.append(" index=\"");
							contents.append(generateDataStoreName(DataStoreType.IDX, collectionTableName, PersistentBean.ELEMENT_COLUMN_NAME));
							contents.append('"');
						}
						contents.append(" />\n");
						contents.append(indentation).append("\t\t</many-to-any>\n");
					} 
					else {
						contents.append(indentation).append("\t\t\t<many-to-many entity-name=\"");
						// reference overridden document if applicable
						if (overriddenORMDocumentsPerCustomer.contains(referencedModuleName + '.' + referencedDocumentName)) {
							contents.append(customerName);
						}
						contents.append(referencedModuleName).append(referencedDocumentName);
						if (orderBy != null) {
							contents.append("\" order-by=\"").append(orderBy);
						}
						contents.append("\" foreign-key=\"");
						contents.append(generateDataStoreName(DataStoreType.FK, collectionTableName, PersistentBean.ELEMENT_COLUMN_NAME));
						if (shouldIndex(collection.getElementDatabaseIndex())) {
							contents.append("\">\n");
							contents.append("\t\t\t\t<column name=\"").append(PersistentBean.ELEMENT_COLUMN_NAME);
							contents.append("\" index=\"");
							contents.append(generateDataStoreName(DataStoreType.IDX, collectionTableName, PersistentBean.ELEMENT_COLUMN_NAME));
							contents.append("\" />\n");
							contents.append("\t\t\t</many-to-many>\n");
						} 
						else {
							contents.append("\" column=\"").append(PersistentBean.ELEMENT_COLUMN_NAME);
							contents.append("\" />\n");
						}
					}

					if (Boolean.TRUE.equals(collection.getOrdered())) {
						contents.append(indentation).append("\t\t</list>\n");
					}
					else {
						contents.append(indentation).append("\t\t</bag>\n");
					}
				}
			}
			else if (attribute instanceof Association) {
				Association association = (Association) attribute;

				String associationName = association.getName();
				String referencedDocumentName = association.getDocumentName();
				Document referencedDocument = module.getDocument(null, referencedDocumentName);
				Persistent referencedPersistent = referencedDocument.getPersistent();
				String referencedModuleName = referencedDocument.getOwningModuleName();

				// check that persistent collections don't reference transient documents.
				if (association.isPersistent()) {
					if (referencedPersistent == null) {
						throw new MetaDataException(String.format("The Association %s in document %s.%s is persistent but the target [documentName] of %s is a transient document.", 
																	associationName, moduleName, documentName, referencedDocumentName));
					}

					// ignore an association to a dynamic document
					if (referencedDocument.isDynamic()) {
						continue;
					}
				}
				// ignore transient attributes
				else {
					continue;
				}
				
				AssociationType type = association.getType();
				if (AssociationType.embedded.equals(type)) {
					if (referencedModuleName.equals(moduleName) && (referencedDocumentName.equals(documentName))) {
						throw new MetaDataException(String.format("The Association %s in document %s.%s cannot be of type 'embedded' when it references itself.", 
																	associationName, 
																	moduleName, 
																	documentName));
						
					}
					
					contents.append(indentation).append("\t\t<component name=\"").append(associationName);
					// TODO need to add class here for customer overrides
					// <component class="" /> is required for customer overriding otherwise defaults to return type via reflection
					contents.append("\">\n");

					// Add parent link in
					if (documentName.equals(referencedDocument.getParentDocumentName())) {
						contents.append(indentation).append("\t\t\t<parent name=\"parent\" />\n");
					}
					
					Module referencedModule = repository.getModule(customer, referencedModuleName);
					
					// use the enclosing document's persistent object
					generateAttributeMappings(contents,
												customer,
												referencedModule,
												referencedDocument,
												persistent,
												association.getEmbeddedColumnsPrefix(),
												columnNames,
												documentName,
												forExt,
												indentation + "\t");

					contents.append(indentation).append("\t\t</component>\n");
				}
				else if (ExtensionStrategy.mapped.equals(referencedPersistent.getStrategy())) {
					contents.append(indentation).append("\t\t<any name=\"").append(associationName);
					contents.append("\" meta-type=\"string\" id-type=\"string\">\n");
					Map<String, Document> arcs = new TreeMap<>();
					populateArcs(referencedDocument, arcs);
					for (Entry<String, Document> entry : arcs.entrySet()) {
						Document derivedDocument = entry.getValue();
						String derivedModuleName = derivedDocument.getOwningModuleName();
						String derivedDocumentName = derivedDocument.getName();

						contents.append(indentation).append("\t\t\t<meta-value value=\"").append(entry.getKey());
						contents.append("\" class=\"");
						// reference overridden document if applicable
						if (overriddenORMDocumentsPerCustomer.contains(derivedModuleName + '.' + derivedDocumentName)) {
							contents.append(customerName);
						}
						contents.append(derivedModuleName).append(derivedDocumentName).append("\" />\n");
					}

					// check type column name length if required
					String columnName = columnName(moduleName, owningDocumentName, associationName, columnPrefix, "_type", columnNames);
					if (identifierIsTooLong(columnName)) {
						throw new MetaDataException("Association name " + associationName + 
														" in document " + documentName + 
														" in module " + moduleName +
														" is longer than the allowed data store identifier character limit of " +
														dialectOptions.getDataStoreIdentifierCharacterLimit() + " (" + columnName + ")");
					}

					// Even though it would be better index wise to put the bizId column first
					// This doesn't work - hibernate returns nulls for the association getter call.
					// So sub-optimal but working if type column is first.
					// Notice that an index is applied unless explicitly false as this type of reference is not constrained by a FK.
					contents.append(indentation).append("\t\t\t<column name=\"");
					contents.append(columnName);
					if (! Boolean.FALSE.equals(association.getDatabaseIndex())) {
						contents.append("\" index=\"");
						contents.append(generateDataStoreName(DataStoreType.IDX, persistent.getName(), associationName));
					}
					contents.append("\" />\n");
					contents.append(indentation).append("\t\t\t<column name=\"");
					contents.append(columnName(moduleName, owningDocumentName, associationName, columnPrefix, "_id", columnNames)).append("\" length=\"36");
					if (! Boolean.FALSE.equals(association.getDatabaseIndex())) {
						contents.append("\" index=\"");
						contents.append(generateDataStoreName(DataStoreType.IDX, persistent.getName(), associationName));
					}
					contents.append("\" />\n");
					contents.append(indentation).append("\t\t</any>\n");
				}
				else {
					// check id column name length if required
					String associationColumnName = columnName(moduleName, owningDocumentName, associationName, columnPrefix, "_id", columnNames);
					if (identifierIsTooLong(associationColumnName)) {
						throw new MetaDataException("Association name " + associationColumnName + 
														" in document " + documentName + 
														" in module " + moduleName +
														" is longer than the allowed data store identifier character limit of " +
														dialectOptions.getDataStoreIdentifierCharacterLimit() + "(" + associationColumnName + ")");
					}

					contents.append(indentation).append("\t\t<many-to-one name=\"");
					contents.append(associationName).append("\" entity-name=\"");
					// reference overridden document if applicable
					if (overriddenORMDocumentsPerCustomer.contains(referencedModuleName + '.' + referencedDocumentName)) {
						contents.append(customerName);
					}
					contents.append(referencedModuleName).append(referencedDocumentName);
					contents.append("\" column=\"").append(associationColumnName);
					if (AssociationType.composition.equals(type)) {
						contents.append("\" unique=\"true\" cascade=\"persist,save-update,refresh,delete-orphan,merge");
						contents.append("\" unique-key=\"");
						contents.append(generateDataStoreName(DataStoreType.UK, persistent.getName(), associationName));
					}
					else if (AssociationType.aggregation.equals(type)) {
						contents.append("\" cascade=\"persist,save-update,refresh,merge");
					}
					else {
						throw new IllegalStateException("Association type " + type + " not supported.");
					}
					if (shouldIndex(association.getDatabaseIndex())) {
						contents.append("\" index=\"");
						contents.append(generateDataStoreName(DataStoreType.IDX, persistent.getName(), associationName));
					}
					contents.append("\" foreign-key=\"");
					contents.append(generateDataStoreName(DataStoreType.FK, persistent.getName(), associationName));
					contents.append("\"/>\n");
				}
			}
			else if (attribute instanceof Enumeration) {
				// ignore transient attributes
				if (! attribute.isPersistent()) {
					continue;
				}

				Enumeration enumeration = (Enumeration) attribute;

				// ignore dynamic attributes
				if (enumeration.isDynamic()) {
					continue;
				}
				String enumerationName = enumeration.getName();

				// check column name length if required
				if (identifierIsTooLong(enumerationName)) {
					throw new MetaDataException("Enumeration name " + enumerationName + 
													" in document " + documentName + 
													" in module " + moduleName +
													" is longer than the allowed data store identifier character limit of " +
													dialectOptions.getDataStoreIdentifierCharacterLimit());
				}

				contents.append(indentation).append("\t\t<property name=\"").append(enumerationName);

				Integer fieldLength = persistentPropertyLengths.get(persistent.getPersistentIdentifier()).get(enumerationName);
				if (fieldLength != null) {
					contents.append("\" length=\"").append(fieldLength.toString());
				}
				
				String enumerationColumnName = columnName(moduleName, owningDocumentName, enumerationName, columnPrefix, null, columnNames);
				if (! enumerationColumnName.equals(enumerationName)) {
					contents.append("\" column=\"").append(enumerationColumnName);
				}
				
				IndexType index = enumeration.getIndex();
				if (IndexType.database.equals(index) || IndexType.both.equals(index)) {
					contents.append("\" index=\"");
					contents.append(generateDataStoreName(DataStoreType.IDX, persistent.getName(), enumerationName));
				}
				contents.append("\">\n");

				contents.append(indentation).append("\t\t\t<type name=\"Enum\">\n");
				contents.append(indentation).append("\t\t\t\t<param name=\"enumClass\">");

				// for extension and the enumeration attribute doesn't exist in the vanilla document.
				// NB - There a 2 scenarios here...
				// 1) The enumeration is defined here - forExt is true and there is no enum in the vanilla document
				// 2) The enumeration is a reference to a definition elsewhere - in this case, forExt is true
				// and the enumeration is not defined in the enumeration target vanilla document
				Enumeration target = enumeration.getTarget();
				String implementingEnumClassName = target.getImplementingEnumClassName();
				if (implementingEnumClassName != null) {
					contents.append(implementingEnumClassName);
				}
				else {
					Document targetDocument = target.getOwningDocument();
					boolean requiresExtension = forExt &&
													(! moduleDocumentVanillaClasses.get(targetDocument.getOwningModuleName()).get(targetDocument.getName()).attributes.containsKey(target.getName()));
					if (requiresExtension) {
						contents.append(ProvidedRepository.CUSTOMERS_NAME).append('.').append(customerName).append('.');
					}
					contents.append(enumeration.getEncapsulatingClassName());
					if (requiresExtension) {
						contents.append("Ext");
					}
					contents.append('$').append(enumeration.toJavaIdentifier());
				}
				contents.append("</param>\n");
				contents.append(indentation).append("\t\t\t</type>\n");
				contents.append(indentation).append("\t\t</property>\n");
			}
			else if (attribute instanceof Inverse) {
				// ignore transient attributes
				if (! attribute.isPersistent()) {
					continue;
				}

				AbstractInverse inverse = (AbstractInverse) attribute;
				
				// determine the inverse target metadata
				String inverseDocumentName = inverse.getDocumentName();
				Document inverseDocument = module.getDocument(null, inverseDocumentName);
				Persistent inversePersistent = inverseDocument.getPersistent();
				String inverseReferenceName = inverse.getReferenceName();
				String inverseModuleName = inverseDocument.getOwningModuleName();
				InverseRelationship inverseRelationship = inverse.getRelationship();
				Boolean cascade = inverse.getCascade();

				// ignore an inverse to a dynamic document
				if (inverseDocument.isDynamic()) {
					continue;
				}
				
				if (InverseRelationship.oneToOne.equals(inverseRelationship)) {
					contents.append("\t\t<one-to-one name=\"").append(inverse.getName());

					contents.append("\" entity-name=\"");
					// reference overridden document if applicable
					if (overriddenORMDocumentsPerCustomer.contains(inverseModuleName + '.' + inverseDocumentName)) {
						contents.append(customerName);
					}
					contents.append(inverseModuleName).append(inverseDocumentName);

					contents.append("\" property-ref=\"").append(inverseReferenceName);

					if (Boolean.TRUE.equals(cascade)) {
						contents.append("\" cascade=\"persist,save-update,refresh,merge");
					}

					contents.append("\" />\n");
				}
				else {
					contents.append(indentation).append("\t\t<bag name=\"").append(inverse.getName());
					if (InverseRelationship.manyToMany.equals(inverseRelationship)) {
						String catalog = inversePersistent.getCatalog();
						if (catalog != null) {
							contents.append("\" catalog=\"").append(catalog);
						}
						String schema = inversePersistent.getSchema();
						if (schema != null) {
							contents.append("\" schema=\"").append(schema);
						}
						contents.append("\" table=\"").append(inversePersistent.getName()).append('_').append(inverseReferenceName);
					}

					if (Boolean.TRUE.equals(cascade)) {
						contents.append("\" cascade=\"persist,save-update,refresh,merge");
					}

					contents.append("\" inverse=\"true\">\n");

					contents.append(indentation).append("\t\t\t<key column=\"");
					if (InverseRelationship.manyToMany.equals(inverseRelationship)) {
						contents.append("element");
					}
					else {
						contents.append(inverseReferenceName);
					}
					contents.append("_id\" />\n");
					if (InverseRelationship.manyToMany.equals(inverseRelationship)) {
						contents.append(indentation).append("\t\t\t<many-to-many entity-name=\"");
					}
					else {
						contents.append(indentation).append("\t\t\t<one-to-many entity-name=\"");
					}

					// reference overridden document if applicable
					if (overriddenORMDocumentsPerCustomer.contains(inverseModuleName + '.' + inverseDocumentName)) {
						contents.append(customerName);
					}
					contents.append(inverseModuleName).append(inverseDocumentName);

					if (InverseRelationship.manyToMany.equals(inverseRelationship)) {
						contents.append("\" column=\"").append(PersistentBean.OWNER_COLUMN_NAME);
					}
					contents.append("\" />\n");
					contents.append(indentation).append("\t\t</bag>\n");
				}
			}
			else {
				// ignore transient attributes
				if (! attribute.isPersistent()) {
					continue;
				}

				Field field = (Field) attribute;

				// ignore dynamic attributes
				if (field.isDynamic()) {
					continue;
				}
				
				String fieldName = field.getName();
				String fieldColumnName = columnName(moduleName, owningDocumentName, fieldName, columnPrefix, null, columnNames);

				// check column name length if required
				if (identifierIsTooLong(fieldColumnName)) {
					throw new MetaDataException("Field name " + fieldName + 
													" in document " + document.getName() + 
													" in module " + module.getName() +
													" has a column name " + fieldColumnName + 
													" which is longer than the allowed data store identifier character limit of " +
													dialectOptions.getDataStoreIdentifierCharacterLimit());
				}

				contents.append(indentation).append("\t\t<property name=\"").append(fieldName);

				Integer fieldLength = persistentPropertyLengths.get(persistent.getPersistentIdentifier()).get(fieldName);
				if (fieldLength != null) {
					contents.append("\" length=\"").append(fieldLength.toString());
				}

				AttributeType type = attribute.getAttributeType();
				if (type == AttributeType.decimal2) {
					contents.append("\" type=\"").append(DECIMAL2).append("\" precision=\"20\" scale=\"2");
				}
				else if (type == AttributeType.decimal5) {
					contents.append("\" type=\"").append(DECIMAL5).append("\" precision=\"23\" scale=\"5");
				}
				else if (type == AttributeType.decimal10) {
					contents.append("\" type=\"").append(DECIMAL10).append("\" precision=\"28\" scale=\"10");
				}
				else if (type == AttributeType.date) {
					contents.append("\" type=\"").append(DATE_ONLY);
				}
				else if (type == AttributeType.dateTime) {
					contents.append("\" type=\"").append(DATE_TIME);
				}
				else if (type == AttributeType.time) {
					contents.append("\" type=\"").append(TIME_ONLY);
				}
				else if (type == AttributeType.timestamp) {
					contents.append("\" type=\"").append(TIMESTAMP);
				}
				else if (type == AttributeType.id) {
					contents.append("\" length=\"36");
				}
				else if ((type == AttributeType.content) || (type == AttributeType.image)) {
					contents.append("\" length=\"36");
				}
				
				/* Wouldn't update or insert rows in a mysql database in latin1 or utf8.
				 * I don't think the JDBC recognizes CLOBs correctly as it
				 * would insert through MySQL query browser.
				 * This could be reproduced on linux on dev laptop and test environment on prod server.
				 * Non-descript Error was
				 * 15:54:10,829 WARN  [JDBCExceptionReporter] SQL Error: 1210, SQLState: HY000
				 * 15:54:10,829 ERROR [JDBCExceptionReporter] Incorrect arguments to mysqld_stmt_execute
				 * The net tells me that the problem is MySQL not linking surrogate UTF-16 chars in its fields
				 * The following code is required if we hit the HY000 problem again
				 * 
				 * StringBuilder sb = new StringBuilder();
				 * for (int i = 0; i < text.length(); i++) {
				 *   char ch = text.charAt(i);
				 *   if (!Character.isHighSurrogate(ch) && !Character.isLowSurrogate(ch)) {
				 *     sb.append(ch);
				 *   }
				 * }
				 * return sb.toString();
				 */
				else if ((type == AttributeType.memo) || (type == AttributeType.markup)) {
					contents.append("\" type=\"text");
				}

				if (! fieldColumnName.equals(fieldName)) {
					contents.append("\" column=\"").append(fieldColumnName);
				}
				
				IndexType index = field.getIndex();
				if (IndexType.database.equals(index) || IndexType.both.equals(index)) {
					contents.append("\" index=\"");
					contents.append(generateDataStoreName(DataStoreType.IDX, persistent.getName(), fieldName));
				}
				contents.append("\" />\n");
			}
		}
	}

	// generate the appropriate column name and check it is unique
	private static String columnName(String moduleName,
										String owningDocumentName,
										String attributeName,
										String columnPrefixWithoutUnderscore,
										String columnSuffixStartingWithUnderscore,
										Set<String> columnNames) {
		String result;
		if (columnPrefixWithoutUnderscore == null) {
			result = attributeName;
		}
		else {
			result = columnPrefixWithoutUnderscore + '_' + attributeName;
		}
		if (columnSuffixStartingWithUnderscore != null) {
			result += columnSuffixStartingWithUnderscore;
		}
		if (! columnNames.add(result)) {
			throw new MetaDataException("Column name " + result + 
											" is duplicated by an embedded association or mapped extension in document " + 
											owningDocumentName + " in module " + moduleName + 
											". Use the embeddedColumnsPrefix attribute on the embedded association to add a namespace/prefix.");
		}
		return result;
	}
	
	// check identifier length if required
	private boolean identifierIsTooLong(String identifier) {
		return ((dialectOptions.getDataStoreIdentifierCharacterLimit() > 0) &&
				(identifier.length() > dialectOptions.getDataStoreIdentifierCharacterLimit()));
	}
	
	private void populateArcs(Document document, Map<String, Document> result) {
		String key = new StringBuilder(32).append(document.getOwningModuleName()).append('.').append(document.getName()).toString();
		TreeMap<String, Document> derivations = modocDerivations.get(key);
		if (derivations != null) {
			for (Document derivation : derivations.values()) {
				Persistent derivationPersistent = derivation.getPersistent();
				if ((derivationPersistent != null) && (derivationPersistent.getName() != null)) {
					key = new StringBuilder(32).append(derivation.getOwningModuleName()).append('.').append(derivation.getName()).toString();
					result.put(key, derivation);
				}
				populateArcs(derivation, result);
			}
		}
	}

	private static void generateFilterStuff(String entityName,
												StringBuilder contents,
												StringBuilder filterDefinitions,
												String indentation) {
		contents.append(indentation).append("\t\t<filter name=\"").append(entityName).append("NoneFilter\" condition=\"1=0\"/>\n");
		contents.append(indentation).append("\t\t<filter name=\"").append(entityName).append("CustomerFilter\" condition=\"bizCustomer=:customerParam\"/>\n");
		contents.append(indentation).append("\t\t<filter name=\"").append(entityName).append("DataGroupIdFilter\" condition=\"bizDataGroupId=:dataGroupIdParam\"/>\n");
		contents.append(indentation).append("\t\t<filter name=\"").append(entityName).append("UserIdFilter\" condition=\"bizUserId=:userIdParam\"/>\n");

		filterDefinitions.append("\t<filter-def name=\"").append(entityName).append("NoneFilter\" />\n");

		filterDefinitions.append("\t<filter-def name=\"").append(entityName).append("CustomerFilter\">\n");
		filterDefinitions.append("\t\t<filter-param name=\"customerParam\" type=\"string\"/>\n");
		filterDefinitions.append("\t</filter-def>\n");

		filterDefinitions.append("\t<filter-def name=\"").append(entityName).append("DataGroupIdFilter\">\n");
		filterDefinitions.append("\t\t<filter-param name=\"dataGroupIdParam\" type=\"string\"/>\n");
		filterDefinitions.append("\t</filter-def>\n");

		filterDefinitions.append("\t<filter-def name=\"").append(entityName).append("UserIdFilter\">\n");
		filterDefinitions.append("\t\t<filter-param name=\"userIdParam\" type=\"string\"/>\n");
		filterDefinitions.append("\t</filter-def>\n");
	}

	private void generateOverriddenORM(StringBuilder contents,
										Customer customer,
										Module module,
										Document document,
										StringBuilder filterDefinitions)
										throws Exception {
		CustomerImpl internalCustomer = (CustomerImpl) customer;
		String moduleName = module.getName();
		String documentName = document.getName();
		String moduleDotDocument = new StringBuilder(128).append(moduleName).append('.').append(documentName).toString();
		String key = new StringBuilder(64).append(ProvidedRepository.MODULES_NAMESPACE).append(moduleName).append('/').append(documentName).toString();
		String packagePathPrefix = ProvidedRepository.MODULES_NAMESPACE;
		// if customer defined document
		String newKey = repository.vtable(customer.getName(), key);
		if ((newKey != null) && newKey.startsWith(ProvidedRepository.CUSTOMERS_NAMESPACE)) {
			// this is either an override or a totally new document.
			// for an override, baseDocumentName != null
			// for a new document definition, baseDocumentName == null
			TreeMap<String, DomainClass> documentClasses = moduleDocumentVanillaClasses.get(moduleName);
			String vanillaDocumentName = documentClasses.containsKey(documentName) ? documentName : null;
			if (vanillaDocumentName != null) { // overridden document
				// bug out if this document is an override but doesn't add any more properties
				TreeMap<String, AttributeType> extraProperties = getOverriddenDocumentExtraProperties(document);
				if (! extraProperties.isEmpty()) { // there exists extra properties in override
					packagePathPrefix = new StringBuilder(128).append(ProvidedRepository.CUSTOMERS_NAMESPACE)
																.append(customer.getName())
																.append('/')
																.append(packagePathPrefix).toString();
				}
			}
			else { // totally new document defined as a customer override
				packagePathPrefix = new StringBuilder(128).append(ProvidedRepository.CUSTOMERS_NAMESPACE)
															.append(customer.getName())
															.append('/')
															.append(packagePathPrefix).toString();
			}
		}
		packagePathPrefix = packagePathPrefix.replaceAll("\\\\|\\/", ".");

		if (! visitedOverriddenORMDocumentsPerCustomer.contains(moduleDotDocument)) {
			visitedOverriddenORMDocumentsPerCustomer.add(moduleDotDocument);
			overriddenORMDocumentsPerCustomer.add(moduleDotDocument);

			// Propagate to parent document (if applicable)
			Document parentDocument = document.getParentDocument(customer);
			if (parentDocument != null) {
				if (! parentDocument.getName().equals(documentName)) { // exclude hierarchical
					String parentModuleName = parentDocument.getOwningModuleName();
					//String parentDocumentName = parentDocument.getName();
					//String parentModuleDotDocument = parentModuleName + '.' + parentDocumentName;
					//System.out.println("\t" + parentModuleDotDocument);
					Module parentModule = customer.getModule(parentModuleName);
					generateOverriddenORM(contents, customer, parentModule, parentDocument, filterDefinitions);
				}
			}

			// Propagate to exported references
			List<ExportedReference> refs = internalCustomer.getExportedReferences(document);
			//System.out.println(documentName + " has refs");
			if (refs != null) {
				for (ExportedReference ref : refs) {
					String refModuleName = ref.getModuleName();
					String refDocumentName = ref.getDocumentName();
					//String refModuleDotDocument = refModuleName + '.' + refDocumentName;
					//System.out.println("\t" + ref.getReferenceFieldName() + " = " + refModuleDotDocument);
					Module refModule = customer.getModule(refModuleName);
					Document refDocument = refModule.getDocument(customer, refDocumentName);
					Persistent refPersistent = refDocument.getPersistent();
					if (refPersistent != null) {
						ExtensionStrategy refStrategy = refPersistent.getStrategy();
						if (ExtensionStrategy.mapped.equals(refStrategy)) {
							Map<String, Document> arcs = new TreeMap<>();
							populateArcs(refDocument, arcs);
							for (Document derivedDocument : arcs.values()) {
								Module derivedModule = customer.getModule(derivedDocument.getOwningModuleName());
								generateOverriddenORM(contents, customer, derivedModule, derivedDocument, filterDefinitions);
							}
						}
						else {
							generateOverriddenORM(contents, customer, refModule, refDocument, filterDefinitions);
						}
					}
					else {
						throw new IllegalStateException("Cannot generate ORM for a transient document");
					}
				}
			}

			// Propagate to child collections
			// Note:- these are not in the list of exported references
			for (String referenceName : document.getReferenceNames()) {
				Reference reference = document.getReferenceByName(referenceName);
				if (CollectionType.child.equals(reference.getType())) {
					Document refDocument = module.getDocument(customer, reference.getDocumentName());
					Module refModule = customer.getModule(refDocument.getOwningModuleName());
					generateOverriddenORM(contents, customer, refModule, refDocument, filterDefinitions);
				}
			}

			TreeMap<String, DomainClass> domainClasses = moduleDocumentVanillaClasses.get(moduleName);
			DomainClass domainClass = (domainClasses == null) ? null : domainClasses.get(documentName);
			generateORM(contents,
							module,
							document,
							packagePathPrefix,
							// extension class in use if this is a customer override of an existing domain class
							(domainClass != null) && (packagePathPrefix.startsWith(ProvidedRepository.CUSTOMERS_NAME)),
							true,
							customer,
							filterDefinitions,
							"");
		}
	}

	private enum DataStoreType {
		PK, FK, UK, IDX
	}
	private String generateDataStoreName(DataStoreType type, String tableName, String columnName) {
		StringBuilder result = new StringBuilder(128);
		result.append(type).append('_');
		if (dialectOptions.isDataStoreIndexNamesInGlobalNamespace() || DataStoreType.FK.equals(type)) {
			result.append(tableName).append('_');
		}
		result.append(columnName);
		String name = result.toString();
		if (identifierIsTooLong(name)) {
			// MD5 hash
		    try {
		        MessageDigest md = MessageDigest.getInstance("MD5");
		        md.reset();
		        md.update(name.getBytes());
		        byte[] digest = md.digest();
		        BigInteger bigInt = new BigInteger(1, digest);
		        // By converting to base 35 (full alphanumeric), we guarantee
		        // that the length of the name will always be smaller than the 30
		        // character identifier restriction enforced by a few dialects.
		        result.setLength(0);
				result.append(type).append('_').append(bigInt.toString(35));
				name = result.toString();
		    }
		    catch (NoSuchAlgorithmException e) {
		        throw new MetaDataException("Unable to generate a hashed Constraint name", e);
		    }
		}
		return name;
	}
	
	private boolean shouldIndex(Boolean index) {
		if (index == null) {
			return dialectOptions.isDataStoreIndexForeignKeys();
		}

		return index.booleanValue();
	}
	
	private static final TreeMap<String, AttributeType> generateDocumentPropertyNames(Document document) {
		TreeMap<String, AttributeType> result = new TreeMap<>();

		for (Attribute attribute : document.getAttributes()) {
			// skip dynamic attributes
			if ((attribute instanceof Field) && ((Field) attribute).isDynamic()) {
				continue;
			}
			// skip bizKey
			if (! attribute.getName().equals(Bean.BIZ_KEY)) {
				result.put(attribute.getName(), attribute.getAttributeType());
			}
		}

		for (String conditionName : ((DocumentImpl) document).getConditionNames()) {
			result.put(conditionName, AttributeType.bool);
		}

		return result;
	}

	private void populatePropertyLengths(Customer customer,
											Module module,
											Document document,
											String derivedPersistentIdentifier) {
		Persistent persistent = document.getPersistent();
		if (persistent != null) {
			String persistentIdentifier = null;
			if (ExtensionStrategy.mapped.equals(persistent.getStrategy())) {
				if (derivedPersistentIdentifier != null) {
					persistentIdentifier = derivedPersistentIdentifier;
				}
			}
			else if (persistent.getName() != null) {
				persistentIdentifier = persistent.getPersistentIdentifier();
			}

			if (persistentIdentifier == null) {
				return;
			}

			Extends inherits = document.getExtends();
			if (inherits != null) {
				Document baseDocument = module.getDocument(customer, inherits.getDocumentName());
				Module baseModule = repository.getModule(customer, baseDocument.getOwningModuleName());
				populatePropertyLengths(customer, baseModule, baseDocument, persistentIdentifier);
			}

			TreeMap<String, Integer> propertyLengths = persistentPropertyLengths.get(persistentIdentifier);
			if (propertyLengths == null) {
				propertyLengths = new TreeMap<>();
				persistentPropertyLengths.put(persistentIdentifier, propertyLengths);
			}

			for (Attribute attribute : document.getAttributes()) {
				// Note - do this for persistent and non-persistent attributes
				// in case a persistent attribute references a non-persistent one (like enumerations do).
				// Include dynamic properties for the same reason
				int length = Integer.MIN_VALUE;
				if (attribute instanceof LengthField) {
					length = ((LengthField) attribute).getLength();
				}
				else if (attribute instanceof Enumeration) {
					// Find the maximum code length
					Enumeration enumeration = (Enumeration) attribute;
					String implementingEnumClassName = enumeration.getImplementingEnumClassName();
					if (implementingEnumClassName != null) { // hand-coded implementation
						// Load the class and find the longest code domain value
						try {
							Class<org.skyve.domain.types.Enumeration> enumerationClass = enumeration.getEnum();
							@SuppressWarnings("unchecked")
							List<DomainValue> values = (List<DomainValue>) enumerationClass.getMethod(org.skyve.domain.types.Enumeration.TO_DOMAIN_VALUES_METHOD_NAME).invoke(null);
							for (DomainValue value : values) {
								int valueLength = value.getCode().length();
								if (valueLength > length) {
									length = valueLength;
								}
							}
						}
						catch (Exception e) {
							throw new MetaDataException("Cannot determine length of enumeration values for implementing class " + implementingEnumClassName, e);
						}
					}
					else { // generated implementation
						// Find the longest code domain value in the metadata
						for (EnumeratedValue value : enumeration.getValues()) {
							int valueLength = value.getCode().length();
							if (valueLength > length) {
								length = valueLength;
							}
						}
					}
				}
				if (length >= 0) {
					String attributeName = attribute.getName();
					Integer existingLength = propertyLengths.get(attributeName);
					if ((existingLength == null) || (existingLength.intValue() < length)) {
						propertyLengths.put(attributeName, Integer.valueOf(length));
					}
				}
			}
		}
	}

	private static abstract class ModuleDocumentVisitor {
		/**
		 * Customer can be null if visiting un-overridden documents only
		 */
		public final void visit(Customer customer, Module module) throws Exception {
			String moduleName = module.getName();

			for (Entry<String, DocumentRef> entry : module.getDocumentRefs().entrySet()) {
				String documentName = entry.getKey();
				DocumentRef ref = entry.getValue();

				// do it for persistent and transient documents defined in this module - not external references
				if (ref.getOwningModuleName().equals(moduleName)) {
					Document document = module.getDocument(customer, documentName);
					accept(document);
				}
			}
		}

		public abstract void accept(Document document) throws Exception;
	}

	/**
	 * Append the enum definition and return the enum type name.
	 * 
	 * @param enumeration
	 * @param enums
	 */
	private static void appendEnumDefinition(Enumeration enumeration, String typeName, StringBuilder enums) {
		attributeJavadoc(enumeration, enums);
		enums.append("\t@XmlEnum\n");
		enums.append("\tpublic static enum ").append(typeName).append(" implements Enumeration {\n");
		for (EnumeratedValue value : enumeration.getValues()) {
			String code = value.getCode();
			String description = value.getDescription();
			if (description == null) {
				description = code;
			}
			enums.append("\t\t").append(value.toJavaIdentifier()).append("(\"").append(code);
			enums.append("\", \"").append(description).append("\"),\n");
		}
		enums.setLength(enums.length() - 2); // remove last '\n' and ','

		// members
		enums.append(";\n\n");
		// enums.append("\t\t/** @hidden */\n");
		enums.append("\t\tprivate String code;\n");
		// enums.append("\t\t/** @hidden */\n");
		enums.append("\t\tprivate String description;\n\n");
		enums.append("\t\t/** @hidden */\n");
		enums.append("\t\tprivate DomainValue domainValue;\n\n");
		enums.append("\t\t/** @hidden */\n");
		enums.append("\t\tprivate static List<DomainValue> domainValues;\n\n");

		// constructor
		enums.append("\t\tprivate ").append(typeName).append("(String code, String description) {\n");
		enums.append("\t\t\tthis.code = code;\n");
		enums.append("\t\t\tthis.description = description;\n");
		enums.append("\t\t\tthis.domainValue = new DomainValue(code, description);\n");
		enums.append("\t\t}\n\n");

		// toCode()
		enums.append("\t\t@Override\n");
		enums.append("\t\tpublic String toCode() {\n");
		enums.append("\t\t\treturn code;\n");
		enums.append("\t\t}\n\n");

		// toLocalisedDescription()
		enums.append("\t\t@Override\n");
		enums.append("\t\tpublic String toLocalisedDescription() {\n");
		enums.append("\t\t\treturn Util.i18n(description);\n");
		enums.append("\t\t}\n\n");

		// toDomainValue()
		enums.append("\t\t@Override\n");
		enums.append("\t\tpublic DomainValue toDomainValue() {\n");
		enums.append("\t\t\treturn domainValue;\n");
		enums.append("\t\t}\n\n");

		// fromCode
		enums.append("\t\tpublic static ").append(typeName).append(" fromCode(String code) {\n");
		enums.append("\t\t\t").append(typeName).append(" result = null;\n\n");
		enums.append("\t\t\tfor (").append(typeName).append(" value : values()) {\n");
		enums.append("\t\t\t\tif (value.code.equals(code)) {\n");
		enums.append("\t\t\t\t\tresult = value;\n");
		enums.append("\t\t\t\t\tbreak;\n");
		enums.append("\t\t\t\t}\n");
		enums.append("\t\t\t}\n\n");
		enums.append("\t\t\treturn result;\n");
		enums.append("\t\t}\n\n");

		// fromLocalisedDescription
		enums.append("\t\tpublic static ").append(typeName).append(" fromLocalisedDescription(String description) {\n");
		enums.append("\t\t\t").append(typeName).append(" result = null;\n\n");
		enums.append("\t\t\tfor (").append(typeName).append(" value : values()) {\n");
		enums.append("\t\t\t\tif (value.toLocalisedDescription().equals(description)) {\n");
		enums.append("\t\t\t\t\tresult = value;\n");
		enums.append("\t\t\t\t\tbreak;\n");
		enums.append("\t\t\t\t}\n");
		enums.append("\t\t\t}\n\n");
		enums.append("\t\t\treturn result;\n");
		enums.append("\t\t}\n\n");

		enums.append("\t\tpublic static List<DomainValue> toDomainValues() {\n");
		enums.append("\t\t\tif (domainValues == null) {\n");
		enums.append("\t\t\t\t").append(typeName).append("[] values = values();\n");
		enums.append("\t\t\t\tdomainValues = new ArrayList<>(values.length);\n");
		enums.append("\t\t\t\tfor (").append(typeName).append(" value : values) {\n");
		enums.append("\t\t\t\t\tdomainValues.add(value.domainValue);\n");
		enums.append("\t\t\t\t}\n");
		enums.append("\t\t\t}\n\n");
		enums.append("\t\t\treturn domainValues;\n");
		enums.append("\t\t}\n");
		enums.append("\t}\n\n");
	}

	private void addReference(Reference reference,
								boolean overriddenReference,
								Customer customer,
								Module owningModule,
								String owningDocumentName,
								boolean owningDomainExtensionClassExists,
								String packagePath,
								Set<String> imports,
								StringBuilder attributes,
								StringBuilder methods) {
		String referenceClassName = reference.getDocumentName();
		Document referenceDocument = owningModule.getDocument(customer, referenceClassName);
		if (referenceDocument.isDynamic()) {
			return;
		}

		String referencePackageName = referenceDocument.getOwningModuleName();
		String name = reference.getName();
		boolean deprecated = reference.isDeprecated();
		boolean tranzient = reference.isTransient();
		ReferenceType type = reference.getType();

		if (overriddenReference) { // overridden reference to concrete implementation
			return; // this already exists on the base class - don't override it.
		}

		String referencePackagePath = new StringBuilder(128).append("modules.")
															.append(referencePackageName)
															.append(".domain").toString();
		// Check for overridden only in customer folder (ie no vanilla document) and change package path accordingly
		Map<String, DomainClass> documentVanillaClasses = moduleDocumentVanillaClasses.get(referencePackageName);
		if ((documentVanillaClasses == null) || (documentVanillaClasses.get(referenceClassName) == null)) {
			referencePackagePath = new StringBuilder(192).append("customers.")
															.append(customer.getName())
															.append('.')
															.append(referencePackagePath).toString();
		}
		
		// Check for Extension class defined and alter the class name accordingly
		String modulePath = ProvidedRepository.MODULES_NAMESPACE + referencePackageName;
		boolean referenceDomainExtensionClassExists = domainExtensionClassExists(modulePath, referenceClassName);
		if (referenceDomainExtensionClassExists) {
			referencePackagePath = new StringBuilder(128).append("modules.")
															.append(referencePackageName)
															.append('.')
															.append(referenceClassName).toString();
			referenceClassName += "Extension";
		}

		if (! referencePackagePath.equals(packagePath)) {
			imports.add(new StringBuilder(256).append(referencePackagePath).append('.').append(referenceClassName).toString());
		}

		String methodName = name.substring(0, 1).toUpperCase() + name.substring(1);

		// Determine the corresponding inverse if any
		// Could have an inverse if not a child collection and not an embedded association
		AbstractInverse inverse = null;
		String inverseMethodName = null;
		if ((! CollectionType.child.equals(type)) &&
				(! AssociationType.embedded.equals(type))) {
			for (Attribute a : getAllAttributes(referenceDocument)) {
				if (a instanceof AbstractInverse) {
					AbstractInverse i = (AbstractInverse) a;
					if (owningDocumentName.equals(i.getDocumentName()) && 
							name.equals(i.getReferenceName())) {
						inverse = i;
						inverseMethodName = i.getName();
						inverseMethodName = Character.toUpperCase(inverseMethodName.charAt(0)) + 
												inverseMethodName.substring(1);
						break;
					}
				}
			}
		}

		if (reference instanceof Collection) {
			imports.add("java.util.List");

			attributeJavadoc(reference, attributes);
			if (deprecated) {
				attributes.append("\t@Deprecated\n");
			}
			attributes.append("\tprivate ");
			if (tranzient) {
				attributes.append("transient ");
			}
			attributes.append("List<").append(referenceClassName).append("> ").append(name);
			if (reference.isTrackChanges()) {
				imports.add("org.skyve.impl.domain.ChangeTrackingArrayList");
				attributes.append(" = new ChangeTrackingArrayList<>(\"");
				attributes.append(name).append("\", this);\n\n");
			}
			else {
				imports.add("java.util.ArrayList");
				attributes.append(" = new ArrayList<>();\n\n");
			}

			// Accessor method
			accessorJavadoc(reference, methods, false);
			if (overriddenReference) { // method in base class
				methods.append("\t@Override\n");
			}
			if (deprecated) {
				methods.append("\t@Deprecated\n");
			}
			methods.append("\t@XmlElement\n");
			methods.append("\tpublic List<").append(referenceClassName).append("> get").append(methodName).append("() {\n");
			methods.append("\t\treturn ").append(name).append(";\n");
			methods.append("\t}\n\n");

			// Mapped Accessor method
			accessorJavadoc(reference, methods, true);
			if (overriddenReference) { // method in base class
				methods.append("\t@Override\n");
			}
			if (deprecated) {
				methods.append("\t@Deprecated\n");
			}
			methods.append("\tpublic ").append(referenceClassName).append(" get").append(methodName).append("ElementById(String bizId) {\n");
			methods.append("\t\treturn getElementById(").append(name).append(", bizId);\n");
			methods.append("\t}\n\n");

			// Mapped Mutator method
			mutatorJavadoc(reference, methods, true);
			if (overriddenReference) { // method in base class
				methods.append("\t@Override\n");
			}
			if (deprecated) {
				methods.append("\t@Deprecated\n");
			}
			methods.append("\tpublic void set").append(methodName);
			methods.append("ElementById(String bizId, ").append(referenceClassName).append(" element) {\n");
			methods.append("\t\tsetElementById(").append(name).append(", element);\n");
			// NB no need to set the parent here as this method does not add any elements ever
			methods.append("\t}\n\n");

			// collection add
			collectionJavadoc(name, methods, true, false);
			if (overriddenReference) { // method in base class
				methods.append("\t@Override\n");
			}
			if (deprecated) {
				methods.append("\t@Deprecated\n");
			}
			methods.append("\tpublic boolean add").append(methodName);
			methods.append("Element(").append(referenceClassName).append(" element) {\n");
			if (CollectionType.child.equals(type)) {
				String referenceParentDocumentName = referenceDocument.getParentDocumentName();
				if (! owningDocumentName.equals(referenceParentDocumentName)) {
					throw new MetaDataException("Document " + owningModule.getName() + '.' + owningDocumentName + " has a child collection named [" + reference.getName() + "] of document " + 
													referenceDocument.getName() + " that has" + 
													((referenceParentDocumentName == null) ? " no parent document (not a child document)" : " a parent document of " + referenceParentDocumentName));
				}
				methods.append("\t\tboolean result = ").append(name).append(".add(element);\n");
				methods.append("\t\telement.setParent(");
				if (owningDomainExtensionClassExists) {
					methods.append("(").append(owningDocumentName).append("Extension) ");
				}
				methods.append("this);\n");
				methods.append("\t\treturn result;\n");
			}
			else if (inverse != null) {
				methods.append("\t\tboolean result = ").append(name).append(".add(element);\n");
				methods.append("\t\telement.get").append(inverseMethodName).append("().add(");
				if (owningDomainExtensionClassExists) {
					methods.append("(").append(owningDocumentName).append("Extension) ");
				}
				methods.append("this);\n");
				methods.append("\t\treturn result;\n");
			}
			else {
				methods.append("\t\treturn ").append(name).append(".add(element);\n");
			}
			methods.append("\t}\n\n");
			
			// collection indexed add
			collectionJavadoc(name, methods, true, true);
			if (overriddenReference) { // method in base class
				methods.append("\t@Override\n");
			}
			if (deprecated) {
				methods.append("\t@Deprecated\n");
			}
			methods.append("\tpublic void add").append(methodName);
			methods.append("Element(int index, ").append(referenceClassName).append(" element) {\n");
			methods.append("\t\t").append(name).append(".add(index, element);\n");
			if (CollectionType.child.equals(type)) {
				methods.append("\t\telement.setParent(");
				if (owningDomainExtensionClassExists) {
					methods.append("(").append(owningDocumentName).append("Extension) ");
				}
				methods.append("this);\n");
			}
			else if (inverse != null) {
				methods.append("\t\telement.get").append(inverseMethodName).append("().add(");
				if (owningDomainExtensionClassExists) {
					methods.append("(").append(owningDocumentName).append("Extension) ");
				}
				methods.append("this);\n");
			}
			methods.append("\t}\n\n");
			
			// collection remove
			collectionJavadoc(name, methods, false, false);
			if (overriddenReference) { // method in base class
				methods.append("\t@Override\n");
			}
			if (deprecated) {
				methods.append("\t@Deprecated\n");
			}
			methods.append("\tpublic boolean remove").append(methodName);
			methods.append("Element(").append(referenceClassName).append(" element) {\n");
			if (CollectionType.child.equals(type)) {
				methods.append("\t\tboolean result = ").append(name).append(".remove(element);\n");
				methods.append("\t\telement.setParent(null);\n");
				methods.append("\t\treturn result;\n");
			}
			else if (inverse != null) {
				methods.append("\t\tboolean result = ").append(name).append(".remove(element);\n");
				methods.append("\t\telement.get").append(inverseMethodName).append("().remove(this);\n");
				methods.append("\t\treturn result;\n");
			}
			else {
				methods.append("\t\treturn ").append(name).append(".remove(element);\n");
			}
			methods.append("\t}\n\n");
			
			// collection indexed remove
			collectionJavadoc(name, methods, false, true);
			if (overriddenReference) { // method in base class
				methods.append("\t@Override\n");
			}
			if (deprecated) {
				methods.append("\t@Deprecated\n");
			}
			methods.append("\tpublic ").append(referenceClassName).append(" remove").append(methodName);
			methods.append("Element(int index) {\n");
			if (CollectionType.child.equals(type)) {
				methods.append("\t\t").append(referenceClassName).append(" result = ").append(name).append(".remove(index);\n");
				methods.append("\t\tresult.setParent(null);\n");
				methods.append("\t\treturn result;\n");
			}
			else if (inverse != null) {
				methods.append("\t\t").append(referenceClassName).append(" result = ").append(name).append(".remove(index);\n");
				methods.append("\t\tresult.get").append(inverseMethodName).append("().remove(this);\n");
				methods.append("\t\treturn result;\n");
			}
			else {
				methods.append("\t\treturn ").append(name).append(".remove(index);\n");
			}
			methods.append("\t}\n\n");
		}
		else { // this is an association Attribute
			attributeJavadoc(reference, attributes);
			if (deprecated) {
				attributes.append("\t@Deprecated\n");
			}
			attributes.append("\tprivate ");
			if (tranzient) {
				attributes.append("transient ");
			}
			attributes.append(referenceClassName).append(" ").append(name);
			attributes.append(" = null;\n\n");

			// Accessor method
			accessorJavadoc(reference, methods, false);
			if (overriddenReference) { // method in base class
				methods.append("\t@Override\n");
			}
			if (deprecated) {
				methods.append("\t@Deprecated\n");
			}
			methods.append("\tpublic ").append(referenceClassName).append(" get").append(methodName).append("() {\n");
			methods.append("\t\treturn ").append(name).append(";\n");
			methods.append("\t}\n\n");

			// Mutator method
			mutatorJavadoc(reference, methods, false);
			if (overriddenReference) { // method in base class
				methods.append("\t@Override\n");
			}
			if (deprecated) {
				methods.append("\t@Deprecated\n");
			}
			methods.append("\t@XmlElement\n");
			methods.append("\tpublic void set").append(methodName).append('(');
			methods.append(referenceClassName).append(' ').append(name).append(") {\n");
			methods.append("\t\tif (this.").append(name).append(" != ").append(name).append(") {\n");
			if (reference.isTrackChanges()) {
				methods.append("\t\t\tpreset(").append(name).append("PropertyName, ").append(name).append(");\n");
			}
			// Embedded child reference - set the parent
			// NB Don't null the parent of the old reference here as it screws hibernate
			if (AssociationType.embedded.equals(type) && (owningDocumentName.equals(referenceDocument.getParentDocumentName()))) {
				methods.append("\t\t\tthis.").append(name).append(" = ").append(name).append(";\n");
				methods.append("\t\t\tif (").append(name).append(" != null) {\n");
				methods.append("\t\t\t\t").append(name).append(".setParent(");
				if (owningDomainExtensionClassExists) {
					methods.append("(").append(owningDocumentName).append("Extension) ");
				}
				methods.append("this);\n");
				methods.append("\t\t\t}\n\n");
			}
			else if (inverse != null) {
				if (InverseRelationship.oneToOne.equals(inverse.getRelationship())) {
					methods.append("\t\t\t").append(referenceClassName).append(" old").append(methodName).append(" = this.").append(name).append(";\n");
					methods.append("\t\t\tthis.").append(name).append(" = ").append(name).append(";\n");
					methods.append("\t\t\tif (").append(name).append(" != null) {\n");
					methods.append("\t\t\t\t").append(name).append(".set").append(inverseMethodName).append('(');
					if (owningDomainExtensionClassExists) {
						methods.append("(").append(owningDocumentName).append("Extension) ");
					}
					methods.append("this);\n");
					methods.append("\t\t\t}\n");
					methods.append("\t\t\tif (old").append(methodName).append(" != null) {\n");
					methods.append("\t\t\t\told").append(methodName).append(".null").append(inverseMethodName).append("();\n");
					methods.append("\t\t\t}\n");
				}
				else {
					methods.append("\t\t\t").append(referenceClassName).append(" old").append(methodName).append(" = this.").append(name).append(";\n");
					methods.append("\t\t\tthis.").append(name).append(" = ").append(name).append(";\n");
					methods.append("\t\t\tif ((").append(name).append(" != null) && (").append(name).append(".get").append(inverseMethodName).append("ElementById(getBizId()) == null)) {\n");
					methods.append("\t\t\t\t").append(name).append(".get").append(inverseMethodName).append("().add(");
					if (owningDomainExtensionClassExists) {
						methods.append("(").append(owningDocumentName).append("Extension) ");
					}
					methods.append("this);\n");
					methods.append("\t\t\t}\n");
					methods.append("\t\t\tif (old").append(methodName).append(" != null) {\n");
					methods.append("\t\t\t\told").append(methodName).append(".get").append(inverseMethodName).append("().remove(this);\n");
					methods.append("\t\t\t}\n");
				}
			}
			else {
				methods.append("\t\t\tthis.").append(name).append(" = ").append(name).append(";\n");
			}
			methods.append("\t\t}\n");
			methods.append("\t}\n\n");

			// if 1 to 1 inverse add null methods
			if (inverse != null) {
				InverseRelationship relationship = inverse.getRelationship();
				if (relationship != InverseRelationship.manyToMany) { // 1:1 or 1:many
					methods.append("\tpublic void null").append(methodName).append("() {\n");
					methods.append("\t\tthis.").append(name).append(" = null;\n");
					methods.append("\t}\n\n");
				}
			}
		}
	}

	private void addInverse(AbstractInverse inverse,
								boolean overriddenInverse,
								Customer customer,
								Module owningModule,
								String owningDocumentName,
								boolean owningDomainExtensionClassExists,
								String packagePath,
								Set<String> imports,
								StringBuilder attributes,
								StringBuilder methods) {
		String propertyClassName = inverse.getDocumentName();
		Document propertyDocument = owningModule.getDocument(customer, propertyClassName);
		if (propertyDocument.isDynamic()) {
			return;
		}
		
		String inverseReferenceName = inverse.getReferenceName();
		String propertyPackageName = propertyDocument.getOwningModuleName();
		String name = inverse.getName();
		InverseRelationship relationship = inverse.getRelationship();
		boolean toMany = (! InverseRelationship.oneToOne.equals(relationship));
		boolean many = InverseRelationship.manyToMany.equals(relationship);
		boolean deprecated = inverse.isDeprecated();
		boolean tranzient = inverse.isTransient();

		String propertyPackagePath = new StringBuilder(128).append("modules.")
																.append(propertyPackageName)
																.append(".domain").toString();
		// Check for overridden only in customer folder (ie no vanilla document) and change package path accordingly
		Map<String, DomainClass> documentVanillaClasses = moduleDocumentVanillaClasses.get(propertyPackageName);
		if ((documentVanillaClasses == null) || (documentVanillaClasses.get(propertyClassName) == null)) {
			propertyPackagePath = new StringBuilder(182).append("customers.")
															.append(customer.getName())
															.append('.')
															.append(propertyPackagePath).toString();
		}
		
		// Check for Extension class defined and alter the class name accordingly
		String modulePath = ProvidedRepository.MODULES_NAMESPACE + propertyPackageName;
		boolean targetDomainExtensionClassExists = domainExtensionClassExists(modulePath, propertyClassName);
		if (targetDomainExtensionClassExists) {
			propertyPackagePath = new StringBuilder(128).append("modules.")
															.append(propertyPackageName)
															.append('.')
															.append(propertyClassName).toString();
			propertyClassName += "Extension";
		}

		if (! propertyPackagePath.equals(packagePath)) {
			imports.add(new StringBuilder(128).append(propertyPackagePath).append('.').append(propertyClassName).toString());
		}

		String methodName = Character.toUpperCase(name.charAt(0)) + name.substring(1);

		if (toMany) {
			imports.add("java.util.List");
			imports.add("java.util.ArrayList");
		}

		attributeJavadoc(inverse, attributes);
		if (deprecated) {
			attributes.append("\t@Deprecated\n");
		}
		if (toMany) {
			attributes.append("\tprivate ");
			if (tranzient) {
				attributes.append("transient ");
			}
			attributes.append("List<").append(propertyClassName).append("> ").append(name);
			attributes.append(" = new ArrayList<>();\n\n");
		}
		else {
			attributes.append("\tprivate ");
			if (tranzient) {
				attributes.append("transient ");
			}
			attributes.append(propertyClassName).append(" ").append(name).append(";\n\n");
		}

		// Accessor method
		accessorJavadoc(inverse, methods, false);
		if (overriddenInverse) { // method in base class
			methods.append("\t@Override\n");
		}
		if (deprecated) {
			methods.append("\t@Deprecated\n");
		}
		methods.append("\t@XmlElement\n");
		if (toMany) {
			methods.append("\tpublic List<").append(propertyClassName).append("> get").append(methodName).append("() {\n");
		}
		else {
			methods.append("\tpublic ").append(propertyClassName).append(" get").append(methodName).append("() {\n");
		}
		methods.append("\t\treturn ").append(name).append(";\n");
		methods.append("\t}\n\n");

		String inverseMethodName = Character.toUpperCase(inverseReferenceName.charAt(0)) + inverseReferenceName.substring(1);

		if (toMany) {
			// Mapped Accessor method
			accessorJavadoc(inverse, methods, true);
			if (overriddenInverse) { // method in base class
				methods.append("\t@Override\n");
			}
			if (deprecated) {
				methods.append("\t@Deprecated\n");
			}
			methods.append("\tpublic ").append(propertyClassName).append(" get").append(methodName).append("ElementById(String bizId) {\n");
			methods.append("\t\treturn getElementById(").append(name).append(", bizId);\n");
			methods.append("\t}\n\n");

			// Mapped Mutator method
			mutatorJavadoc(inverse, methods, true);
			if (overriddenInverse) { // method in base class
				methods.append("\t@Override\n");
			}
			if (deprecated) {
				methods.append("\t@Deprecated\n");
			}
			methods.append("\tpublic void set").append(methodName);
			methods.append("ElementById(String bizId, ").append(propertyClassName).append(" element) {\n");
			methods.append("\t\tsetElementById(").append(name).append(", element);\n");
			// NB no need to set the parent here as this method does not add any elements ever
			methods.append("\t}\n\n");

			// collection add
			collectionJavadoc(name, methods, true, false);
			if (overriddenInverse) { // method in base class
				methods.append("\t@Override\n");
			}
			if (deprecated) {
				methods.append("\t@Deprecated\n");
			}
			methods.append("\tpublic boolean add").append(methodName);
			methods.append("Element(").append(propertyClassName).append(" element) {\n");
			if (many) {
				methods.append("\t\tboolean result = ").append(name).append(".add(element);\n");
				methods.append("\t\telement.get").append(inverseMethodName).append("().add(");
			}
			else {
				methods.append("\t\tboolean result = false;\n");
				methods.append("\t\tif (getElementById(").append(name).append(", element.getBizId()) == null) {\n");
				methods.append("\t\t\tresult = ").append(name).append(".add(element);\n");
				methods.append("\t\t}\n");
				methods.append("\t\telement.set").append(inverseMethodName).append("(");
			}
			if (owningDomainExtensionClassExists) {
				methods.append('(').append(owningDocumentName).append("Extension) ");
			}
			methods.append("this);\n");
			methods.append("\t\treturn result;\n");
			methods.append("\t}\n\n");
			
			// collection indexed add
			collectionJavadoc(name, methods, true, true);
			if (overriddenInverse) { // method in base class
				methods.append("\t@Override\n");
			}
			if (deprecated) {
				methods.append("\t@Deprecated\n");
			}
			methods.append("\tpublic void add").append(methodName);
			methods.append("Element(int index, ").append(propertyClassName).append(" element) {\n");
			methods.append("\t\t").append(name).append(".add(index, element);\n");
			if (many) {
				methods.append("\t\telement.get").append(inverseMethodName).append("().add(");
			}
			else {
				methods.append("\t\telement.set").append(inverseMethodName).append("(");
			}
			if (owningDomainExtensionClassExists) {
				methods.append('(').append(owningDocumentName).append("Extension) ");
			}
			methods.append("this);\n");
			methods.append("\t}\n\n");
			
			// collection remove
			collectionJavadoc(name, methods, false, false);
			if (overriddenInverse) { // method in base class
				methods.append("\t@Override\n");
			}
			if (deprecated) {
				methods.append("\t@Deprecated\n");
			}
			methods.append("\tpublic boolean remove").append(methodName);
			methods.append("Element(").append(propertyClassName).append(" element) {\n");
			methods.append("\t\tboolean result = ").append(name).append(".remove(element);\n");
			methods.append("\t\tif (result) {\n");
			if (many) {
				methods.append("\t\t\telement.get").append(inverseMethodName).append("().remove(this);\n");
			}
			else {
				methods.append("\t\t\telement.null").append(inverseMethodName).append("();\n");
			}
			methods.append("\t\t}\n");
			methods.append("\t\treturn result;\n");
			methods.append("\t}\n\n");
			
			// collection indexed remove
			collectionJavadoc(name, methods, false, true);
			if (overriddenInverse) { // method in base class
				methods.append("\t@Override\n");
			}
			if (deprecated) {
				methods.append("\t@Deprecated\n");
			}
			methods.append("\tpublic ").append(propertyClassName).append(" remove").append(methodName);
			methods.append("Element(int index) {\n");
			methods.append("\t\t").append(propertyClassName).append(" result = ").append(name).append(".remove(index);\n");
			if (many) {
				methods.append("\t\tresult.get").append(inverseMethodName).append("().remove(this);\n");
			}
			else {
				methods.append("\t\tresult.null").append(inverseMethodName).append("();\n");
			}
			methods.append("\t\treturn result;\n");
			methods.append("\t}\n\n");
		}
		else {
			// Mutator method
			mutatorJavadoc(inverse, methods, false);
			if (overriddenInverse) { // method in base class
				methods.append("\t@Override\n");
			}
			if (deprecated) {
				methods.append("\t@Deprecated\n");
			}
			methods.append("\tpublic void set").append(methodName).append("(");
			methods.append(propertyClassName).append(' ').append(name).append(") {\n");
			methods.append("\t\tif (this.").append(name).append(" != ").append(name).append(") {\n");
			methods.append("\t\t\t").append(propertyClassName).append(" old").append(methodName).append(" = this.").append(name).append(";\n");
			methods.append("\t\t\tthis.").append(name).append(" = ").append(name).append(";\n");
			methods.append("\t\t\tif (").append(name).append(" != null) {\n");
			if (many) {
				methods.append("\t\t\t\t").append(name).append(".get").append(inverseMethodName).append("().add(");
			}
			else {
				methods.append("\t\t\t\t").append(name).append(".set").append(inverseMethodName).append("(");
			}
			if (owningDomainExtensionClassExists) {
				methods.append('(').append(owningDocumentName).append("Extension) ");
			}
			methods.append("this);\n");
			methods.append("\t\t\t}\n");
			methods.append("\t\t\tif (old").append(methodName).append(" != null) {\n");
			if (many) {
				methods.append("\t\t\t\told").append(methodName).append(".get").append(inverseMethodName).append("().remove(this);\n");
			}
			else {
				methods.append("\t\t\t\told").append(methodName).append(".null").append(inverseMethodName).append("();\n");
			}
			methods.append("\t\t\t}\n");
			methods.append("\t\t}\n");
			methods.append("\t}\n\n");
			
			// Add null method
			methods.append("\tpublic void null").append(methodName).append("() {\n");
			methods.append("\t\tthis.").append(name).append(" = null;\n");
			methods.append("\t}\n\n");
		}
	}

	@SuppressWarnings("boxing")
	private void generateActionTests(final String moduleName,
										final String packagePath,
										final String modulePath,
										Document document,
										String documentName,
										SkyveFactory annotation) {
		final String actionPath = ProvidedRepository.MODULES_NAMESPACE + moduleName + File.separator + documentName + File.separator + "actions";
		final Path actionTestPath = Paths.get(generatedTestPath + actionPath);

		for (String actionName : document.getDefinedActionNames()) {
			boolean skipGeneration = false, useExtensionDocument = false;

			// check this is a ServerSideAction
			String className = new StringBuilder(256).append(actionTestPath.toString()
																			.replaceAll("\\\\|\\/", ".")
																			.replace(generatedTestPath.replaceAll("\\\\|\\/", "."), ""))
														.append('.')
														.append(actionName).toString();

			// check if this is a server side action, all other types are ignored
			try {
				Class<?> c = Thread.currentThread().getContextClassLoader().loadClass(className);
				if (! ArrayUtils.contains(c.getInterfaces(), ServerSideAction.class)) {
					// System.out.println("Skipping " + actionName + " which is not a ServerSideAction");
					continue;
				}

				// get the document type for this action, the base class or an extension
				Type[] t = c.getGenericInterfaces();
				for (Type type : t) {
					if (type instanceof ParameterizedType) {
						Type[] actualTypeArguments = ((ParameterizedType) type).getActualTypeArguments();
						for (Type param : actualTypeArguments) {
							// if the generic type for this server side action is the extension class, use that
							// NB param.toString() can become param.getTypeName() from Java 8 onwards.
							if (param.toString().endsWith(documentName + "Extension")) {
								useExtensionDocument = true;
								break;
							}
						}
					}
				}
			}
			catch (Exception e) {
				if (debug) System.err.println("Could not find action class for: " + e.getMessage());
			}

			// check if there is a factory extension annotation which skips this test
			if (annotation != null) {
				// skip if all actions are excluded for this document
				// System.out.println("Found annotation for class " + className + " with value " + annotation.testAction());
				if (Boolean.FALSE.equals(annotation.testAction())) {
					continue;
				}

				// skip if this action is excluded for this document
				for (int i = 0; i < annotation.excludedActions().length; i++) {
					// System.out.println("Testing if " + annotation.excludedActions()[i].getName() + " equals " + actionName);
					if (annotation.excludedActions()[i].getSimpleName().equals(actionName)) {
						skipGeneration = true;
						break;
					}
				}
			}

			if (! skipGeneration) {
				Path actionFilePath = actionTestPath.resolve(actionName + "Test.java");
				// don't generate a test if the developer has created an action test in this location in the test directory
				if (testAlreadyExists(actionFilePath)) {
					if (debug) {
						System.out.println(new StringBuilder(256).append("Skipping action test generation for ")
																	.append(actionPath.replaceAll("\\\\|\\/", "."))
																	.append('.')
																	.append(actionName)
																	.append(", file already exists in ")
																	.append(testPath).toString());
					}
					continue;
				}

				// generate the action test
				StringBuilder actionFileContents = new StringBuilder(2048);
				generateActionTest(actionFileContents,
									modulePath,
									actionPath.replaceAll("\\\\|\\/", "."),
									packagePath.replaceAll("\\\\|\\/", "."),
									documentName,
									actionName,
									useExtensionDocument);
				actionFileContents.trimToSize();
				if (write) {
					generation.put(actionFilePath, actionFileContents);
				}
			}
			else {
				if (debug) {
					System.out.println(new StringBuilder(128).append("Skipping action test generation for ")
																.append(actionPath.replaceAll("\\\\|\\/", "."))
																.append('.')
																.append(actionName).toString());
				}
			}
		}
	}

	private void generateActionTest(StringBuilder contents,
										String modulePath,
										String actionPath,
										String domainPath,
										String documentName,
										String actionName,
										boolean useExtensionDocument) {
		if (debug) {
			System.out.println(new StringBuilder(256).append("Generate action test class for ")
														.append(actionPath)
														.append('.')
														.append(actionName).toString());
		}
		contents.append("package ").append(actionPath).append(";\n\n");

		Set<String> imports = new TreeSet<>();

		imports.add("util.AbstractActionTest");
		imports.add("org.skyve.util.DataBuilder");
		imports.add("org.skyve.util.test.SkyveFixture.FixtureType");

		// import the domain class
		imports.add(new StringBuilder(256).append(domainPath).append('.').append(documentName).toString());

		// customise imports if this is not a base class
		if (useExtensionDocument) {
			imports.add(new StringBuilder(256).append(modulePath.replaceAll("\\\\|\\/", "."))
												.append('.')
												.append(documentName)
												.append('.')
												.append(documentName)
												.append("Extension").toString());
		}

		// generate imports
		for (String importClassName : imports) {
			contents.append("import ").append(importClassName).append(";\n");
		}

		// generate javadoc
		contents.append("\n").append("/**");
		contents.append("\n").append(" * Generated - local changes will be overwritten.");
		contents.append("\n").append(" * Extend {@link AbstractActionTest} to create your own tests for this action.");
		contents.append("\n").append(" */");

		// generate class
		contents.append("\n").append("public class ").append(actionName).append("Test");
		contents.append(" extends AbstractActionTest<").append(documentName);
		contents.append(useExtensionDocument ? "Extension" : "").append(", ").append(actionName).append("> {");
		contents.append("\n");

		// generate test body
		contents.append("\n\t").append("@Override");
		contents.append("\n\t").append("protected ").append(actionName).append(" getAction() {");
		contents.append("\n\t\t").append("return new ").append(actionName).append("();");
		contents.append("\n\t").append("}");

		contents.append("\n");
		contents.append("\n\t").append("@Override");
		contents.append("\n\t").append("protected ").append(documentName);
		contents.append(useExtensionDocument ? "Extension" : "").append(" getBean() throws Exception {");
		contents.append("\n\t\t").append("return new DataBuilder()");
		contents.append("\n\t\t\t.fixture(FixtureType.crud)");
		contents.append("\n\t\t\t.build(").append(documentName).append(".MODULE_NAME, ");
		contents.append(documentName).append(".DOCUMENT_NAME);");
		contents.append("\n\t").append("}");
		contents.append("\n}");
	}

	private void generateDomainTest(StringBuilder contents,
										@SuppressWarnings("unused") String modulePath,
										String packagePath,
										String documentName) {
		if (debug) {
			System.out.println(new StringBuilder(256).append("Generate domain test class for ")
														.append(packagePath)
														.append('.')
														.append(documentName).toString());
		}
		contents.append("package ").append(packagePath).append(";\n\n");

		Set<String> imports = new TreeSet<>();

		imports.add("util.AbstractDomainTest");
		imports.add("org.skyve.util.DataBuilder");
		imports.add("org.skyve.util.test.SkyveFixture.FixtureType");

		// generate imports
		for (String importClassName : imports) {
			contents.append("import ").append(importClassName).append(";\n");
		}

		// generate javadoc
		contents.append("\n").append("/**");
		contents.append("\n").append(" * Generated - local changes will be overwritten.");
		contents.append("\n").append(" * Extend {@link AbstractDomainTest} to create your own tests for this document.");
		contents.append("\n").append(" */");

		// generate class
		contents.append("\n").append("public class ").append(documentName).append("Test");
		contents.append(" extends AbstractDomainTest<").append(documentName).append("> {");
		contents.append("\n");

		// generate test body
		contents.append("\n\t").append("@Override");
		contents.append("\n\t").append("protected ").append(documentName).append(" getBean() throws Exception {");
		contents.append("\n\t\t").append("return new DataBuilder()");
		contents.append("\n\t\t\t.fixture(FixtureType.crud)");
		contents.append("\n\t\t\t.build(").append(documentName).append(".MODULE_NAME, ");
		contents.append(documentName).append(".DOCUMENT_NAME);").append("\n\t").append("}");
		contents.append("\n}");
	}

	private void generateJava(Customer customer,
								Module module,
								Document document,
								StringBuilder contents,
								String packagePath,
								String documentName,
								String baseDocumentName,
								boolean overridden) {
		if (debug) {
			System.out.println(new StringBuilder(256).append("Generate class for ")
														.append(packagePath)
														.append('.')
														.append(documentName).toString());
		}
		Persistent persistent = document.getPersistent();
		contents.append("package ").append(packagePath).append(";\n\n");

		Set<String> imports = new TreeSet<>();
		StringBuilder statics = new StringBuilder(1024);
		StringBuilder enums = new StringBuilder(1024);
		StringBuilder attributes = new StringBuilder(1024);
		StringBuilder methods = new StringBuilder(2048);

		TreeMap<String, DomainClass> documentClasses = moduleDocumentVanillaClasses.get(module.getName());
		DomainClass documentClass = (documentClasses == null) ? null : documentClasses.get(documentName);

		// Determine if there is an Extension class defined in the document package
		String modulePath = ProvidedRepository.MODULES_NAMESPACE + module.getName();
		boolean domainExtensionClassExists = domainExtensionClassExists(modulePath, documentName);

		// Document and module names

		if ((! overridden) || (baseDocumentName == null)) { // not an extension
			imports.add("javax.xml.bind.annotation.XmlTransient");
			imports.add("org.skyve.CORE");
			imports.add("org.skyve.domain.messages.DomainException");

			statics.append("\t/** @hidden */\n");
			if (baseDocumentName != null) {
				statics.append("\t@SuppressWarnings(\"hiding\")\n");
			}
			statics.append("\tpublic static final String MODULE_NAME = \"").append(module.getName()).append("\";\n\n");
			statics.append("\t/** @hidden */\n");
			if (baseDocumentName != null) {
				statics.append("\t@SuppressWarnings(\"hiding\")\n");
			}
			statics.append("\tpublic static final String DOCUMENT_NAME = \"").append(document.getName()).append("\";\n\n");

			methods.append("\t@Override");
			methods.append("\n\t@XmlTransient");
			methods.append("\n\tpublic String getBizModule() {\n");
			methods.append("\t\treturn ").append(documentName).append(".MODULE_NAME;\n");
			methods.append("\t}\n\n");

			methods.append("\t@Override");
			methods.append("\n\t@XmlTransient");
			methods.append("\n\tpublic String getBizDocument() {\n");
			methods.append("\t\treturn ").append(documentName).append(".DOCUMENT_NAME;\n");
			methods.append("\t}\n\n");

			// Check for Extension class defined and alter the class returned accordingly
			if (domainExtensionClassExists) {
				imports.add(new StringBuilder(256).append("modules.")
													.append(module.getName())
													.append('.')
													.append(documentName)
													.append('.')
													.append(documentName)
													.append("Extension").toString());
				methods.append("\tpublic static ").append(documentName).append("Extension newInstance() {\n");
			}
			else {
				methods.append("\tpublic static ").append(documentName).append(" newInstance() {\n");
			}
			methods.append("\t\ttry {\n");
			methods.append("\t\t\treturn CORE.getUser().getCustomer().getModule(MODULE_NAME).getDocument(CORE.getUser().getCustomer(), DOCUMENT_NAME).newInstance(CORE.getUser());\n");
			methods.append("\t\t}\n");
			methods.append("\t\tcatch (RuntimeException e) {\n");
			methods.append("\t\t\tthrow e;\n");
			methods.append("\t\t}\n");
			methods.append("\t\tcatch (Exception e) {\n");
			methods.append("\t\t\tthrow new DomainException(e);\n");
			methods.append("\t\t}\n");
			methods.append("\t}\n\n");

			String bizKeyMethodCode = ((DocumentImpl) document).getBizKeyMethodCode();
			if (bizKeyMethodCode != null) {
				methods.append("\t@Override");
				methods.append("\n\t@XmlTransient");
				methods.append("\n\tpublic String getBizKey() {\n");
				methods.append(bizKeyMethodCode).append("\n");
				methods.append("\t}\n\n");
			}

			methods.append("\t@Override");
			methods.append("\n\tpublic boolean equals(Object o) {\n");
			methods.append("\t\treturn ((o instanceof ").append(documentName);
			methods.append(") && \n\t\t\t\t\tthis.getBizId().equals(((");
			methods.append(documentName).append(") o).getBizId()));\n");
			methods.append("\t}\n\n");
		}

		for (Attribute attribute : document.getAttributes()) {
			String name = attribute.getName();
			boolean deprecated = attribute.isDeprecated();

			// skip dynamic attributes (just generate the static final property name)
			if ((attribute instanceof Field) && ((Field) attribute).isDynamic()) {
				statics.append("\t/** @hidden */\n");
				if (deprecated) {
					statics.append("\t@Deprecated\n");
				}
				statics.append("\tpublic static final String ").append(name).append("PropertyName = \"");
				statics.append(attribute.getName()).append("\";\n\n");
				continue;
			}

			imports.add("javax.xml.bind.annotation.XmlElement");

			boolean tranzient = attribute.isTransient();
			// Add if
			// 1) not the bizKey attribute
			// AND
			// 2) We are creating the vanilla class AND the attribute should be present
			// OR
			// 3) Its an extension but the attribute DNE in the vanilla class OR its a customer class that is NOT an override
			if ((! name.equals(Bean.BIZ_KEY)) &&
					(((! overridden) && ((documentClass == null) || documentClass.attributes.containsKey(name))) ||
						(overridden && (((documentClass != null) && (! documentClass.attributes.containsKey(name))) || (documentClass == null))))) {
				statics.append("\t/** @hidden */\n");
				if (deprecated) {
					statics.append("\t@Deprecated\n");
				}
				statics.append("\tpublic static final String ").append(name).append("PropertyName = \"");
				statics.append(attribute.getName()).append("\";\n\n");

				// Generate imports

				AttributeType type = attribute.getAttributeType();
				Class<?> implementingType = type.getImplementingType();
				String methodName = name.substring(0, 1).toUpperCase() + name.substring(1);
				String propertySimpleClassName = null;
				if (attribute instanceof Enumeration) {
					Enumeration enumeration = (Enumeration) attribute;
					
					// skip dynamic attributes
					if (enumeration.isDynamic()) {
						continue;
					}

					String implementingEnumClassName = enumeration.getImplementingEnumClassName();
					if (implementingEnumClassName != null) { // hand-coded implementation
						// cater for inner classes
						implementingEnumClassName = implementingEnumClassName.replace('$', '.');
						final String finalClassName = implementingEnumClassName;
						if (document.getInterfaces().stream().noneMatch(i -> finalClassName.startsWith(i.getInterfaceName() + '.'))) {
							imports.add(finalClassName);
						}
						propertySimpleClassName = implementingEnumClassName.substring(implementingEnumClassName.lastIndexOf('.') + 1);
					}
					else { // generated implementation
						propertySimpleClassName = enumeration.toJavaIdentifier();
	
						if (enumeration.getAttributeRef() != null) { // this is a reference
							if (enumeration.getDocumentRef() != null) { // references a different document
								StringBuilder fullyQualifiedEnumName = new StringBuilder(64);
								fullyQualifiedEnumName.append(enumeration.getEncapsulatingClassName());
								fullyQualifiedEnumName.append('.').append(propertySimpleClassName);
								imports.add(fullyQualifiedEnumName.toString());
							}
						}
						else { // this is an inline definition
							imports.add("org.skyve.domain.types.Enumeration");
							imports.add("org.skyve.metadata.model.document.Bizlet.DomainValue");
							imports.add("org.skyve.util.Util");
							imports.add("java.util.List");
							imports.add("java.util.ArrayList");
							imports.add("javax.xml.bind.annotation.XmlEnum");
	
							appendEnumDefinition(enumeration, propertySimpleClassName, enums);
						}
					}
				}
				else if (attribute instanceof Reference) {
					addReference((Reference) attribute,
									(overridden && // this is an extension class
										// the reference is defined in the base class
										(documentClass != null) &&
										documentClass.attributes.containsKey(attribute.getName())),
									customer,
									module,
									documentName,
									domainExtensionClassExists,
									packagePath,
									imports,
									attributes,
									methods);
					continue;
				}
				else if (attribute instanceof Inverse) {
					addInverse((AbstractInverse) attribute,
								(overridden && // this is an extension class
									// the reference is defined in the base class
									(documentClass != null) &&
									documentClass.attributes.containsKey(attribute.getName())),
								customer,
								module,
								documentName,
								domainExtensionClassExists,
								packagePath,
								imports,
								attributes,
								methods);
					continue;
				}
				else {
					String propertyClassName = implementingType.getName();
					propertySimpleClassName = implementingType.getSimpleName();

					if (! propertyClassName.startsWith("java.lang")) {
						imports.add(propertyClassName);
					}
				}

				// attribute declaration
				attributeJavadoc(attribute, attributes);
				if (deprecated) {
					attributes.append("\t@Deprecated\n");
				}
				attributes.append("\tprivate ");
				if (tranzient) {
					attributes.append("transient ");
				}
				attributes.append(propertySimpleClassName).append(' ').append(name);

				// add attribute definition / default value if required
				String defaultValue = ((Field) attribute).getDefaultValue();
				if (defaultValue != null) {
					if (implementingType.equals(String.class)) {
						if (BindUtil.containsSkyveExpressions(defaultValue)) {
							imports.add("org.skyve.util.Binder");
							attributes.append(" = Binder.formatMessage(\"").append(defaultValue).append("\", this)");
						}
						else {
							// NB cater for escaped {
							attributes.append(" = \"").append(defaultValue.replace("\\{", "{")).append('"');
						}
					}
					else {
						if (BindUtil.isSkyveExpression(defaultValue)) {
							imports.add("org.skyve.util.ExpressionEvaluator");
							attributes.append(" = (").append(propertySimpleClassName).append(") ExpressionEvaluator.evaluate(\"").append(defaultValue).append("\", this)");
						}
						else {
							if (AttributeType.bool.equals(type) ||
									AttributeType.integer.equals(type) ||
									AttributeType.longInteger.equals(type)) {
								attributes.append(" = ").append(propertySimpleClassName).append(".valueOf(");
								attributes.append(defaultValue).append(')');
							}
							else if (AttributeType.enumeration.equals(type)) {
								attributes.append(" = ").append(propertySimpleClassName).append('.').append(defaultValue);
							}
							else {
								attributes.append(" = new ").append(propertySimpleClassName);
								attributes.append("(\"").append(defaultValue).append("\")");
							}
						}
					}
				}
				attributes.append(";\n\n");

				// Accessor method
				accessorJavadoc(attribute, methods, false);
				if (overridden &&
						(baseDocumentName != null) && // base class exists
						(documentClass != null) &&
						documentClass.attributes.containsKey(name)) { // method in base class
					methods.append("\t@Override\n");
				}
				if (deprecated) {
					methods.append("\t@Deprecated\n");
				}
				methods.append("\tpublic ").append(propertySimpleClassName).append(" get").append(methodName).append("() {\n");
				methods.append("\t\treturn ").append(name).append(";\n");
				methods.append("\t}\n\n");

				// Mutator method
				mutatorJavadoc(attribute, methods, false);
				if (overridden &&
						(baseDocumentName != null) && // base class exists
						(documentClass != null) &&
						documentClass.attributes.containsKey(name)) { // method in base class
					methods.append("\t@Override\n");
				}
				if (deprecated) {
					methods.append("\t@Deprecated\n");
				}
				methods.append("\t@XmlElement\n");
				if (AttributeType.date.equals(type)) {
					imports.add("javax.xml.bind.annotation.XmlSchemaType");
					imports.add("javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter");
					imports.add("org.skyve.impl.domain.types.jaxb.DateOnlyMapper");
					methods.append("\t@XmlSchemaType(name = \"date\")\n");
					methods.append("\t@XmlJavaTypeAdapter(DateOnlyMapper.class)\n");
				}
				else if (AttributeType.time.equals(type)) {
					imports.add("javax.xml.bind.annotation.XmlSchemaType");
					imports.add("javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter");
					imports.add("org.skyve.impl.domain.types.jaxb.TimeOnlyMapper");
					methods.append("\t@XmlSchemaType(name = \"time\")\n");
					methods.append("\t@XmlJavaTypeAdapter(TimeOnlyMapper.class)\n");
				}
				else if (AttributeType.dateTime.equals(type)) {
					imports.add("javax.xml.bind.annotation.XmlSchemaType");
					imports.add("javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter");
					imports.add("org.skyve.impl.domain.types.jaxb.DateTimeMapper");
					methods.append("\t@XmlSchemaType(name = \"dateTime\")\n");
					methods.append("\t@XmlJavaTypeAdapter(DateTimeMapper.class)\n");
				}
				else if (AttributeType.timestamp.equals(type)) {
					imports.add("javax.xml.bind.annotation.XmlSchemaType");
					imports.add("javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter");
					imports.add("org.skyve.impl.domain.types.jaxb.TimestampMapper");
					methods.append("\t@XmlSchemaType(name = \"dateTime\")\n");
					methods.append("\t@XmlJavaTypeAdapter(TimestampMapper.class)\n");
				}
				else if (AttributeType.decimal2.equals(type)) {
					imports.add("javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter");
					imports.add("org.skyve.impl.domain.types.jaxb.Decimal2Mapper");
					methods.append("\t@XmlJavaTypeAdapter(Decimal2Mapper.class)\n");
				}
				else if (AttributeType.decimal5.equals(type)) {
					imports.add("javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter");
					imports.add("org.skyve.impl.domain.types.jaxb.Decimal5Mapper");
					methods.append("\t@XmlJavaTypeAdapter(Decimal5Mapper.class)\n");
				}
				else if (AttributeType.decimal10.equals(type)) {
					imports.add("javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter");
					imports.add("org.skyve.impl.domain.types.jaxb.Decimal10Mapper");
					methods.append("\t@XmlJavaTypeAdapter(Decimal10Mapper.class)\n");
				}
				else if (AttributeType.geometry.equals(type)) {
					imports.add("javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter");
					imports.add("org.skyve.impl.domain.types.jaxb.GeometryMapper");
					methods.append("\t@XmlJavaTypeAdapter(GeometryMapper.class)\n");
				}
				methods.append("\tpublic void set").append(methodName).append('(');
				methods.append(propertySimpleClassName).append(' ').append(name).append(") {\n");
				if (attribute.isTrackChanges()) {
					methods.append("\t\tpreset(").append(name).append("PropertyName, ").append(name).append(");\n");
				}
				methods.append("\t\tthis.").append(name).append(" = ").append(name).append(";\n");
				methods.append("\t}\n\n");
			}
		}

		String parentDocumentName = document.getParentDocumentName();
		Document parentDocument = null;
		String parentPackagePath = null;
		String parentClassName = null;

		if (parentDocumentName != null) {
			parentDocument = module.getDocument(customer, parentDocumentName);
			String parentPackageName = parentDocument.getOwningModuleName();
			parentPackagePath = new StringBuilder(128).append("modules.")
														.append(parentPackageName)
														.append(".domain").toString();
			parentClassName = parentDocumentName;
			String parentDocumentModulePath = ProvidedRepository.MODULES_NAMESPACE + parentPackageName;
			if (domainExtensionClassExists(parentDocumentModulePath, parentDocumentName)) {
				parentPackagePath = new StringBuilder(128).append("modules.")
															.append(parentPackageName)
															.append('.')
															.append(parentDocumentName).toString();
				parentClassName += "Extension";
			}

			if (parentDocumentName.equals(documentName)) { // hierarchical
				imports.add("java.util.List");
				imports.add("org.skyve.CORE");
				imports.add("org.skyve.domain.Bean");
				imports.add("org.skyve.domain.HierarchicalBean");
				imports.add("org.skyve.persistence.DocumentQuery");
				imports.add("org.skyve.persistence.Persistence");
			}
			else {
				if (! parentPackagePath.equals(packagePath)) {
					imports.add(parentPackagePath + '.' + parentClassName);
				}
				imports.add("org.skyve.domain.ChildBean");
			}

			// add import for parent setter if there are no attributes in the child
			if (document.getAttributes().size() == 0) {
				imports.add("javax.xml.bind.annotation.XmlElement");
			}
		}

		boolean polymorphic = testPolymorphic(document);
		if (polymorphic) {
			imports.add("org.skyve.domain.PolymorphicPersistentBean");
		}

		// indicates if the base document has <BaseDocument>Extension.java defined in the document folder.
		boolean baseDocumentExtensionClassExists = false;
		if (baseDocumentName != null) {
			Document baseDocument = module.getDocument(customer, baseDocumentName);
			String baseDocumentExtensionPath = new StringBuilder(256).append(srcPath)
																		.append(ProvidedRepository.MODULES_NAMESPACE)
																		.append(baseDocument.getOwningModuleName())
																		.append('/')
																		.append(baseDocumentName)
																		.append('/')
																		.append(baseDocumentName)
																		.append("Extension.java").toString();
			baseDocumentExtensionClassExists = new File(baseDocumentExtensionPath).exists();
			if (baseDocumentExtensionClassExists) {
				imports.add(new StringBuilder(128).append("modules.")
													.append(baseDocument.getOwningModuleName())
													.append('.')
													.append(baseDocumentName)
													.append('.')
													.append(baseDocumentName)
													.append("Extension").toString());
			}
			else {
				String basePackgePath = new StringBuilder(128).append("modules.")
																.append(baseDocument.getOwningModuleName())
																.append(".domain").toString();
				if (! basePackgePath.equals(packagePath)) {
					imports.add(basePackgePath + '.' + baseDocumentName);
				}
			}
		}

		// Add extra imports required if this is not a base class
		if (baseDocumentName == null) {
			if (persistent != null) {
				imports.add("org.skyve.impl.domain.AbstractPersistentBean");
			}
			else {
				imports.add("org.skyve.impl.domain.AbstractTransientBean");
			}
		}

		// Add conditions
		Map<String, Condition> conditions = ((DocumentImpl) document).getConditions();
		if (conditions != null) {
			for (String conditionName : conditions.keySet()) {
				Condition condition = conditions.get(conditionName);

				if ((! overridden) ||
						(documentClass == null) ||
						(!documentClass.attributes.containsKey(conditionName))) {
					imports.add("javax.xml.bind.annotation.XmlTransient");

					boolean overriddenCondition = "created".equals(conditionName);

					// Generate/Include UML doc
					String description = condition.getDocumentation();
					if (description == null) {
						description = condition.getDescription();
					}
					if (description == null) {
						description = conditionName;
					}
					methods.append("\t/**");
					String doc = condition.getDescription();
					boolean nothingDocumented = true;
					if (doc != null) {
						nothingDocumented = false;
						methods.append("\n\t * ").append(doc);
					}
					doc = condition.getDocumentation();
					if (doc != null) {
						nothingDocumented = false;
						methods.append("\n\t * ").append(doc);
					}
					if (nothingDocumented) {
						methods.append("\n\t * ").append(conditionName);
					}
					methods.append("\n\t *")
							.append("\n\t * @return The condition")
							.append("\n\t */");

					methods.append("\n\t@XmlTransient");
					if (overriddenCondition) {
						methods.append("\n\t@Override");
					}
					String methodName = new StringBuilder(64).append("is")
																.append(Character.toUpperCase(conditionName.charAt(0)))
																.append(conditionName.substring(1)).toString();
					String expression = condition.getExpression();
					methods.append("\n\tpublic boolean ").append(methodName).append("() {");
					if (BindUtil.isSkyveExpression(expression)) {
						imports.add("org.skyve.util.ExpressionEvaluator");
						methods.append("\n\t\treturn Boolean.TRUE.equals(ExpressionEvaluator.evaluate(\"");
						methods.append(expression);
						methods.append("\", this));");
					}
					else {
						methods.append("\n\t\treturn (").append(expression).append(");");
					}
					methods.append("\n\t}\n\n");

					methods.append("\t/**")
							.append("\n\t * {@link #").append(methodName).append("} negation.")
							.append("\n\t *")
							.append("\n\t * @return The negated condition")
							.append("\n\t */");
					if (overriddenCondition) {
						methods.append("\n\t@Override");
					}
					methods.append("\n\tpublic boolean isNot").append(Character.toUpperCase(conditionName.charAt(0)))
							.append(conditionName.substring(1)).append("() {")
							.append("\n\t\treturn (! is").append(Character.toUpperCase(conditionName.charAt(0)))
							.append(conditionName.substring(1)).append("());")
							.append("\n\t}\n\n");
				}
			}
		}

		imports.add("javax.xml.bind.annotation.XmlType");
		imports.add("javax.xml.bind.annotation.XmlRootElement");

		// Add parent reference and bizOrdinal property
		// if this is a base class of a child document
		if ((parentDocumentName != null) &&
				((! overridden) || (baseDocumentName == null))) {
			if (parentDocumentName.equals(documentName)) {
				attributes.append("\tprivate String bizParentId;\n\n");

				// Accessor method
				methods.append("\t@Override\n");
				methods.append("\tpublic String getBizParentId() {\n");
				methods.append("\t\treturn bizParentId;\n");
				methods.append("\t}\n\n");

				// Mutator method
				methods.append("\t@Override\n");
				methods.append("\t@XmlElement\n");
				methods.append("\tpublic void setBizParentId(String bizParentId) {\n");
				methods.append("\t\tpreset(HierarchicalBean.PARENT_ID, bizParentId);\n");
				methods.append("\t\tthis.bizParentId = bizParentId;\n");
				methods.append("\t}\n\n");

				// Traversal method
				methods.append("\t@Override\n");
				methods.append("\tpublic ").append(documentName);
				if (domainExtensionClassExists) {
					methods.append("Extension");
				}
				methods.append(" getParent() {\n");
				methods.append("\t\t").append(documentName);
				if (domainExtensionClassExists) {
					methods.append("Extension");
				}
				methods.append(" result = null;\n\n");
				methods.append("\t\tif (bizParentId != null) {\n");
				methods.append("\t\t\tPersistence p = CORE.getPersistence();\n");
				methods.append("\t\t\tDocumentQuery q = p.newDocumentQuery(").append(documentName).append(".MODULE_NAME, ").append(documentName).append(".DOCUMENT_NAME);\n");
				methods.append("\t\t\tq.getFilter().addEquals(Bean.DOCUMENT_ID, bizParentId);\n");
				methods.append("\t\t\tresult = q.retrieveBean();\n");
				methods.append("\t\t}\n\n");
				methods.append("\t\treturn result;\n");
				methods.append("\t}\n\n");

				// Traversal method
				methods.append("\t@Override\n");
				methods.append("\t@XmlTransient\n");
				methods.append("\tpublic List<").append(documentName);
				if (domainExtensionClassExists) {
					methods.append("Extension");
				}
				methods.append("> getChildren() {\n");
				methods.append("\t\tPersistence p = CORE.getPersistence();\n");
				methods.append("\t\tDocumentQuery q = p.newDocumentQuery(").append(documentName).append(".MODULE_NAME, ").append(documentName).append(".DOCUMENT_NAME);\n");
				methods.append("\t\tq.getFilter().addEquals(HierarchicalBean.PARENT_ID, getBizId());\n");
				methods.append("\t\treturn q.beanResults();\n");
				methods.append("\t}\n\n");
			}
			else {
				attributes.append("\tprivate ").append(parentClassName).append(" parent;\n\n");

				// Accessor method
				methods.append("\t@Override\n");
				methods.append("\tpublic ").append(parentClassName).append(" getParent() {\n");
				methods.append("\t\treturn parent;\n");
				methods.append("\t}\n\n");

				// Determine the corresponding child collection
				String childCollectionMethodName = null;
				String embeddedAssociationMethodName = null;
				if (parentDocument != null) {
					for (Attribute a : getAllAttributes(parentDocument)) {
						if (a instanceof Collection) {
							Collection col = (Collection) a;
							if (CollectionType.child.equals(col.getType()) && 
									documentName.equals(col.getDocumentName())) {
								childCollectionMethodName = col.getName();
								childCollectionMethodName = Character.toUpperCase(childCollectionMethodName.charAt(0)) + 
																childCollectionMethodName.substring(1);
								break;
							}
						}
						else if (a instanceof Association) {
							Association ass = (Association) a;
							if (AssociationType.embedded.equals(ass.getType()) &&
									documentName.equals(ass.getDocumentName())) {
								embeddedAssociationMethodName = ass.getName();
								embeddedAssociationMethodName = Character.toUpperCase(embeddedAssociationMethodName.charAt(0)) + 
																	embeddedAssociationMethodName.substring(1);
								break;
							}
						}
					}
				}
				
				// Mutator method
				methods.append("\t@Override\n");
				methods.append("\t@XmlElement\n");
				methods.append("\tpublic void setParent(");
				methods.append(parentClassName).append(" parent) {\n");
				methods.append("\t\tif (this.parent != parent) {\n");
				if ((childCollectionMethodName != null) || (embeddedAssociationMethodName != null)) {
// TODO ENABLE BI-DIRECTIONAL					methods.append("\t\t\t").append(parentClassName).append(" old = this.parent;\n");
					methods.append("\t\t\tpreset(ChildBean.PARENT_NAME, parent);\n");
					methods.append("\t\t\tthis.parent = parent;\n");
/* TODO ENABLE BI-DIRECTIONAL
					if (childCollectionMethodName != null) {
						methods.append("\t\t\tif ((parent != null) && (parent.get").append(childCollectionMethodName).append("ElementById(getBizId()) == null)) {\n");
						methods.append("\t\t\t\tparent.get").append(childCollectionMethodName).append("().add(");
					}
					else {
						methods.append("\t\t\tif (parent != null) {\n");
						methods.append("\t\t\t\tparent.set").append(embeddedAssociationMethodName).append('(');
					}
					if (domainExtensionClassExists) {
						methods.append('(').append(documentName).append("Extension) ");
					}
					methods.append("this);\n");
					methods.append("\t\t\t}\n");
					methods.append("\t\t\tif (old != null) {\n");
					if (childCollectionMethodName != null) {
						methods.append("\t\t\t\told.get").append(childCollectionMethodName).append("().remove(this);\n");
					}
					else {
						methods.append("\t\t\t\told.set").append(embeddedAssociationMethodName).append("(null);\n");
					}
					methods.append("\t\t\t}\n");
*/
					methods.append("\t\t}\n");
				}
				else {
					throw new MetaDataException("Child Document " + module.getName() + '.' + documentName + " defines a parent document of " + 
													parentDocumentName + " but " + parentDocumentName + " has no child collection or embedded association to its child document");
				}
				methods.append("\t}\n\n");

				// BizOrdinal property
				imports.add("org.skyve.domain.Bean");
				attributes.append("\tprivate Integer bizOrdinal;\n\n");

				// Accessor method
				methods.append("\t@Override\n");
				methods.append("\tpublic Integer getBizOrdinal() {\n");
				methods.append("\t\treturn bizOrdinal;\n");
				methods.append("\t}\n\n");

				// Mutator method
				methods.append("\t@Override\n");
				methods.append("\t@XmlElement\n");
				methods.append("\tpublic void setBizOrdinal(Integer bizOrdinal) {\n");
				methods.append("\t\tpreset(Bean.ORDINAL_NAME, bizOrdinal);\n");
				methods.append("\t\tthis.bizOrdinal = ").append(" bizOrdinal;\n");
				methods.append("\t}\n\n");
			}
		}

		for (String importClassName : imports) {
			contents.append("import ").append(importClassName).append(";\n");
		}

		// Generate/Include UML doc
		contents.append("\n/**");
		contents.append("\n * ").append(document.getLocalisedSingularAlias());
		String doc = document.getLocalisedDescription();
		if (doc != null) {
			contents.append("\n * <br/>");
			contents.append("\n * ").append(doc);
		}
		doc = document.getDocumentation();
		if (doc != null) {
			contents.append("\n * <br/>");
			contents.append("\n * ").append(doc);
		}
		contents.append("\n * \n");

		for (Attribute attribute : document.getAttributes()) {
			if (attribute instanceof Enumeration) {
				Enumeration enumeration = (Enumeration) attribute;
				if (! enumeration.isDynamic()) {
					contents.append(" * @depend - - - ").append(enumeration.toJavaIdentifier()).append('\n');
				}
			}
		}

		for (String referenceName : document.getReferenceNames()) {
			Reference reference = document.getReferenceByName(referenceName);
			ReferenceType type = reference.getType();
			boolean required = reference.isRequired();
			if (AssociationType.aggregation.equals(type)) {
				contents.append(" * @navhas n ").append(referenceName).append(required ? " 1 " : " 0..1 ").append(reference.getDocumentName()).append('\n');
			}
			else if (AssociationType.composition.equals(type) || AssociationType.embedded.equals(type)) {
				contents.append(" * @navcomposed n ").append(referenceName).append(required ? " 1 " : " 0..1 ").append(reference.getDocumentName()).append('\n');
			}
			else {
				if (CollectionType.aggregation.equals(type)) {
					contents.append(" * @navhas n ");
				}
				else if (CollectionType.composition.equals(type)) {
					contents.append(" * @navcomposed n ");
				}
				else if (CollectionType.child.equals(type)) {
					contents.append(" * @navcomposed 1 ");
				}
				contents.append(referenceName);
				Collection collection = (Collection) reference;
				Integer min = collection.getMinCardinality();
				contents.append(' ').append(min.toString()).append("..");
				Integer max = collection.getMaxCardinality();
				if (max == null) {
					contents.append("n ");
				} else {
					contents.append(max.toString()).append(' ');
				}
				contents.append(reference.getDocumentName()).append('\n');
			}
		}
		if (persistent == null) {
			contents.append(" * @stereotype \"transient");
		}
		else {
			contents.append(" * @stereotype \"persistent");
		}
		if (parentDocumentName != null) {
			contents.append(" child\"\n");
			// contents.append(" * @navassoc 1 parent 1 ").append(parentDocumentName).append('\n');
		}
		else {
			contents.append("\"\n");
		}

		contents.append(" */\n");

		// generate class body
		contents.append("@XmlType");
		contents.append("\n@XmlRootElement");
		if (polymorphic) {
			contents.append("\n@PolymorphicPersistentBean");
		}
		contents.append("\npublic ");
		if (domainExtensionClassExists) {
			contents.append("abstract ");
		}
		else if (baseDocumentName == null) {
			TreeMap<String, DomainClass> domainClasses = moduleDocumentVanillaClasses.get(module.getName());
			DomainClass domainClass = (domainClasses == null) ? null : domainClasses.get(documentName);
			if (document.isAbstract() || ((domainClass != null) && (domainClass.isAbstract))) {
				contents.append("abstract ");
			}
		}
		else {
			if (document.isAbstract()) {
				contents.append("abstract ");
			}
		}
		contents.append("class ").append(documentName);
		if (baseDocumentName != null) { // extension
			if (overridden) {
				contents.append("Ext");
			}

			if (baseDocumentExtensionClassExists) {
				contents.append(" extends ").append(baseDocumentName).append("Extension");
			}
			else {
				contents.append(" extends ").append(baseDocumentName);
			}
		}
		else {
			contents.append(" extends Abstract").append((persistent == null) ? "TransientBean" : "PersistentBean");
		}

		final String interfacesCommaSeparated = document.getInterfaces().stream()
															.map(Interface::getInterfaceName)
															.collect(Collectors.joining(", "));
		if (parentDocumentName != null) {
			if (parentDocumentName.equals(documentName)) { // hierarchical
				contents.append(" implements HierarchicalBean<").append(parentClassName).append('>');
			}
			else {
				contents.append(" implements ChildBean<").append(parentClassName).append('>');
			}

			if (! document.getInterfaces().isEmpty()) {
				contents.append(", ").append(interfacesCommaSeparated);
			}
		}
		else if (! document.getInterfaces().isEmpty()) {
			contents.append(" implements ").append(interfacesCommaSeparated);
		}

		contents.append(" {\n");

		contents.append("\t/**\n");
		contents.append("\t * For Serialization\n");
		contents.append("\t * @hidden\n");
		contents.append("\t */\n");
		contents.append("\tprivate static final long serialVersionUID = 1L;\n\n");

		if (statics.length() > 0) {
			contents.append(statics);
		}
		if (enums.length() > 0) {
			contents.append(enums);
		}
		contents.append(attributes);
		if (methods.length() > 0) {
			contents.append(methods, 0, methods.length() - 1); // don't include last \n
		}

		contents.append("}\n");
	}

	private boolean testPolymorphic(Document document) {
		// If this is a mapped document, its not polymorphic - it can't be queried and isn't really persistent
		Persistent persistent = document.getPersistent();
		if (persistent != null) {
			if (ExtensionStrategy.mapped.equals(persistent.getStrategy())) {
				return false;
			}
		} else {
			return false;
		}

		return testPolymorphicAllTheWayDown(document);
	}

	private boolean testPolymorphicAllTheWayDown(Document document) {
		// If any sub-document down the tree is single or joined strategy, then this document is polymorphic
		TreeMap<String, Document> derivations = modocDerivations.get(document.getOwningModuleName() + '.' + document.getName());
		if (derivations != null) {
			for (Document derivation : derivations.values()) {
				Persistent derivationPersistent = derivation.getPersistent();
				ExtensionStrategy derivationStrategy = (derivationPersistent == null) ? null : derivationPersistent.getStrategy();
				if (ExtensionStrategy.single.equals(derivationStrategy) ||
						ExtensionStrategy.joined.equals(derivationStrategy)) {
					return true;
				}
				if (testPolymorphic(derivation)) {
					return true;
				}
			}
		}
		return false;
	}

	private static void attributeJavadoc(Attribute attribute, StringBuilder toAppendTo) {
		toAppendTo.append("\t/**\n");
		toAppendTo.append("\t * ").append(attribute.getLocalisedDisplayName()).append('\n');
		String doc = attribute.getLocalisedDescription();
		if (doc != null) {
			toAppendTo.append("\t * <br/>\n");
			toAppendTo.append("\t * ").append(doc).append("\n");
		}
		doc = attribute.getDocumentation();
		if (doc != null) {
			toAppendTo.append("\t * <br/>\n");
			toAppendTo.append("\t * ").append(doc).append("\n");
		}
		toAppendTo.append("\t **/\n");
	}

	private static void accessorJavadoc(Attribute attribute, StringBuilder toAppendTo, boolean mapped) {
		toAppendTo.append("\t/**\n");
		toAppendTo.append("\t * {@link #").append(attribute.getName()).append("} accessor.\n");
		if (mapped) {
			toAppendTo.append("\t * @param bizId\tThe bizId of the element in the list.\n");
			toAppendTo.append("\t * @return\tThe value of the element in the list.\n");
		}
		else {
			toAppendTo.append("\t * @return\tThe value.\n");
		}
		toAppendTo.append("\t **/\n");
	}

	private static void mutatorJavadoc(Attribute attribute, StringBuilder toAppendTo, boolean mapped) {
		toAppendTo.append("\t/**\n");
		toAppendTo.append("\t * {@link #").append(attribute.getName()).append("} mutator.\n");
		if (mapped) {
			toAppendTo.append("\t * @param bizId\tThe bizId of the element in the list.\n");
			toAppendTo.append("\t * @param element\tThe new value of the element in the list.\n");
		}
		else {
			toAppendTo.append("\t * @param ").append(attribute.getName()).append("\tThe new value.\n");
		}
		toAppendTo.append("\t **/\n");
	}


	private static void collectionJavadoc(String attributeName, StringBuilder toAppendTo, boolean add, boolean indexed) {
		toAppendTo.append("\t/**\n");
		toAppendTo.append("\t * {@link #").append(attributeName).append(add ? "} add.\n" : "} remove.\n");
		if (indexed) {
			toAppendTo.append("\t * @param index\tThe index in the list to ").append(add ? "add" : "remove");
			toAppendTo.append(" the element ").append(add ? "to.\n" : "from.\n");
		}
		if (add) {
			toAppendTo.append("\t * @param element\tThe element to add.\n");
		}
		else if (! indexed) {
			toAppendTo.append("\t * @param element\tThe element to remove.\n");
		}
		toAppendTo.append("\t **/\n");
	}

	/**
	 * Checks if a domain extension class exists for the given document name in the specified package
	 * and module path.
	 * 
	 * @param modulePath the path to the document's module; e.g. modules/admin
	 * @param documentName The name of the document, e.g. Audit
	 * @return true if the extension class exists in the expected location, false otherwise
	 */
	private boolean domainExtensionClassExists(String modulePath, String documentName) {
		if (Files.exists(Paths.get(srcPath, modulePath, documentName, documentName + "Extension.java"))) {
			return true;
		}

		return false;
	}

	/**
	 * Return all vanilla attributes for the specified document.
	 */
	private List<? extends Attribute> getAllAttributes(Document document) {
		List<Attribute> result = new ArrayList<>(document.getAttributes());
		Extends currentInherits = document.getExtends();
		if (currentInherits != null) {
			while (currentInherits != null) {
				Module module = repository.getModule(null, document.getOwningModuleName());
				Document baseDocument = module.getDocument(null, currentInherits.getDocumentName());
				result.addAll(baseDocument.getAttributes());
				currentInherits = baseDocument.getExtends();
			}
		}

		return Collections.unmodifiableList(result);
	}

	/**
	 * Checks if a file in the TEST_PATH already exists for the test about
	 * to be created in the GENERATED_TEST_PATH.
	 * 
	 * @param testToBeCreated The full file path to the test file about to be created
	 * @return True if developer has already created a file with the name, false otherwise
	 */
	private boolean testAlreadyExists(Path testToBeCreated) {
		File testFile = new File(testToBeCreated.toString().replace("\\", "/").replace(generatedTestPath.replace("\\", "/"),
									testPath.replace("\\", "/")));
		return testFile.exists();
	}

	/**
	 * Validates the attribute names of the specified document are valid and not a
	 * reserved word of the dialect passed into this domain generator.
	 * 
	 * @param document The document containing the attributes to be visited
	 */
	private void validateDocumentAttributeNames(final Document document) {
		if (document != null) {
			for (Attribute attribute : document.getAttributes()) {
				// attribute names cannot contain underscore
				if (attribute.getName().contains("_")) {
					throw new MetaDataException(new StringBuilder(256).append("Document ")
																		.append(document.getOwningModuleName())
																		.append('.')
																		.append(document.getName())
																		.append(" cannot contain attribute named ")
																		.append(attribute.getName())
																		.append(" because it contains an underscore. Underscores are reserved for JSON serialisation.").toString());
				}

				// ignore dynamic attributes from here
				if ((attribute instanceof Field) && ((Field) attribute).isDynamic()) {
					continue;
				}
				
				AttributeType type = attribute.getAttributeType();

				if (document.getPersistent() == null || attribute.isPersistent() == false) {
					// return, attribute is transient
					if (debug) {
						UtilImpl.LOGGER.fine(new StringBuilder(128).append("Ignoring transient attribute ")
																	.append(attribute.getName())
																	.append(" for document ")
																	.append(document.getName()).toString());
					}
					continue;
				}

				// skip checking association or collections as their persistent field name will be modified
				if (AttributeType.collection.equals(type) || AttributeType.association.equals(type)
						|| AttributeType.inverseOne.equals(type) || AttributeType.inverseMany.equals(type)) {
					continue;
				}

				// check not using a reserved word
				switch (dialectOptions) {
					case MSSQL_2014:
					case MSSQL_2016:
						if (SQL_SERVER_RESERVED_WORDS.contains(attribute.getName().toLowerCase())) {
							throw new MetaDataException(
									createDialectError(document, attribute));
						}
						break;
					case MYSQL_5:
					case MYSQL_5_4_BYTE_CHARSET:
						if (MYSQL_5_RESERVED_WORDS.contains(attribute.getName().toLowerCase())) {
							throw new MetaDataException(
									createDialectError(document, attribute));
						}
						break;
					case MYSQL_8:
					case MYSQL_8_4_BYTE_CHARSET:
						if (MYSQL_8_RESERVED_WORDS.contains(attribute.getName().toLowerCase())) {
							throw new MetaDataException(
									createDialectError(document, attribute));
						}
						break;
					case POSTGRESQL:
						if (POSTGRESQL_RESERVED_WORDS.contains(attribute.getName().toLowerCase())) {
							throw new MetaDataException(
									createDialectError(document, attribute));
						}
						break;
					case H2:
					case H2_NO_INDEXES:
					default:
						// H2 is the default dialect
						if (H2_RESERVED_WORDS.contains(attribute.getName().toLowerCase())) {
							System.err.println("Reserved word: " + attribute.getName());
							throw new MetaDataException(
									createDialectError(document, attribute));
						}
						break;
				}
			}
		}
	}

	private String createDialectError(final Document document, Attribute attribute) {
		return new StringBuilder(256).append("Document ")
										.append(document.getOwningModuleName())
										.append('.')
										.append(document.getName())
										.append(" cannot contain attribute named \"")
										.append(attribute.getName())
										.append("\" because it is a reserved word in database dialect ")
										.append(dialectOptions.getDescription())
										.append('.').toString();
	}

	/**
	 * Deletes the specified directory and all subfolders/files
	 * 
	 * @param directory The path to the directory to delete
	 * @throws IOException
	 */
	private static void deleteDirectory(Path directory) throws IOException {
		if (Files.exists(directory)) {
			Files.walkFileTree(directory, new SimpleFileVisitor<Path>() {
				@Override
				public FileVisitResult visitFile(Path path, BasicFileAttributes basicFileAttributes) throws IOException {
					Files.delete(path);
					return FileVisitResult.CONTINUE;
				}

				@Override
				public FileVisitResult postVisitDirectory(Path subDir, IOException ioException) throws IOException {
					Files.delete(subDir);
					return FileVisitResult.CONTINUE;
				}
			});
		}
	}

	/**
	 * Deletes the generated source domain and test directories and all files below them for all active
	 * modules. Deletes the entire source and test generated directories for unreferenced modules. This
	 * is to ensure a clean start for generating the domain.
	 * 
	 * @param moduleNames The list of all active module names
	 * 
	 * @throws IOException
	 */
	private void replaceGenerated(List<String> moduleNames) throws IOException {
		// src/main/java/generated/modules
		final Path generatedDirectory = Paths.get(generatedSrcPath, ProvidedRepository.MODULES_NAMESPACE);

		// get all directories at this level
		final File[] generatedFiles = generatedDirectory.toFile().listFiles();
		if (generatedFiles != null) {
			for (File child : generatedFiles) {
				if (child.isDirectory()) {
					String childName = child.getName();
					if (moduleNames.contains(childName)) {
						final Path packagePath = generatedDirectory.resolve(childName).resolve(ProvidedRepository.DOMAIN_NAME);
						if (Files.exists(packagePath)) {
							for (File domainFile : packagePath.toFile().listFiles()) {
								domainFile.delete();
							}
						}
						else {
							Files.createDirectories(packagePath);
						}
					}
					else {
						if (debug) System.out.println("Deleting unreferenced module source directory " + child.getPath());
						deleteDirectory(child.toPath());
					}
				}
			}
		}

		// delete the generated test directory
		final Path generatedTestDirectory = Paths.get(generatedTestPath);
		if (Files.exists(generatedTestDirectory)) {
			if (debug) System.out.println("Deleting generated test directory " + generatedTestDirectory.toString());
			deleteDirectory(generatedTestDirectory);
		}

		// create files and disassemble the generation map
		Iterator<Map.Entry<Path, CharSequence>> i = generation.entrySet().iterator();
		while (i.hasNext()) {
			Map.Entry<Path, CharSequence> entry = i.next();
			Path path = entry.getKey();
			Files.createDirectories(path.getParent());
			try (BufferedWriter bw = Files.newBufferedWriter(path, StandardCharsets.UTF_8, StandardOpenOption.CREATE_NEW)) {
				bw.append(entry.getValue());
			}
			i.remove();
		}
	}
}
