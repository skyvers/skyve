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
import java.nio.file.LinkOption;
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
import java.util.stream.Stream;

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
import org.skyve.impl.metadata.model.document.InverseMany;
import org.skyve.impl.metadata.model.document.OrderedAttribute;
import org.skyve.impl.metadata.model.document.field.Enumeration;
import org.skyve.impl.metadata.model.document.field.Enumeration.EnumeratedValue;
import org.skyve.impl.metadata.model.document.field.Field;
import org.skyve.impl.metadata.model.document.field.Field.GeneratedType;
import org.skyve.impl.metadata.model.document.field.Field.IndexType;
import org.skyve.impl.metadata.model.document.field.LengthField;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.Ordering;
import org.skyve.metadata.SortDirection;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.customer.CustomerRole;
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
import org.skyve.metadata.model.document.Condition;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.Interface;
import org.skyve.metadata.model.document.Inverse;
import org.skyve.metadata.model.document.Reference;
import org.skyve.metadata.model.document.Reference.ReferenceType;
import org.skyve.metadata.model.document.Relation;
import org.skyve.metadata.module.JobMetaData;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.BizQLDefinition;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.metadata.module.query.QueryDefinition;
import org.skyve.metadata.module.query.SQLDefinition;
import org.skyve.metadata.user.Role;
import org.skyve.metadata.module.Module.DocumentRef;
import org.skyve.metadata.repository.ProvidedRepository;
import org.skyve.util.logging.SkyveLoggerFactory;
import org.skyve.util.test.SkyveFactory;
import org.slf4j.Logger;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Run through all vanilla modules and create the base class data structure and the extensions (if required).
 * Run through each customer and generate impls and package hibernate.cfg.xmls.
 * Constrain base classes.
 * Generate base classes.
 */
@SuppressWarnings("java:S1192") // Repeated literals are deliberate fragments of generated source and mapping output.
public final class OverridableDomainGenerator extends DomainGenerator {
    private static final Logger LOGGER = SkyveLoggerFactory.getLogger(OverridableDomainGenerator.class);

	/**
	 * The suffix of the concrete vanilla class generated when a customer override leaves out a vanilla
	 * attribute and so makes the vanilla domain class abstract - <code>modules.&lt;m&gt;.domain.&lt;Document&gt;Vanilla</code>.
	 * {@link DocumentImpl#getBeanClass} relies on it.
	 */
	private static final String VANILLA_CLASS_SUFFIX = "Vanilla";

	/**
	 * The suffix of the class generated for a customer's override of a vanilla document -
	 * <code>customers.&lt;c&gt;.modules.&lt;m&gt;.domain.&lt;Document&gt;Override</code>.
	 * {@link DocumentImpl#getBeanClass} relies on it.
	 */
	private static final String OVERRIDE_CLASS_SUFFIX = "Override";

	/**
	 * Simple container for generated document class metadata.
	 */
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

	@SuppressWarnings("java:S107") // too many parameters
	OverridableDomainGenerator(boolean write,
								boolean debug,
								boolean multiTenant,
								@Nonnull DialectOptions dialectOptions,
								@Nonnull String srcPath,
								@Nonnull String generatedSrcPath,
								@Nonnull String testPath,
								@Nonnull String generatedTestPath,
								@Nullable String[] excludedModules) {
		super(write, debug, multiTenant, dialectOptions, srcPath, generatedSrcPath, testPath, generatedTestPath, excludedModules);
	}

	@Override
	public void generate() throws Exception {
		populateDataStructures();

		// generate the domain classes for vanilla modules
		for (String moduleName : repository.getAllVanillaModuleNames()) {
			Module module = getRepositoryModule(null, moduleName);
			generateVanilla(module);
		}

		// generate for customer overrides
		for (String customerName : repository.getAllCustomerNames()) {
			Customer customer = repository.getCustomer(customerName);
			if (customer == null) {
				throw new MetaDataException("Customer " + customerName + " does not exist.");
			}

			// generate the metadata enums of each module this customer overrides from its effective module
			for (Module module : customer.getModules()) {
				String moduleName = module.getName();
				String moduleKey = repository.vtable(customerName, ProvidedRepository.MODULES_NAMESPACE + moduleName);
				if ((moduleKey != null) && moduleKey.startsWith(ProvidedRepository.CUSTOMERS_NAMESPACE)) {
					String packagePath = new StringBuilder(128).append(ProvidedRepository.CUSTOMERS_NAMESPACE)
																.append(customerName)
																.append('/')
																.append(ProvidedRepository.MODULES_NAMESPACE)
																.append(moduleName)
																.append('/')
																.append(ProvidedRepository.DOMAIN_NAME).toString();
					generateModuleMetadataEnums(module, customerName, packagePath);
				}
			}

			// generate the customer role enum
			generateCustomerRoleEnum(customer);

			String modulesPath = new StringBuilder(256).append(generatedSrcPath)
														.append(ProvidedRepository.CUSTOMERS_NAMESPACE)
														.append(customerName)
														.append('/')
														.append(ProvidedRepository.MODULES_NAME)
														.append('/').toString();
			generateOverridden(customer, modulesPath);
			visitedOverriddenORMDocumentsPerCustomer.clear();
			generateOverridden(customer, modulesPath);

			// clear out overrides as we have finished with this customer
			overriddenORMDocumentsPerCustomer.clear();
			visitedOverriddenORMDocumentsPerCustomer.clear();
		}

		// delete generated and generatedTest directories
		if (write) {
			replaceGenerated(repository.getAllVanillaModuleNames());
		}
	}

	/**
	 * Build vanilla document metadata, then refine based on customer overrides.
	 */
	@SuppressWarnings({"java:S3776", "java:S112"}) // complexity OK
	private void populateDataStructures() throws Exception {
		// Populate Base Data Structure with Vanilla definitions
		for (String moduleName : repository.getAllVanillaModuleNames()) {
			final Module module = getRepositoryModule(null, moduleName);

			// Populate the properties map
			final TreeMap<String, DomainClass> documentClasses = new TreeMap<>();
			moduleDocumentVanillaClasses.put(moduleName, documentClasses);

			new ModuleDocumentVisitor() {
				@Override
				public void accept(@Nonnull Document document) throws Exception {
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
			if (customer == null) {
				throw new MetaDataException("Customer " + customerName + " does not exist.");
			}
			for (final Module module : customer.getModules()) {
				final String moduleName = module.getName();
				final TreeMap<String, DomainClass> vanillaDocumentClasses = moduleDocumentVanillaClasses.get(moduleName);

				new ModuleDocumentVisitor() {
					@Override
					public void accept(@Nonnull Document document) throws Exception {
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

	/**
	 * Populate derived document mappings and validate extension strategies.
	 */
	@SuppressWarnings({"java:S6541", "java:S3776", "java:S2583"}) // complexity OK
	private void populateModocDerivations(@Nonnull Module module,
											@Nonnull Document document,
											@Nullable ExtensionStrategy strategyToAssert) {
		Extends inherits = document.getExtends();
		Persistent persistent = document.getPersistent();
		ExtensionStrategy strategy = (persistent == null) ? null : persistent.getStrategy();
		boolean mapped = ExtensionStrategy.mapped.equals(strategy);
		Document baseDocument = (inherits != null) ? module.getDocument(null, inherits.getDocumentName()) : null;

		if ((baseDocument != null) && baseDocument.isDynamic() && (! document.isDynamic())) {
			throw new MetaDataException("Document " + document.getName() + " cannot extend dynamic document " + baseDocument.getName() + " as it is not a dynamic document");
		}
		
		if ((persistent != null) &&
				(strategyToAssert != null) &&
				(! mapped) &&
				(! strategyToAssert.equals(strategy))) {
			throw new MetaDataException("Document " + document.getName() +
											((strategy == null) ? " has no extension strategy" : " uses extension strategy " + strategy) +
											" which conflicts with other extensions in the hierarchy using strategy " + strategyToAssert);
		}

		if ((inherits != null) && (persistent != null)) {
			if (baseDocument == null) {
				throw new MetaDataException("Document " + document.getName() +
												" extends document " + inherits.getDocumentName() +
												" which does not exist in module " + module.getName());
			}

			Persistent basePersistent = baseDocument.getPersistent();
			ExtensionStrategy baseStrategy = (basePersistent == null) ? null : basePersistent.getStrategy();
			boolean baseMapped = ExtensionStrategy.mapped.equals(baseStrategy);

			String persistentIdentifier = persistent.getPersistentIdentifier();
			if ((persistentIdentifier == null) && (! mapped)) {
				throw new MetaDataException("Document " + document.getName() + " cannot be transient when it is extending document " + baseDocument.getName());
			}
			if ((basePersistent == null) || (basePersistent.getName() == null)) {
				if (! baseMapped) {
					throw new MetaDataException("Document " + document.getName() + " cannot extend transient document " + baseDocument.getName());
				}
			}

			if (ExtensionStrategy.joined.equals(strategy)) {
				Document baseJoinedDocument = baseDocument;
				Persistent baseJoinedPersistent = basePersistent;
				if (baseMapped) {
					baseJoinedDocument = repository.findNearestPersistentSingleOrJoinedSuperDocument(null, module, document);
					baseJoinedPersistent = (baseJoinedDocument == null) ? null : baseJoinedDocument.getPersistent();
				}
				if ((baseJoinedDocument != null) &&
						(persistentIdentifier != null) &&
						persistentIdentifier.equals((baseJoinedPersistent == null) ? 
														null : 
														baseJoinedPersistent.getPersistentIdentifier())) {
					throw new MetaDataException("Document " + document.getName() + " extends document " +
													baseJoinedDocument.getName() + " with a strategy of " + strategy +
													" but the persistent identifiers are the same.");
				}
			}
			if (ExtensionStrategy.single.equals(strategy)) {
				Document baseSingleDocument = baseDocument;
				Persistent baseSinglePersistent = basePersistent;
				if (baseMapped) {
					baseSingleDocument = repository.findNearestPersistentSingleOrJoinedSuperDocument(null, module, document);
					baseSinglePersistent = (baseSingleDocument == null) ? null : baseSingleDocument.getPersistent();
				}
				if ((baseSingleDocument != null) &&
						(persistentIdentifier != null) &&
						(! persistentIdentifier.equals((baseSinglePersistent == null) ? 
															null : 
															baseSinglePersistent.getPersistentIdentifier()))) {
					throw new MetaDataException("Document " + document.getName() + " extends document " +
													baseSingleDocument.getName() + " with a strategy of " + strategy +
													" but the persistent identifiers are different.");
				}
			}

			putModocDerivation(document, baseDocument);
			Module baseModule = getRepositoryModule(null, baseDocument.getOwningModuleName());
			populateModocDerivations(baseModule,
										baseDocument,
										(strategyToAssert != null) ? 
											strategyToAssert : 
											(ExtensionStrategy.mapped.equals(strategy) ? null : strategy));
		}
	}

	/**
	 * Register a document as a derivation of a base document.
	 */
	private void putModocDerivation(@Nonnull Document document, @Nonnull Document baseDocument) {
		String baseModoc = baseDocument.getOwningModuleName() + '.' + baseDocument.getName();
		TreeMap<String, Document> derivations = modocDerivations.get(baseModoc);
		if (derivations == null) {
			derivations = new TreeMap<>();
			modocDerivations.put(baseModoc, derivations);
		}
		derivations.put(document.getOwningModuleName() + '.' + document.getName(), document);
	}

	/**
	 * Generate base domain classes and ORM mapping for a vanilla module.
	 */
	@SuppressWarnings({"java:S3776", "java:S112"}) // complexity OK
	private void generateVanilla(final @Nonnull Module module) throws Exception {
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
	    LOGGER.debug("Mapping file is {}", mappingFilePath);

		final StringBuilder filterDefinitions = new StringBuilder(1024);
		StringBuilder mappingFileContents = new StringBuilder(4096);
		createMappingFileHeader(mappingFileContents);

		new ModuleDocumentVisitor() {
			@Override
			public void accept(@Nonnull Document document) throws Exception {
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
					// Generate the concrete vanilla class
					String vanillaClassName = documentName + VANILLA_CLASS_SUFFIX;
					checkGeneratedClassName(module, documentName, vanillaClassName);
					javaFilePath = Paths.get(generatedSrcPath, packagePath, vanillaClassName + ".java");
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
						(! Arrays.asList(excludedModules).contains(moduleName))) {
					// check if this document is annotated to skip domain tests
					File factoryFile = new File(new StringBuilder(256).append(srcPath)
																		.append(modulePath)
																		.append('/')
																		.append(documentName)
																		.append('/')
																		.append(documentName)
																		.append("Factory.java").toString());
					SkyveFactory annotation = retrieveFactoryAnnotation(factoryFile);

					// generate domain test for persistent documents that are persistable and not children
					if (document.isPersistable() && (document.getParentDocumentName() == null)) {
						boolean skipGeneration = false;

						if (annotation != null && !annotation.testDomain()) {
							skipGeneration = true;
						}

						if (! skipGeneration) {
							Path testFilePath = Paths.get(domainTestPath.toString(), documentName + "Test.java");
							// don't generate a test if the developer has created a domain test in this location in the test directory
							if (testAlreadyExists(testFilePath)) {
								if (debug) {
									LOGGER.info(new StringBuilder(256).append("Skipping domain test generation for ")
																				.append(packagePath.replaceAll("\\\\|\\/", "."))
																				.append('.')
																				.append(documentName)
																				.append(", file already exists in ")
																				.append(testPath).toString());
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
								LOGGER.info(new StringBuilder(256).append("Skipping domain test generation for ")
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

		generateModuleMetadataEnums(module, null, packagePath);
	}

	/**
	 * Generate one enum per kind of module metadata (<code>&lt;Module&gt;Role</code>,
	 * <code>&lt;Module&gt;Document</code>, <code>&lt;Module&gt;Query</code>,
	 * <code>&lt;Module&gt;BizQL</code>, <code>&lt;Module&gt;SQL</code> and
	 * <code>&lt;Module&gt;Job</code>) in the given domain package, each implementing the
	 * corresponding <code>org.skyve.metadata</code> reference interface so a single enum constant
	 * carries the module name and metadata name (e.g. <code>user.isInRole(AdminRole.AUDIT_MANAGER)</code>)
	 * and resolves itself through the current customer via the interface's default method. The
	 * module's queries are split by type: metadata queries, BizQL and SQL each get their own enum.
	 * An empty kind generates no enum.
	 * <p/>
	 * For a customer override the module is the customer's effective module - the override replaces
	 * the vanilla module definition, so the enums list exactly the metadata that resolves for that
	 * customer - and the package is the customer's module domain package.
	 *
	 * @param module	The module to generate metadata enums for - vanilla, or the customer's override.
	 * @param customerName	The customer whose module override this is, or null for a vanilla module.
	 * @param packagePath	The domain package path (relative, '/' separated) to generate into.
	 */
	private void generateModuleMetadataEnums(final @Nonnull Module module,
												final @Nullable String customerName,
												final @Nonnull String packagePath) {
		generateModuleMetadataEnum(module, customerName, packagePath, "Role", "role", "roles",
									"org.skyve.metadata.user.ModuleRole", "roleName",
									module.getRoles().stream().map(Role::getName).toList());
		generateModuleMetadataEnum(module, customerName, packagePath, "Document", "document", "documents",
									"org.skyve.metadata.model.document.ModuleDocument", "documentName",
									new ArrayList<>(module.getDocumentRefs().keySet()));

		List<String> metaDataQueryNames = new ArrayList<>();
		List<String> bizQLNames = new ArrayList<>();
		List<String> sqlNames = new ArrayList<>();
		for (QueryDefinition query : module.getMetadataQueries()) {
			if (query instanceof MetaDataQueryDefinition) {
				metaDataQueryNames.add(query.getName());
			}
			else if (query instanceof BizQLDefinition) {
				bizQLNames.add(query.getName());
			}
			else if (query instanceof SQLDefinition) {
				sqlNames.add(query.getName());
			}
		}
		generateModuleMetadataEnum(module, customerName, packagePath, "Query", "metadata query", "metadata queries",
									"org.skyve.metadata.module.query.ModuleQuery", "queryName",
									metaDataQueryNames);
		generateModuleMetadataEnum(module, customerName, packagePath, "BizQL", "BizQL query", "BizQL queries",
									"org.skyve.metadata.module.query.ModuleBizQL", "queryName",
									bizQLNames);
		generateModuleMetadataEnum(module, customerName, packagePath, "SQL", "SQL query", "SQL queries",
									"org.skyve.metadata.module.query.ModuleSQL", "queryName",
									sqlNames);

		generateModuleMetadataEnum(module, customerName, packagePath, "Job", "job", "jobs",
									"org.skyve.metadata.module.ModuleJob", "jobName",
									module.getJobs().stream().map(JobMetaData::getName).toList());
	}

	/**
	 * Generate one module metadata reference enum (e.g. <code>AdminRole</code>) with a constant
	 * per metadata name, implementing the given reference interface. The enum class name dodges
	 * the module's document class names case-insensitively, mirroring the module metadata
	 * factory class. Nothing is generated when there are no names.
	 *
	 * @param module	The module to generate the enum for - vanilla, or the customer's override.
	 * @param customerName	The customer whose module override this is, or null for a vanilla module.
	 * @param packagePath	The domain package path (relative, '/' separated) to generate into.
	 * @param kindSuffix	The enum class name suffix - "Role", "Document", "Query", "BizQL", "SQL" or "Job".
	 * @param kind	The singular metadata kind for javadoc and error messages - e.g. "role".
	 * @param kindPlural	The plural metadata kind for javadoc - e.g. "roles", "metadata queries".
	 * @param interfaceClassName	The fully qualified reference interface the enum implements.
	 * @param nameAccessor	The interface's name accessor method - e.g. "roleName".
	 * @param names	The metadata names to generate enum constants for.
	 * @throws MetaDataException	if two names yield the same Java enum constant name.
	 */
	@SuppressWarnings("java:S107") // too many parameters
	private void generateModuleMetadataEnum(final @Nonnull Module module,
											final @Nullable String customerName,
											final @Nonnull String packagePath,
											final @Nonnull String kindSuffix,
											final @Nonnull String kind,
											final @Nonnull String kindPlural,
											final @Nonnull String interfaceClassName,
											final @Nonnull String nameAccessor,
											final @Nonnull List<String> names) {
		if (names.isEmpty()) {
			return;
		}

		final String moduleName = module.getName();

		// Dodge document class names in the same generated package - e.g. a module could declare a
		// document named <Module>Role. The comparison is case-insensitive because the generated
		// files must coexist on case-insensitive file systems (macOS, Windows).
		String enumName = BindUtil.toJavaTypeIdentifier(moduleName) + kindSuffix;
		while (containsIgnoreCase(module.getDocumentRefs().keySet(), enumName)) {
			enumName += kindSuffix;
		}

		StringBuilder javadoc = new StringBuilder(256);
		javadoc.append(" * Compile-time references to the ").append(kindPlural).append(" declared in the ")
				.append(moduleName).append(" module");
		if (customerName != null) {
			javadoc.append(" for customer ").append(customerName);
		}
		javadoc.append(".\n");
		if ("Document".equals(kindSuffix)) {
			javadoc.append(" * Documents the module references from other modules are included and are addressed through this module.\n");
		}

		generateMetadataEnum(packagePath,
								enumName,
								javadoc,
								interfaceClassName,
								"Module",
								"moduleName",
								moduleName,
								nameAccessor,
								kind,
								names);
	}

	/**
	 * Generate the customer role reference enum (<code>&lt;Customer&gt;CustomerRole</code>) in the
	 * customer's domain package (<code>customers.&lt;c&gt;.domain</code>) with a constant per customer role
	 * declared in <code>customer.xml</code>, implementing
	 * {@link org.skyve.metadata.customer.CustomerRoleReference}. Nothing is generated when the customer
	 * declares no roles.
	 *
	 * @param customer	The customer to generate the enum for.
	 * @throws MetaDataException	if two role names yield the same Java enum constant name.
	 */
	private void generateCustomerRoleEnum(final @Nonnull Customer customer) {
		final List<String> names = new ArrayList<>();
		for (CustomerRole role : customer.getRoles()) {
			names.add(role.getName());
		}
		if (names.isEmpty()) {
			return;
		}

		final String customerName = customer.getName();

		generateMetadataEnum(new StringBuilder(64).append(ProvidedRepository.CUSTOMERS_NAMESPACE)
													.append(customerName)
													.append('/')
													.append(ProvidedRepository.DOMAIN_NAME).toString(),
								BindUtil.toJavaTypeIdentifier(customerName) + "CustomerRole",
								new StringBuilder(128).append(" * Compile-time references to the customer roles declared for customer ")
														.append(customerName)
														.append(".\n"),
								"org.skyve.metadata.customer.CustomerRoleReference",
								"Customer",
								"customerName",
								customerName,
								"roleName",
								"customer role",
								names);
	}

	/**
	 * Generate a metadata reference enum with a constant per metadata name, implementing the given
	 * reference interface with an owner accessor returning the owner's name and a name accessor
	 * returning the constant's metadata name.
	 *
	 * @param packagePath	The package path (relative, '/' separated) to generate into.
	 * @param enumName	The enum class name.
	 * @param javadoc	The class javadoc summary lines, each starting " * " and ending with a newline.
	 * @param interfaceClassName	The fully qualified reference interface the enum implements.
	 * @param ownerKind	The owner kind for error messages - "Module" or "Customer".
	 * @param ownerAccessor	The interface's owner name accessor method - e.g. "moduleName".
	 * @param ownerName	The name of the module or customer that declares the metadata.
	 * @param nameAccessor	The interface's name accessor method - e.g. "roleName".
	 * @param kind	The singular metadata kind for javadoc and error messages - e.g. "role".
	 * @param names	The metadata names to generate enum constants for; not empty.
	 * @throws MetaDataException	if two names yield the same Java enum constant name.
	 */
	@SuppressWarnings("java:S107") // too many parameters
	private void generateMetadataEnum(final @Nonnull String packagePath,
										final @Nonnull String enumName,
										final @Nonnull CharSequence javadoc,
										final @Nonnull String interfaceClassName,
										final @Nonnull String ownerKind,
										final @Nonnull String ownerAccessor,
										final @Nonnull String ownerName,
										final @Nonnull String nameAccessor,
										final @Nonnull String kind,
										final @Nonnull List<String> names) {
		final String packageName = packagePath.replaceAll("\\\\|\\/", ".");
		final String interfaceSimpleName = interfaceClassName.substring(interfaceClassName.lastIndexOf('.') + 1);

		StringBuilder contents = new StringBuilder(2048);
		contents.append("package ").append(packageName).append(";\n\n");

		Set<String> imports = new TreeSet<>();
		imports.add("jakarta.annotation.Generated");
		imports.add(interfaceClassName);

		// generate imports
		for (String importClassName : imports) {
			contents.append("import ").append(importClassName).append(";\n");
		}

		// generate javadoc
		contents.append("\n/**\n");
		contents.append(javadoc);
		contents.append(" * Generated - local changes will be overwritten.\n");
		contents.append(" */\n");
		contents.append("@Generated(value = \"").append(getClass().getName()).append("\")\n");
		contents.append("public enum ").append(enumName).append(" implements ").append(interfaceSimpleName).append(" {\n");

		Set<String> constantNames = new TreeSet<>();
		boolean first = true;
		for (String name : names) {
			String constantName = BindUtil.toJavaStaticIdentifier(name);
			if (! constantNames.add(constantName)) {
				throw new MetaDataException(ownerKind + ' ' + ownerName + ' ' + kind + ' ' + name +
												" yields the duplicate Java enum constant name " + constantName +
												" in the generated " + enumName + " enum. Rename the " + kind + '.');
			}
			if (! first) {
				contents.append(",\n");
			}
			first = false;
			String literal = javaStringLiteral(name);
			contents.append("\t/** The \"").append(literal).append("\" ").append(kind).append(". */\n");
			contents.append("\t").append(constantName).append("(\"").append(literal).append("\")");
		}
		contents.append(";\n");

		contents.append("\n\tprivate final String name;\n");
		contents.append("\n\tprivate ").append(enumName).append("(String name) {\n");
		contents.append("\t\tthis.name = name;\n");
		contents.append("\t}\n");

		contents.append("\n\t@Override\n");
		contents.append("\tpublic String ").append(ownerAccessor).append("() {\n");
		contents.append("\t\treturn \"").append(javaStringLiteral(ownerName)).append("\";\n");
		contents.append("\t}\n");

		contents.append("\n\t@Override\n");
		contents.append("\tpublic String ").append(nameAccessor).append("() {\n");
		contents.append("\t\treturn name;\n");
		contents.append("\t}\n");
		contents.append("}\n");
		contents.trimToSize();

		if (write) {
			generation.put(Paths.get(generatedSrcPath, packagePath, enumName + ".java"), contents);
		}
	}

	/** Whether the set contains the candidate, compared case-insensitively. */
	private static boolean containsIgnoreCase(final Set<String> names, final String candidate) {
		return names.stream().anyMatch(candidate::equalsIgnoreCase);
	}

	/** Escapes a metadata name for emission inside a generated Java String literal. */
	private static String javaStringLiteral(final String name) {
		return name.replace("\\", "\\\\")
					.replace("\"", "\\\"")
					.replace("\n", "\\n")
					.replace("\r", "\\r")
					.replace("\t", "\\t");
	}

	private @Nullable SkyveFactory retrieveFactoryAnnotation(final @Nonnull File factoryFile) {
		SkyveFactory annotation = null;

		if (factoryFile.exists()) {
			String className = factoryFile.getPath().replaceAll("\\\\|\\/", ".")
													.replace(srcPath.replaceAll("\\\\|\\/", "."), "");

			LOGGER.debug("Found factory {}", className);
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

	@SuppressWarnings({"java:S3776", "java:S112"}) // complexity OK
	private void generateOverridden(final @Nonnull Customer customer, final @Nonnull String modulesPath)
	throws Exception {
		// Make the orm.hbm.xml file
		Path mappingFilePath = Paths.get(modulesPath + "orm.hbm.xml");
		StringBuilder mappingFileContents = new StringBuilder(2048);

		final StringBuilder filterDefinitions = new StringBuilder(1024);
		createMappingFileHeader(mappingFileContents);
		final int headerLength = mappingFileContents.length();

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
				public void accept(@Nonnull Document document) throws Exception {
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

						// bug out if this document is an override that can use the vanilla class
						if ((vanillaDocumentName != null) && (! overrideNeedsCustomerClass(document))) {
							generateOverriddenORM(mappingFileContents, customer, module, document, filterDefinitions);
							return;
						}

						// Generate the override class, or the new document's class
						StringBuilder javaFileName = new StringBuilder(256);
						javaFileName.append(generatedSrcPath).append(packagePath).append('/').append(documentName);
						if (vanillaDocumentName != null) {
							checkGeneratedClassName(module, documentName, documentName + OVERRIDE_CLASS_SUFFIX);
							javaFileName.append(OVERRIDE_CLASS_SUFFIX);
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

		// Every customer document - an override, even one without extra properties, or a new document -
		// appends its mapping, so a customer without any gets no mapping file
		if (mappingFileContents.length() > headerLength) {
			createMappingFileFooter(mappingFileContents, filterDefinitions);
			mappingFileContents.trimToSize();
			if (write) {
				generation.put(mappingFilePath, mappingFileContents);
			}
		}
	}

	/**
	 * Checks that a class generated for a document - its concrete vanilla class or a customer's override
	 * class - does not take the name of a document in the module, compared case-insensitively because
	 * the generated files must coexist on case-insensitive file systems. The generated name cannot be
	 * dodged, as {@link DocumentImpl#getBeanClass} looks the class up by it.
	 *
	 * @param module	The module whose domain package the class is generated into - vanilla, or the
	 * 					customer's override.
	 * @param documentName	The document the class is generated for.
	 * @param className	The generated class's simple name.
	 * @throws MetaDataException	if a document in the module has the class's name.
	 */
	private static void checkGeneratedClassName(@Nonnull Module module, @Nonnull String documentName, @Nonnull String className) {
		for (String name : module.getDocumentRefs().keySet()) {
			if (name.equalsIgnoreCase(className)) {
				throw new MetaDataException("Document " + name + " in module " + module.getName() +
												" has the name of the class " + className + " generated for document " + documentName +
												". Rename the document.");
			}
		}
	}

	/**
	 * Determines whether a customer's override of a vanilla document needs its own concrete customer
	 * class (<code>customers.&lt;c&gt;.modules.&lt;m&gt;.domain.&lt;Document&gt;Override</code>) rather than the
	 * vanilla class: when it adds properties to the vanilla class, or when the vanilla class is abstract
	 * because a customer's override - this one or another customer's - leaves out a vanilla attribute.
	 *
	 * @param overriddenDocument	The customer's override of a vanilla document.
	 * @return	true if the override needs a customer class.
	 */
	private boolean overrideNeedsCustomerClass(@Nonnull Document overriddenDocument) {
		DomainClass vanillaClass = moduleDocumentVanillaClasses.get(overriddenDocument.getOwningModuleName()).get(overriddenDocument.getName());
		return vanillaClass.isAbstract || (! getOverriddenDocumentExtraProperties(overriddenDocument).isEmpty());
	}

	private @Nonnull TreeMap<String, AttributeType> getOverriddenDocumentExtraProperties(@Nonnull Document overriddenDocument) {
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

	private static void createMappingFileHeader(@Nonnull StringBuilder contents) {
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

	private static void createMappingFileFooter(@Nonnull StringBuilder contents, @Nonnull StringBuilder filterDefinitions) {
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
	private void generateORM(@Nonnull StringBuilder contents,
								@Nonnull Module module,
								@Nonnull Document document,
								@Nonnull String packagePathPrefix,
								boolean forExt,
								@Nullable Customer customer,
								@Nonnull StringBuilder filterDefinitions) {
		generateORM(contents, module, document, packagePathPrefix, forExt, false, customer, filterDefinitions, "");
	}

	@SuppressWarnings({"java:S3776", "java:S6541", "java:S107"}) // complexity OK
	private void generateORM(@Nonnull StringBuilder contents,
								@Nonnull Module module,
								@Nonnull Document document,
								@Nonnull String packagePathPrefix,
								boolean forExt, // indicates if we are processing a customer override
								boolean recursive,
								@Nullable Customer customer,
								@Nonnull StringBuilder filterDefinitions,
								@Nonnull String indentation) { // indents subclass definitions within the class definition
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
		if (repository.findNearestPersistentSingleOrJoinedSuperDocument(null, module, document) == null) { // not an ORM subclass
			baseDocumentName = null;
		}
		String indent = indentation;
		ExtensionStrategy strategy = (persistent == null) ? null : persistent.getStrategy();
		if (recursive && ExtensionStrategy.mapped.equals(strategy)) {
			indent = indentation.substring(0, indentation.length() - 1);
		}

		String entityName = null;

		if (debug) {
			LOGGER.info("Generate ORM for {}{}.{}.{}", packagePathPrefix, moduleName, ProvidedRepository.DOMAIN_NAME, documentName);
		}

		// class defn
		if ((persistent != null) && document.isPersistable()) { // persistent document
			String persistentName = persistent.getName();
			if (persistentName == null) { // cannot happen as the document is persistable
				throw new IllegalStateException("Persistable document " + moduleName + '.' + documentName + " has no persistent name");
			}
			// check table name length if required
			if (identifierIsTooLong(persistentName)) {
				throw new MetaDataException("Persistent name " + persistentName + 
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
					if (debug) {
						LOGGER.info("    Generate ORM using {}", extensionPath);
					}
					contents.append(packagePathPrefix).append(moduleName).append('.').append(documentName).append('.').append(documentName).append("Extension");
				}
				else {
					contents.append(packagePathPrefix).append(moduleName).append('.').append(ProvidedRepository.DOMAIN_NAME).append('.').append(documentName);
					if (forExt) {
						contents.append(packagePathPrefix.startsWith(ProvidedRepository.CUSTOMERS_NAME) ? OVERRIDE_CLASS_SUFFIX : VANILLA_CLASS_SUFFIX);
					}
				}
				contents.append("\" ");
			}
			
			if ((baseDocumentName == null) || ExtensionStrategy.joined.equals(strategy)) {
				contents.append("table=\"").append(persistentName);
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
					contents.append(generateDataStoreName(DataStoreType.IDX, persistentName, Bean.BIZ_KEY));
				}
				contents.append("\" not-null=\"true\" />\n");

				contents.append(indent).append("\t\t<property name=\"").append(Bean.CUSTOMER_NAME).append("\" length=\"50");
				if (multiTenant && shouldIndex) {
					contents.append("\" index=\"");
					contents.append(generateDataStoreName(DataStoreType.IDX, persistentName, Bean.CUSTOMER_NAME));
				}
				contents.append("\" not-null=\"true\" />\n");

				contents.append(indent).append("\t\t<property name=\"bizFlagComment\" length=\"1024\" />\n");
				contents.append(indent).append("\t\t<property name=\"bizDataGroupId\" length=\"36\" />\n");
				if (shouldIndex) {
					contents.append(indent).append("\t\t<property name=\"").append(Bean.USER_ID).append("\" length=\"36\" index=\"");
					contents.append(generateDataStoreName(DataStoreType.IDX, persistentName, Bean.USER_ID)).append("\" not-null=\"true\" />\n");
				}
				else {
					contents.append(indent).append("\t\t<property name=\"").append(Bean.USER_ID).append("\" length=\"36\" not-null=\"true\" />\n");
				}
			}

			// map the parent property, if parent document is persistent
			String parentDocumentName = document.getParentDocumentName();
			if (parentDocumentName != null) {
				Document parentDocument = document.getParentDocument(null);
				if ((parentDocument != null) && (parentDocument.getPersistent() != null)) {
					if (parentDocumentName.equals(documentName)) { // hierarchical
						contents.append(indent).append("\t\t<property name=\"").append(HierarchicalBean.PARENT_ID).append("\" length=\"36\"");
						if (shouldIndex(((DocumentImpl) document).getParentDatabaseIndex())) {
							contents.append(" index=\"").append(generateDataStoreName(DataStoreType.IDX, persistentName, HierarchicalBean.PARENT_ID)).append('"');
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
							contents.append(" index=\"").append(generateDataStoreName(DataStoreType.IDX, persistentName, ChildBean.PARENT_NAME)).append('"');
						}
						contents.append(" foreign-key=\"");
						contents.append(generateDataStoreName(DataStoreType.FK, persistentName, ChildBean.PARENT_NAME));
						contents.append("\" />\n");
					}
				}
			}

			generateAttributeMappings(contents, customer, module, document, persistent, null, new TreeSet<>(), null, forExt, indent);
		}

		TreeMap<String, Document> derivations = modocDerivations.get(moduleName + '.' + documentName);
		if (derivations != null) {
			// Do subclasses before joined-subclasses
			for (Document derivation : derivations.values()) {
				Persistent derivationPersistent = derivation.getPersistent();
				ExtensionStrategy derivationStrategy = (derivationPersistent == null) ? null : derivationPersistent.getStrategy();
				if (ExtensionStrategy.single.equals(derivationStrategy)) {
					Module derivedModule = getRepositoryModule(customer, derivation.getOwningModuleName());
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
					Module derivedModule = getRepositoryModule(customer, derivation.getOwningModuleName());
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
				if (entityName == null) { // cannot happen as the entity name is set for a persistable base class above
					throw new IllegalStateException("No entity name was generated for document " + moduleName + '.' + documentName);
				}
				generateFilterStuff(entityName, contents, filterDefinitions, indent);
				contents.append(indent).append("\t</class>\n\n");
			}
		}
		
		if (derivations != null) {
			// Take care of subclasses with no derivation strategy (persistent subclass with only mapped superclasses)
			for (Document derivation : derivations.values()) {
				Persistent derivationPersistent = derivation.getPersistent();
				ExtensionStrategy derivationStrategy = (derivationPersistent == null) ? null : derivationPersistent.getStrategy();
				if ((derivationStrategy == null) || ExtensionStrategy.mapped.equals(derivationStrategy)) {
					Module derivedModule = getRepositoryModule(customer, derivation.getOwningModuleName());
					generateORM(contents,
									derivedModule,
									derivation,
									packagePathPrefix,
									forExt,
									true,
									customer,
									filterDefinitions,
									indent);
				}
			}
		}
	}

	@SuppressWarnings({"java:S107", "java:S3776", "java:S6541"}) // Long parameter list preserves the existing framework/API contract; complexity OK
	private void generateAttributeMappings(@Nonnull StringBuilder contents,
											@Nullable Customer customer,
											@Nonnull Module module,
											@Nonnull Document document,
											@Nonnull Persistent persistent,
											@Nullable String columnPrefix,
											@Nonnull Set<String> columnNames,
											@Nullable String owningDocumentName,
											boolean forExt,
											@Nonnull String indentation) {
		Extends inherits = document.getExtends();
		if (inherits != null) {
			Document baseDocument = module.getDocument(customer, inherits.getDocumentName());
			Persistent basePersistent = baseDocument.getPersistent();
			if (basePersistent != null) {
				ExtensionStrategy strategy = basePersistent.getStrategy();
				if (ExtensionStrategy.mapped.equals(strategy)) {
					Module baseModule = getRepositoryModule(customer, baseDocument.getOwningModuleName());
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
		}

		String customerName = (customer == null) ? null : customer.getName();
		String moduleName = module.getName();
		String documentName = document.getName();

		// map the document defined properties
		for (Attribute attribute : document.getAttributes()) {
			if (attribute instanceof Collection collection) {
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

				String orderBy = orderBy((CollectionImpl) collection, referencedDocument);
				
				CollectionType type = collection.getType();
				boolean polymorphicallyMapped = referencedPersistent.isPolymorphicallyMapped();

				if (type == CollectionType.child) {
					if (polymorphicallyMapped) {
						throw new MetaDataException("Collection " + collectionName + 
														" in document " + moduleName + '.' + documentName + " referencing document " +
														referencedDocument.getOwningModuleName() + '.' + referencedDocumentName +
														" cannot be a child collection as the target document is a mapped polymorphic document." +
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

					if (polymorphicallyMapped) {
						contents.append(indentation).append("\t\t\t<many-to-any meta-type=\"string\" id-type=\"string\">\n");
						Map<String, Document> arcs = new TreeMap<>();
						populateArcs(referencedDocument, arcs);
						for (Entry<String, Document> entry : arcs.entrySet()) {
							Document derivedDocument = entry.getValue();
							String derivedModuleName = derivedDocument.getOwningModuleName();
							String derivedDocumentName = derivedDocument.getName();

							contents.append(indentation).append("\t\t\t\t<meta-value value=\"").append(entry.getKey());
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
						contents.append(indentation).append("\t\t\t\t<column name=\"");
						contents.append(collectionName).append("_type\"");
						if (! Boolean.FALSE.equals(collection.getElementDatabaseIndex())) {
							contents.append(" index=\"");
							contents.append(generateDataStoreName(DataStoreType.IDX, collectionTableName, PersistentBean.ELEMENT_COLUMN_NAME));
							contents.append('"');
						}
						contents.append(" />\n");
						contents.append(indentation).append("\t\t\t\t<column name=\"");
						contents.append(collectionName).append("_id\" length=\"36\"");
						if (! Boolean.FALSE.equals(collection.getElementDatabaseIndex())) {
							contents.append(" index=\"");
							contents.append(generateDataStoreName(DataStoreType.IDX, collectionTableName, PersistentBean.ELEMENT_COLUMN_NAME));
							contents.append('"');
						}
						contents.append(" />\n");
						contents.append(indentation).append("\t\t\t</many-to-any>\n");
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
			else if (attribute instanceof Association association) {
				String associationName = association.getName();
				String referencedDocumentName = association.getDocumentName();
				Document referencedDocument = module.getDocument(null, referencedDocumentName);
				Persistent referencedPersistent = referencedDocument.getPersistent();
				String referencedModuleName = referencedDocument.getOwningModuleName();

				// check that persistent associations don't reference transient documents.
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
				boolean polymorphicallyMapped = referencedPersistent.isPolymorphicallyMapped();
				if (AssociationType.embedded.equals(type)) {
					if (referencedModuleName.equals(moduleName) && (referencedDocumentName.equals(documentName))) {
						throw new MetaDataException(String.format("The Association %s in document %s.%s cannot be of type 'embedded' when it references itself.", 
																	associationName, 
																	moduleName, 
																	documentName));
					}

					if (polymorphicallyMapped) {
						throw new MetaDataException(String.format("The Association %s in document %s.%s cannot be of type 'embedded' when it references a mapped polymorphic document - it requires an arc (<any/>).", 
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
					
					Module referencedModule = getRepositoryModule(customer, referencedModuleName);

					// Check that there are no relations in the embedded document
					// Note that we cannot use referencedDocument.getAllAttributes(customer); as customer can be null so we need to use the repository
					Module currentModule = referencedModule;
					Document currentDocument = referencedDocument;
					Extends currentExtends = null;
					do {
						List<? extends Attribute> attributes = currentDocument.getAttributes();
						if (attributes.stream().anyMatch(a -> (a instanceof Relation))) {
							throw new MetaDataException("Embedded association name " + associationName + 
															" in document " + documentName + 
															" in module " + moduleName +
															" references document " + referencedDocumentName +
															" with a relation (association, collection or inverse) but relations cannot be embedded");
						}
						
						currentExtends = currentDocument.getExtends();
						if (currentExtends != null) {
							currentModule = repository.getModule(customer, currentDocument.getOwningModuleName());
							if (currentModule == null) {
								throw new MetaDataException("Owning module " + currentDocument.getOwningModuleName() + " was not found");
							}
							currentDocument = currentModule.getDocument(customer, currentExtends.getDocumentName());
						}
					}
					while (currentExtends != null);

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
				else if (polymorphicallyMapped) {
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
						contents.append(generateDataStoreName(DataStoreType.IDX, getPersistentName(persistent), associationName));
					}
					contents.append("\" />\n");
					contents.append(indentation).append("\t\t\t<column name=\"");
					contents.append(columnName(moduleName, owningDocumentName, associationName, columnPrefix, "_id", columnNames)).append("\" length=\"36");
					if (! Boolean.FALSE.equals(association.getDatabaseIndex())) {
						contents.append("\" index=\"");
						contents.append(generateDataStoreName(DataStoreType.IDX, getPersistentName(persistent), associationName));
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
						contents.append(generateDataStoreName(DataStoreType.UK, getPersistentName(persistent), associationName));
					}
					else if (AssociationType.aggregation.equals(type)) {
						contents.append("\" cascade=\"persist,save-update,refresh,merge");
					}
					else {
						throw new IllegalStateException("Association type " + type + " not supported.");
					}
					if (shouldIndex(association.getDatabaseIndex())) {
						contents.append("\" index=\"");
						contents.append(generateDataStoreName(DataStoreType.IDX, getPersistentName(persistent), associationName));
					}
					contents.append("\" foreign-key=\"");
					contents.append(generateDataStoreName(DataStoreType.FK, getPersistentName(persistent), associationName));
					contents.append("\"/>\n");
				}
			}
			else if (attribute instanceof Enumeration enumeration) {
				// ignore transient attributes
				if (! attribute.isPersistent()) {
					continue;
				}

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
					contents.append(generateDataStoreName(DataStoreType.IDX, getPersistentName(persistent), enumerationName));
				}
				GeneratedType generated = enumeration.getGenerated();
				if (generated != null) {
					contents.append("\" generated=\"").append(generated.toString());
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
					if (targetDocument == null) { // cannot happen as the owning document is set when the document is converted
						throw new IllegalStateException("Enumeration " + target.getName() + " has no owning document");
					}
					boolean requiresExtension = forExt &&
													(! moduleDocumentVanillaClasses.get(targetDocument.getOwningModuleName()).get(targetDocument.getName()).attributes.containsKey(target.getName()));
					// the enumeration is declared in the override class, or in the concrete vanilla class
					if (requiresExtension && (customerName != null)) {
						contents.append(ProvidedRepository.CUSTOMERS_NAME).append('.').append(customerName).append('.');
					}
					contents.append(enumeration.getEncapsulatingClassName());
					if (requiresExtension) {
						contents.append((customerName == null) ? VANILLA_CLASS_SUFFIX : OVERRIDE_CLASS_SUFFIX);
					}
					contents.append('$').append(enumeration.toJavaIdentifier());
				}
				contents.append("</param>\n");
				contents.append(indentation).append("\t\t\t</type>\n");
				contents.append(indentation).append("\t\t</property>\n");
			}
			else if (attribute instanceof Inverse) {
				// ignore non-persistent attributes
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
				
				// Ignore non-persistent inverse document
				if (inversePersistent == null) {
					continue;
				}

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

					String orderBy = orderBy((InverseMany) inverse, inverseDocument);
					if (orderBy != null) {
						contents.append("\" order-by=\"").append(orderBy);
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
					contents.append(generateDataStoreName(DataStoreType.IDX, getPersistentName(persistent), fieldName));
				}
				GeneratedType generated = field.getGenerated();
				if (generated != null) {
					contents.append("\" generated=\"").append(generated.toString());
				}
				contents.append("\" />\n");
			}
		}
	}

	/**
	 * Determine the orderBy Statement for the collection/inverse.
	 * Add order by clause to hibernate ORM only if the bindings are simple and the ordering clause has columns.
	 * 
	 * @param ordered The collection/inverse to potentially be ordered
	 * @param referencedDocument	The collection/inverse document
	 * @return	The order by clause or null if there is no ordering or the ordering is complex.
	 */
	private static @Nullable String orderBy(@Nonnull OrderedAttribute ordered, @Nonnull Document referencedDocument) {
		List<Ordering> ordering = ordered.getOrdering();
		if ((! ordered.isComplexOrdering()) && (! ordering.isEmpty())) {
			StringBuilder result = new StringBuilder(64);
			
			for (Ordering order : ordering) {
				String byBinding = order.getBy();
				String columnName = byBinding;
				// Determine the database column name - if an association the add '_id' to make the FK column name
				Attribute byAttribute = referencedDocument.getAttribute(byBinding);
				if (byAttribute instanceof Association) {
					columnName += "_id";
				}
				result.append(columnName);
				if (SortDirection.descending.equals(order.getSort())) {
					result.append(" desc, ");
				}
				else {
					result.append(" asc, ");
				}
			}
			
			result.setLength(result.length() - 2);
			return result.toString();
		}
		
		return null;
	}
	
	// generate the appropriate column name and check it is unique
	private static @Nonnull String columnName(@Nonnull String moduleName,
												@Nullable String owningDocumentName,
												@Nonnull String attributeName,
												@Nullable String columnPrefixWithoutUnderscore,
												@Nullable String columnSuffixStartingWithUnderscore,
												@Nonnull Set<String> columnNames) {
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
	/**
	 * Returns the name of the persistent settings of a persistable document, which is the table name its attributes are mapped to.
	 *
	 * @param persistent the persistent settings of a persistable document
	 * @return the persistent name; never {@code null}
	 * @throws IllegalStateException when the persistent settings have no name
	 */
	private static @Nonnull String getPersistentName(@Nonnull Persistent persistent) {
		String result = persistent.getName();
		if (result == null) { // cannot happen as attribute mappings are only generated for persistable documents
			throw new IllegalStateException("Persistent " + persistent.getPersistentIdentifier() + " has no name");
		}
		return result;
	}

	/**
	 * Returns a module that must exist in the repository, such as a vanilla module or the owning module of a document.
	 *
	 * @param customer the customer for override resolution, or {@code null} for vanilla
	 * @param moduleName the module name
	 * @return the module; never {@code null}
	 * @throws MetaDataException when the repository cannot resolve the module
	 */
	private @Nonnull Module getRepositoryModule(@Nullable Customer customer, @Nonnull String moduleName) {
		Module result = repository.getModule(customer, moduleName);
		if (result == null) {
			throw new MetaDataException("Module " + moduleName + " was not found");
		}
		return result;
	}

	private boolean identifierIsTooLong(@Nonnull String identifier) {
		return ((dialectOptions.getDataStoreIdentifierCharacterLimit() > 0) &&
				(identifier.length() > dialectOptions.getDataStoreIdentifierCharacterLimit()));
	}
	
	private void populateArcs(@Nonnull Document document, @Nonnull Map<String, Document> result) {
		String key = new StringBuilder(32).append(document.getOwningModuleName()).append('.').append(document.getName()).toString();
		TreeMap<String, Document> derivations = modocDerivations.get(key);
		if (derivations != null) {
			for (Document derivation : derivations.values()) {
				if (derivation.isPersistable()) {
					key = new StringBuilder(32).append(derivation.getOwningModuleName()).append('.').append(derivation.getName()).toString();
					result.put(key, derivation);
				}
				populateArcs(derivation, result);
			}
		}
	}

	private static void generateFilterStuff(@Nonnull String entityName,
												@Nonnull StringBuilder contents,
												@Nonnull StringBuilder filterDefinitions,
												@Nonnull String indentation) {
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

	@SuppressWarnings("java:S3776") // Complexity OK
	private void generateOverriddenORM(@Nonnull StringBuilder contents,
										@Nonnull Customer customer,
										@Nonnull Module module,
										@Nonnull Document document,
										@Nonnull StringBuilder filterDefinitions)
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
				// map the customer class if the override has one
				if (overrideNeedsCustomerClass(document)) {
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
					if ((refModuleName == null) || (refDocumentName == null)) { // cannot happen as exported references are made from documents
						throw new IllegalStateException("Exported reference " + ref.getReferenceFieldName() + " to document " + moduleDotDocument + " has no module or document name");
					}
					//String refModuleDotDocument = refModuleName + '.' + refDocumentName;
					//System.out.println("\t" + ref.getReferenceFieldName() + " = " + refModuleDotDocument);
					Module refModule = customer.getModule(refModuleName);
					Document refDocument = refModule.getDocument(customer, refDocumentName);
					Persistent refPersistent = refDocument.getPersistent();
					if (refPersistent != null) {
						if (refPersistent.isPolymorphicallyMapped()) {
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
				Reference reference = document.getNullSafeReferenceByName(referenceName);
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

	@SuppressWarnings("java:S115") // Suppress "Constant names should comply with a naming convention" as these are not constants but enum values
	private enum DataStoreType {
		PK, FK, UK, IDX
	}
	private @Nonnull String generateDataStoreName(@Nonnull DataStoreType type, @Nonnull String tableName, @Nonnull String columnName) {
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
		    	@SuppressWarnings("java:S4790") // Just to make a unique name within the character limit - not for security purposes
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
	
	private boolean shouldIndex(@Nullable Boolean index) {
		if (index == null) {
			return dialectOptions.isDataStoreIndexForeignKeys();
		}

		return index.booleanValue();
	}
	
	private static final @Nonnull TreeMap<String, AttributeType> generateDocumentPropertyNames(@Nonnull Document document) {
		TreeMap<String, AttributeType> result = new TreeMap<>();

		for (Attribute attribute : document.getAttributes()) {
			// skip dynamic attributes
			if ((attribute instanceof Field field) && field.isDynamic()) {
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

	@SuppressWarnings("java:S3776") // Complexity OK
	private void populatePropertyLengths(@Nullable Customer customer,
											@Nonnull Module module,
											@Nonnull Document document,
											@Nullable String derivedPersistentIdentifier) {
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
				Module baseModule = getRepositoryModule(customer, baseDocument.getOwningModuleName());
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
				if (attribute instanceof LengthField lengthField) {
					length = lengthField.getLength();
				}
				else if (attribute instanceof Enumeration enumeration) {
					// Find the maximum code length
					String implementingEnumClassName = enumeration.getImplementingEnumClassName();
					if (implementingEnumClassName != null) { // hand-coded implementation
						// Load the class and find the longest code domain value
						try {
							Class<?> enumerationClass = enumeration.getImplementingType();
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

	private abstract static class ModuleDocumentVisitor {
		/**
		 * Customer can be null if visiting un-overridden documents only
		 */
		public final void visit(@Nullable Customer customer, @Nonnull Module module) throws Exception {
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

		public abstract void accept(@Nonnull Document document) throws Exception;
	}

	/**
	 * Append the enum definition and return the enum type name.
	 * 
	 * @param enumeration
	 * @param enums
	 */
	private void appendEnumDefinition(@Nonnull Enumeration enumeration, @Nonnull String typeName, @Nonnull StringBuilder enums) {
		
		attributeJavadoc(enumeration, enums);
		enums.append("\t@XmlEnum\n");
		enums.append("\t@Generated(value = \"").append(getClass().getName()).append("\")\n");
		enums.append("\tpublic enum ").append(typeName).append(" implements Enumeration {\n");
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
		enums.append("\t\tprivate static List<DomainValue> domainValues = Stream.of(values()).map(").append(typeName).append("::toDomainValue).collect(Collectors.toUnmodifiableList());\n\n");

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

		// toDomainValues
		enums.append("\t\tpublic static List<DomainValue> toDomainValues() {\n");
		enums.append("\t\t\treturn domainValues;\n");
		enums.append("\t\t}\n");
		enums.append("\t}\n\n");
	}

	@SuppressWarnings({"java:S107", "java:S3776", "java:S6541"}) // Long parameter list preserves the existing framework/API contract; complexity OK
	private void addReference(@Nonnull Reference reference,
								boolean overriddenReference,
								@Nullable Customer customer,
								@Nonnull Module owningModule,
								@Nonnull String owningDocumentName,
								boolean owningDomainExtensionClassExists,
								@Nonnull String packagePath,
								@Nonnull Set<String> imports,
								@Nonnull StringBuilder attributes,
								@Nonnull StringBuilder methods) {
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
			if (customer == null) { // cannot happen as vanilla references target vanilla documents
				throw new IllegalStateException("Reference " + name + " in document " + owningModule.getName() + '.' + owningDocumentName +
													" targets document " + referencePackageName + '.' + referenceClassName + " which has no vanilla domain class");
			}
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
				if (a instanceof AbstractInverse i) {
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
			if (deprecated) {
				methods.append("\t@Deprecated\n");
			}
			methods.append("\t@XmlElement\n");
			methods.append("\tpublic List<").append(referenceClassName).append("> get").append(methodName).append("() {\n");
			methods.append("\t\treturn ").append(name).append(";\n");
			methods.append("\t}\n\n");

			// Mapped Accessor method
			accessorJavadoc(reference, methods, true);
			if (deprecated) {
				methods.append("\t@Deprecated\n");
			}
			methods.append("\tpublic ").append(referenceClassName).append(" get").append(methodName).append("ElementById(String bizId) {\n");
			methods.append("\t\treturn getElementById(").append(name).append(", bizId);\n");
			methods.append("\t}\n\n");

			// Mapped Mutator method
			mutatorJavadoc(reference, methods, true);
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
				methods.append("\t\tif (result) {\n");
				methods.append("\t\t\telement.setParent(");
				if (owningDomainExtensionClassExists) {
					methods.append("(").append(owningDocumentName).append("Extension) ");
				}
				methods.append("this);\n");
				methods.append("\t\t}\n");
				methods.append("\t\treturn result;\n");
			}
			else if (inverse != null) {
				methods.append("\t\tboolean result = ").append(name).append(".add(element);\n");
				methods.append("\t\tif (result) {\n");
				methods.append("\t\t\telement.get").append(inverseMethodName).append("().add(");
				if (owningDomainExtensionClassExists) {
					methods.append("(").append(owningDocumentName).append("Extension) ");
				}
				methods.append("this);\n");
				methods.append("\t\t}\n");
				methods.append("\t\treturn result;\n");
			}
			else {
				methods.append("\t\treturn ").append(name).append(".add(element);\n");
			}
			methods.append("\t}\n\n");
			
			// collection indexed add
			collectionJavadoc(name, methods, true, true);
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
			if (deprecated) {
				methods.append("\t@Deprecated\n");
			}
			methods.append("\tpublic boolean remove").append(methodName);
			methods.append("Element(").append(referenceClassName).append(" element) {\n");
			if (CollectionType.child.equals(type)) {
				methods.append("\t\tboolean result = ").append(name).append(".remove(element);\n");
				methods.append("\t\tif (result) {\n");
				methods.append("\t\t\telement.setParent(null);\n");
				methods.append("\t\t}\n");
				methods.append("\t\treturn result;\n");
			}
			else if (inverse != null) {
				methods.append("\t\tboolean result = ").append(name).append(".remove(element);\n");
				methods.append("\t\tif (result) {\n");
				methods.append("\t\t\telement.get").append(inverseMethodName).append("().remove(this);\n");
				methods.append("\t\t}\n");
				methods.append("\t\treturn result;\n");
			}
			else {
				methods.append("\t\treturn ").append(name).append(".remove(element);\n");
			}
			methods.append("\t}\n\n");
			
			// collection indexed remove
			collectionJavadoc(name, methods, false, true);
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
			if (deprecated) {
				methods.append("\t@Deprecated\n");
			}
			methods.append("\tpublic ").append(referenceClassName).append(" get").append(methodName).append("() {\n");
			methods.append("\t\treturn ").append(name).append(";\n");
			methods.append("\t}\n\n");

			// Mutator method
			mutatorJavadoc(reference, methods, false);
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

	@SuppressWarnings({"java:S107", "java:S3776"}) // Long parameter list preserves the existing framework/API contract; complexity OK.
	private void addInverse(@Nonnull AbstractInverse inverse,
								boolean overriddenInverse,
								@Nullable Customer customer,
								@Nonnull Module owningModule,
								@Nonnull String owningDocumentName,
								boolean owningDomainExtensionClassExists,
								@Nonnull String packagePath,
								@Nonnull Set<String> imports,
								@Nonnull StringBuilder attributes,
								@Nonnull StringBuilder methods) {
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
			if (customer == null) { // cannot happen as vanilla inverses target vanilla documents
				throw new IllegalStateException("Inverse " + name + " in document " + owningModule.getName() + '.' + owningDocumentName +
													" targets document " + propertyPackageName + '.' + propertyClassName + " which has no vanilla domain class");
			}
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

	@SuppressWarnings({"boxing", "java:S3776"}) // Complexity OK
	private void generateActionTests(final @Nonnull String moduleName,
										final @Nonnull String packagePath,
										final @Nonnull String modulePath,
										@Nonnull Document document,
										@Nonnull String documentName,
										@Nullable SkyveFactory annotation) {
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
					if (type instanceof ParameterizedType paramType) {
						Type[] actualTypeArguments = paramType.getActualTypeArguments();
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
				skipGeneration = true;
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

	private void generateActionTest(@Nonnull StringBuilder contents,
										@Nonnull String modulePath,
										@Nonnull String actionPath,
										@Nonnull String domainPath,
										@Nonnull String documentName,
										@Nonnull String actionName,
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

	private void generateDomainTest(@Nonnull StringBuilder contents,
										@SuppressWarnings("unused") @Nonnull String modulePath,
										@Nonnull String packagePath,
										@Nonnull String documentName) {
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

	@SuppressWarnings({"java:S107", "java:S3776", "java:S6541"}) // Long parameter list preserves the existing framework/API contract; complexity OK
	private void generateJava(@Nullable Customer customer,
								@Nonnull Module module,
								@Nonnull Document document,
								@Nonnull StringBuilder contents,
								@Nonnull String packagePath,
								@Nonnull String documentName,
								@Nullable String baseDocumentName,
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
			imports.add("jakarta.xml.bind.annotation.XmlTransient");
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
		}

		for (Attribute attribute : document.getAttributes()) {
			String name = attribute.getName();
			boolean deprecated = attribute.isDeprecated();

			// skip dynamic attributes (just generate the static final property name)
			if ((attribute instanceof Field field) && field.isDynamic()) {
				statics.append("\t/** @hidden */\n");
				if (deprecated) {
					statics.append("\t@Deprecated\n");
				}
				statics.append("\tpublic static final String ").append(name).append("PropertyName = \"");
				statics.append(attribute.getName()).append("\";\n\n");
				continue;
			}

			imports.add("jakarta.xml.bind.annotation.XmlElement");

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
				Class<?> implementingType = null;
				try {
					implementingType = attribute.getImplementingType();
				}
				catch (MetaDataException e) { // thrown when enumerations haven't been generated yet
					if (! (attribute instanceof Enumeration)) {
						throw e;
					}
				}
				
				String methodName = name.substring(0, 1).toUpperCase() + name.substring(1);
				String propertySimpleClassName = null;
				if (attribute instanceof Enumeration enumeration) {
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
							imports.add("java.util.stream.Stream");
							imports.add("java.util.stream.Collectors");
							imports.add("jakarta.xml.bind.annotation.XmlEnum");
	
							appendEnumDefinition(enumeration, propertySimpleClassName, enums);
						}
					}
				}
				else if (attribute instanceof Reference reference) {
					addReference(reference,
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
				else if (implementingType != null ) {
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
					if (String.class.equals(implementingType)) {
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
					imports.add("jakarta.xml.bind.annotation.XmlSchemaType");
					imports.add("jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter");
					imports.add("org.skyve.impl.domain.types.jaxb.DateOnlyMapper");
					methods.append("\t@XmlSchemaType(name = \"date\")\n");
					methods.append("\t@XmlJavaTypeAdapter(DateOnlyMapper.class)\n");
				}
				else if (AttributeType.time.equals(type)) {
					imports.add("jakarta.xml.bind.annotation.XmlSchemaType");
					imports.add("jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter");
					imports.add("org.skyve.impl.domain.types.jaxb.TimeOnlyMapper");
					methods.append("\t@XmlSchemaType(name = \"time\")\n");
					methods.append("\t@XmlJavaTypeAdapter(TimeOnlyMapper.class)\n");
				}
				else if (AttributeType.dateTime.equals(type)) {
					imports.add("jakarta.xml.bind.annotation.XmlSchemaType");
					imports.add("jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter");
					imports.add("org.skyve.impl.domain.types.jaxb.DateTimeMapper");
					methods.append("\t@XmlSchemaType(name = \"dateTime\")\n");
					methods.append("\t@XmlJavaTypeAdapter(DateTimeMapper.class)\n");
				}
				else if (AttributeType.timestamp.equals(type)) {
					imports.add("jakarta.xml.bind.annotation.XmlSchemaType");
					imports.add("jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter");
					imports.add("org.skyve.impl.domain.types.jaxb.TimestampMapper");
					methods.append("\t@XmlSchemaType(name = \"dateTime\")\n");
					methods.append("\t@XmlJavaTypeAdapter(TimestampMapper.class)\n");
				}
				else if (AttributeType.decimal2.equals(type)) {
					imports.add("jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter");
					imports.add("org.skyve.impl.domain.types.jaxb.Decimal2Mapper");
					methods.append("\t@XmlJavaTypeAdapter(Decimal2Mapper.class)\n");
				}
				else if (AttributeType.decimal5.equals(type)) {
					imports.add("jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter");
					imports.add("org.skyve.impl.domain.types.jaxb.Decimal5Mapper");
					methods.append("\t@XmlJavaTypeAdapter(Decimal5Mapper.class)\n");
				}
				else if (AttributeType.decimal10.equals(type)) {
					imports.add("jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter");
					imports.add("org.skyve.impl.domain.types.jaxb.Decimal10Mapper");
					methods.append("\t@XmlJavaTypeAdapter(Decimal10Mapper.class)\n");
				}
				else if (AttributeType.geometry.equals(type)) {
					imports.add("jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter");
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
		String parentClassName = null;

		if (parentDocumentName != null) {
			parentDocument = module.getDocument(customer, parentDocumentName);
			String parentPackageName = parentDocument.getOwningModuleName();
			String parentPackagePath = new StringBuilder(128).append("modules.")
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
			if (document.getAttributes().isEmpty()) {
				imports.add("jakarta.xml.bind.annotation.XmlElement");
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
		for (String conditionName : conditions.keySet()) {
			Condition condition = conditions.get(conditionName);

			if ((! overridden) ||
					(documentClass == null) ||
					(!documentClass.attributes.containsKey(conditionName))) {
				imports.add("jakarta.xml.bind.annotation.XmlTransient");

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

		imports.add("jakarta.xml.bind.annotation.XmlType");
		imports.add("jakarta.xml.bind.annotation.XmlRootElement");
		imports.add("jakarta.annotation.Generated");

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
						if (a instanceof Collection col) {
							if (CollectionType.child.equals(col.getType()) && 
									documentName.equals(col.getDocumentName())) {
								childCollectionMethodName = col.getName();
								childCollectionMethodName = Character.toUpperCase(childCollectionMethodName.charAt(0)) + 
																childCollectionMethodName.substring(1);
								break;
							}
						}
						else if (a instanceof Association ass) {
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
			if (attribute instanceof Enumeration enumeration) {
				if (! enumeration.isDynamic()) {
					contents.append(" * @depend - - - ").append(enumeration.toJavaIdentifier()).append('\n');
				}
			}
		}

		for (String referenceName : document.getReferenceNames()) {
			Reference reference = document.getNullSafeReferenceByName(referenceName);
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
				int min = collection.getMinCardinality();
				contents.append(' ').append(min).append("..");
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
		contents.append("\n@Generated(value = \"").append(getClass().getName()).append("\")");
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
				contents.append((customer == null) ? VANILLA_CLASS_SUFFIX : OVERRIDE_CLASS_SUFFIX);
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

		if (! statics.isEmpty()) {
			contents.append(statics);
		}
		if (! enums.isEmpty()) {
			contents.append(enums);
		}
		contents.append(attributes);
		if (! methods.isEmpty()) {
			contents.append(methods, 0, methods.length() - 1); // don't include last \n
		}

		contents.append("}\n");
	}

	private boolean testPolymorphic(@Nonnull Document document) {
		// If this is a polymorphically mapped document, its NOT a polymorphic query
		// as the polymorphism is not represented in the RDBMS.
		// That is, it can't be queried as it doesn't have its own table.
		Persistent persistent = document.getPersistent();
		if (persistent != null) {
			if (persistent.isPolymorphicallyMapped()) {
				return false;
			}
		}
		else {
			return false;
		}

		return testPolymorphicAllTheWayDown(document);
	}

	private boolean testPolymorphicAllTheWayDown(@Nonnull Document document) {
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

	private static void attributeJavadoc(@Nonnull Attribute attribute, @Nonnull StringBuilder toAppendTo) {
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

	private static void accessorJavadoc(@Nonnull Attribute attribute, @Nonnull StringBuilder toAppendTo, boolean mapped) {
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

	private static void mutatorJavadoc(@Nonnull Attribute attribute, @Nonnull StringBuilder toAppendTo, boolean mapped) {
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


	private static void collectionJavadoc(@Nonnull String attributeName, @Nonnull StringBuilder toAppendTo, boolean add, boolean indexed) {
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
	private boolean domainExtensionClassExists(@Nonnull String modulePath, @Nonnull String documentName) {
		return Files.exists(Paths.get(srcPath, modulePath, documentName, documentName + "Extension.java"));
	}

	/**
	 * Return all vanilla attributes for the specified document.
	 */
	private @Nonnull List<? extends Attribute> getAllAttributes(@Nonnull Document document) {
		List<Attribute> result = new ArrayList<>(document.getAttributes());
		Extends currentInherits = document.getExtends();
		if (currentInherits != null) {
			while (currentInherits != null) {
				Module module = repository.getModule(null, document.getOwningModuleName());
				if (module == null) {
					throw new MetaDataException("Owning module " + document.getOwningModuleName() + " was not found");
				}
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
	private boolean testAlreadyExists(@Nonnull Path testToBeCreated) {
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
	@SuppressWarnings("java:S3776") // Complexity OK
	private void validateDocumentAttributeNames(final @Nullable Document document) {
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
				if ((attribute instanceof Field field) && field.isDynamic()) {
					continue;
				}
				
				AttributeType type = attribute.getAttributeType();

				if (document.getPersistent() == null || attribute.isPersistent() == false) {
					// return, attribute is transient
					LOGGER.debug("Ignoring transient attribute {} for document {}", attribute.getName(), document.getName());
					continue;
				}

				// skip checking association or collections as their persistent field name will be modified
				if (AttributeType.collection.equals(type) || AttributeType.association.equals(type)
						|| AttributeType.inverseOne.equals(type) || AttributeType.inverseMany.equals(type)) {
					continue;
				}

				// check not using a reserved word
				switch (dialectOptions) {
					case MSSQL_2014, MSSQL_2016:
						if (SQL_SERVER_RESERVED_WORDS.contains(attribute.getName().toLowerCase())) {
							throw new MetaDataException(
									createDialectError(document, attribute, dialectOptions));
						}
						break;
					case MYSQL_5, MYSQL_5_4_BYTE_CHARSET:
						if (MYSQL_5_RESERVED_WORDS.contains(attribute.getName().toLowerCase())) {
							throw new MetaDataException(
									createDialectError(document, attribute, dialectOptions));
						}
						break;
					case MYSQL_8, MYSQL_8_4_BYTE_CHARSET:
						if (MYSQL_8_RESERVED_WORDS.contains(attribute.getName().toLowerCase())) {
							throw new MetaDataException(
									createDialectError(document, attribute, dialectOptions));
						}
						break;
					case POSTGRESQL:
						if (POSTGRESQL_RESERVED_WORDS.contains(attribute.getName().toLowerCase())) {
							throw new MetaDataException(
									createDialectError(document, attribute, dialectOptions));
						}
						break;
					case H2, H2_NO_INDEXES:
						// H2 handled below
						break;
					default:
						throw new IllegalStateException(dialectOptions + " not handled");
				}
				// H2 is the default dialect and also the test database	
				if (H2_RESERVED_WORDS.contains(attribute.getName().toLowerCase())) {
					System.err.println("Reserved word: " + attribute.getName());
					throw new MetaDataException(
							createDialectError(document, attribute, DialectOptions.H2_NO_INDEXES));
				}
			}
		}
	}

	private static @Nonnull String createDialectError(final @Nonnull Document document, @Nonnull Attribute attribute, @Nonnull DialectOptions dialect) {
		return new StringBuilder(256).append("Document ")
										.append(document.getOwningModuleName())
										.append('.')
										.append(document.getName())
										.append(" cannot contain attribute named \"")
										.append(attribute.getName())
										.append("\" because it is a reserved word in database dialect ")
										.append(dialect.getDescription())
										.append('.').toString();
	}

	/**
	 * Deletes the specified directory and all subfolders/files
	 * 
	 * @param directory The path to the directory to delete
	 * @throws IOException
	 */
	private static void deleteDirectory(@Nonnull Path directory) throws IOException {
		if (Files.exists(directory)) {
			Files.walkFileTree(directory, new SimpleFileVisitor<Path>() {
				@Override
				public @Nonnull FileVisitResult visitFile(@SuppressWarnings("null") @Nonnull Path path,
															@SuppressWarnings("null") @Nonnull BasicFileAttributes basicFileAttributes) throws IOException {
					Files.delete(path);
					return FileVisitResult.CONTINUE;
				}

				@Override
				public @Nonnull FileVisitResult postVisitDirectory(@SuppressWarnings("null") @Nonnull Path subDir,
																	@Nullable IOException ioException) throws IOException {
					Files.delete(subDir);
					return FileVisitResult.CONTINUE;
				}
			});
		}
	}

	/**
	 * Collects the customer output of a previous run so it can be deleted and written afresh, mirroring
	 * the treatment of the vanilla <code>modules/&lt;m&gt;/domain</code> directories. Below
	 * <code>&lt;generatedSrcPath&gt;/customers/&lt;c&gt;/</code> it collects every entry that is not a
	 * directory:
	 * <ul>
	 * 	<li>directly in <code>modules/&lt;m&gt;/domain/</code> - module metadata enums,
	 * 		<code>&lt;Document&gt;Override</code> classes and new customer document classes;</li>
	 * 	<li>directly in <code>domain/</code> - the customer role enum;</li>
	 * 	<li><code>modules/orm.hbm.xml</code> - the customer ORM mapping;</li>
	 * 	<li><code>modules/metadata-enums-only.txt</code> - a marker an earlier generator wrote.</li>
	 * </ul>
	 * Symbolic links to directories are not followed. Nothing else is collected, so this is safe when
	 * the generated source path is the source path, where <code>customers/&lt;c&gt;/</code> also holds the
	 * customer metadata, resources and module overrides.
	 *
	 * @return	the files (and links) to delete.
	 */
	private @Nonnull List<Path> collectGeneratedCustomerOutput() {
		final List<Path> result = new ArrayList<>();
		for (Path customerDirectory : realSubdirectories(Paths.get(generatedSrcPath, ProvidedRepository.CUSTOMERS_NAME))) {
			final Path modulesDirectory = customerDirectory.resolve(ProvidedRepository.MODULES_NAME);
			for (Path moduleDirectory : realSubdirectories(modulesDirectory)) {
				collectNonDirectories(moduleDirectory.resolve(ProvidedRepository.DOMAIN_NAME), result);
			}
			if (Files.isDirectory(modulesDirectory, LinkOption.NOFOLLOW_LINKS)) {
				for (String fileName : new String[] {"orm.hbm.xml", "metadata-enums-only.txt"}) {
					Path file = modulesDirectory.resolve(fileName);
					if (Files.exists(file, LinkOption.NOFOLLOW_LINKS) && (! Files.isDirectory(file, LinkOption.NOFOLLOW_LINKS))) {
						result.add(file);
					}
				}
			}
			collectNonDirectories(customerDirectory.resolve(ProvidedRepository.DOMAIN_NAME), result);
		}
		return result;
	}

	/**
	 * Checks, before anything is deleted, that every customer file this run generates can be written:
	 * a file already at its path must be one {@link #collectGeneratedCustomerOutput()} collected for
	 * deletion. Otherwise - a directory or an entry behind a linked directory, say - writing it with
	 * {@link StandardOpenOption#CREATE_NEW} would fail after the generated tree had been emptied.
	 *
	 * @param customerOutput	The customer output collected for deletion.
	 * @throws IOException	if a customer file this run generates is blocked by an existing entry.
	 */
	private void checkCustomerGenerationTargets(@Nonnull List<Path> customerOutput) throws IOException {
		final Path customersDirectory = Paths.get(generatedSrcPath, ProvidedRepository.CUSTOMERS_NAME).toAbsolutePath().normalize();
		final Set<Path> deletions = new TreeSet<>();
		for (Path file : customerOutput) {
			deletions.add(file.toAbsolutePath().normalize());
		}
		for (Path target : generation.keySet()) {
			Path absoluteTarget = target.toAbsolutePath().normalize();
			if (absoluteTarget.startsWith(customersDirectory) &&
					Files.exists(target, LinkOption.NOFOLLOW_LINKS) &&
					(! deletions.contains(absoluteTarget))) {
				throw new IOException("Cannot generate " + target + " as something that is not generated output is in the way - move it and generate again");
			}
		}
	}

	/**
	 * Deletes the customer output collected by {@link #collectGeneratedCustomerOutput()}, then the
	 * directories this leaves empty, up to and including <code>customers/&lt;c&gt;/</code> and
	 * <code>customers/</code>. A directory that is not empty, or is a symbolic link, is never deleted.
	 *
	 * <p>Side effects: deletes files and directories below the generated source directory.
	 *
	 * @param customerOutput	The customer output to delete.
	 * @throws IOException	if a file or directory cannot be deleted.
	 */
	private void deleteGeneratedCustomerOutput(@Nonnull List<Path> customerOutput) throws IOException {
		for (Path file : customerOutput) {
			Files.deleteIfExists(file);
		}

		final Path customersDirectory = Paths.get(generatedSrcPath, ProvidedRepository.CUSTOMERS_NAME);
		for (Path customerDirectory : realSubdirectories(customersDirectory)) {
			final Path modulesDirectory = customerDirectory.resolve(ProvidedRepository.MODULES_NAME);
			for (Path moduleDirectory : realSubdirectories(modulesDirectory)) {
				deleteEmptyDirectory(moduleDirectory.resolve(ProvidedRepository.DOMAIN_NAME));
				deleteEmptyDirectory(moduleDirectory);
			}
			deleteEmptyDirectory(modulesDirectory);
			deleteEmptyDirectory(customerDirectory.resolve(ProvidedRepository.DOMAIN_NAME));
			deleteEmptyDirectory(customerDirectory);
		}
		deleteEmptyDirectory(customersDirectory);
	}

	/**
	 * Lists the subdirectories of a directory that are not symbolic links.
	 *
	 * @param directory	The directory to list.
	 * @return	the subdirectories, or an empty list if the directory does not exist or is a link.
	 */
	private static @Nonnull List<Path> realSubdirectories(@Nonnull Path directory) {
		final List<Path> result = new ArrayList<>();
		if (Files.isDirectory(directory, LinkOption.NOFOLLOW_LINKS)) {
			final File[] children = directory.toFile().listFiles();
			if (children != null) {
				for (File child : children) {
					Path childPath = child.toPath();
					if (Files.isDirectory(childPath, LinkOption.NOFOLLOW_LINKS)) {
						result.add(childPath);
					}
				}
			}
		}
		return result;
	}

	/**
	 * Adds the entries directly in a directory that are not directories - files and links - to the list.
	 * Nothing is added if the directory does not exist or is a symbolic link.
	 *
	 * @param directory	The directory to collect from.
	 * @param result	The list to add to.
	 */
	private static void collectNonDirectories(@Nonnull Path directory, @Nonnull List<Path> result) {
		if (Files.isDirectory(directory, LinkOption.NOFOLLOW_LINKS)) {
			final File[] children = directory.toFile().listFiles();
			if (children != null) {
				for (File child : children) {
					Path childPath = child.toPath();
					if (! Files.isDirectory(childPath, LinkOption.NOFOLLOW_LINKS)) {
						result.add(childPath);
					}
				}
			}
		}
	}

	/**
	 * Deletes a directory if it exists and is empty.
	 *
	 * @param directory	The directory to delete.
	 * @throws IOException	if the directory cannot be read or deleted.
	 */
	private static void deleteEmptyDirectory(@Nonnull Path directory) throws IOException {
		if (Files.isDirectory(directory, LinkOption.NOFOLLOW_LINKS)) {
			try (Stream<Path> children = Files.list(directory)) {
				if (children.findAny().isPresent()) {
					return;
				}
			}
			Files.delete(directory);
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
	@SuppressWarnings("java:S3776") // Complexity OK
	private void replaceGenerated(@Nonnull List<String> moduleNames) throws IOException {
		// src/generated/java/customers/* domain classes, metadata enums and ORM mappings - collected and
		// checked before anything is deleted so a blocked customer file cannot leave the tree half emptied
		final List<Path> customerOutput = collectGeneratedCustomerOutput();
		checkCustomerGenerationTargets(customerOutput);

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
								File[] domainFiles = packagePath.toFile().listFiles();
								if (domainFiles != null) {
									for (File domainFile : domainFiles) {
										Files.delete(domainFile.toPath());
									}
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

		deleteGeneratedCustomerOutput(customerOutput);

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
			Path parent = path.getParent();
			if (parent != null) {
				Files.createDirectories(parent);
			}
			try (BufferedWriter bw = Files.newBufferedWriter(path, StandardCharsets.UTF_8, StandardOpenOption.CREATE_NEW)) {
				bw.append(entry.getValue());
			}
			i.remove();
		}
	}
}
