package org.skyve.impl.generate;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;

import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.customer.CustomerImpl.ExportedReference;
import org.skyve.impl.metadata.model.document.AbstractInverse;
import org.skyve.impl.metadata.model.document.AbstractInverse.InverseRelationship;
import org.skyve.impl.metadata.model.document.CollectionImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.model.document.field.Enumeration;
import org.skyve.impl.metadata.model.document.field.Enumeration.EnumeratedValue;
import org.skyve.impl.metadata.model.document.field.Field;
import org.skyve.impl.metadata.model.document.field.Field.IndexType;
import org.skyve.impl.metadata.model.document.field.LengthField;
import org.skyve.impl.metadata.repository.AbstractRepository;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.SortDirection;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.Extends;
import org.skyve.metadata.model.Persistent;
import org.skyve.metadata.model.Persistent.ExtensionStrategy;
import org.skyve.metadata.model.document.Association;
import org.skyve.metadata.model.document.Association.AssociationType;
import org.skyve.metadata.model.document.Collection;
import org.skyve.metadata.model.document.Collection.CollectionType;
import org.skyve.metadata.model.document.Collection.Ordering;
import org.skyve.metadata.model.document.Condition;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.Inverse;
import org.skyve.metadata.model.document.Reference;
import org.skyve.metadata.model.document.Reference.ReferenceType;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.Module.DocumentRef;

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

	OverridableDomainGenerator() {
		// reduce visibility
	}

	@Override
	public void generate() throws Exception {
		AbstractRepository repository = AbstractRepository.get();

		populateDataStructures();

		// generate the domain classes for vanilla modules
		for (String moduleName : repository.getAllVanillaModuleNames()) {
			Module module = repository.getModule(null, moduleName);
			generateVanilla(module);
		}

		// generate for customer overrides
		for (String customerName : repository.getAllCustomerNames()) {
			Customer customer = repository.getCustomer(customerName);
			String modulesPath = SRC_PATH + repository.CUSTOMERS_NAMESPACE + 
									customerName + '/' + repository.MODULES_NAME + '/';
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
	}

	@SuppressWarnings("synthetic-access")
	private void populateDataStructures() throws Exception {
		final AbstractRepository repository = AbstractRepository.get();

		// Populate Base Data Structure with Vanilla definitions
		for (String moduleName : repository.getAllVanillaModuleNames()) {
			final Module module = repository.getModule(null, moduleName);

			// Populate the properties map
			final TreeMap<String, DomainClass> documentClasses = new TreeMap<>();
			moduleDocumentVanillaClasses.put(moduleName, documentClasses);
			
			new ModuleDocumentVisitor() {
				@Override
				public void accept(Document document) throws Exception {
					populatePropertyLengths(repository, null, module, document, null);
					String documentName = document.getName();
					DomainClass domainClass = new DomainClass();
					domainClass.attributes = generateDocumentPropertyNames(document);
					documentClasses.put(documentName, domainClass);

					populateModocDerivations(repository, module, document, null);
				}
			}.visit(null, module);
		}

		
		// TODO how do I handle overridden extended documents?
		
		// Restrict the base class definitions based on customer overrides
		for (String customerName : repository.getAllCustomerNames()) {
			final Customer customer = repository.getCustomer(customerName);
			String modulesPath = SRC_PATH + repository.CUSTOMERS_NAMESPACE + 
									customerName + '/' + repository.MODULES_NAME + '/';
			File customerModulesDirectory = new File(modulesPath);
			if (customerModulesDirectory.exists() && customerModulesDirectory.isDirectory()) {
				for (final Module module : customer.getModules()) {
					final String moduleName = module.getName();
					final TreeMap<String, DomainClass> vanillaDocumentClasses = moduleDocumentVanillaClasses.get(moduleName);

					new ModuleDocumentVisitor() {
						@Override
						public void accept(Document document) throws Exception {
							String documentName = document.getName();
							String modoc = moduleName + '.' + documentName;
							String documentPackagePath = ((CustomerImpl) customer).getVTable().get(modoc);
							if (documentPackagePath.startsWith(repository.CUSTOMERS_NAMESPACE)) {
								populatePropertyLengths(repository, customer, module, document, null);

								// Refine the moduleDocumentProperties, if the vanilla class exists
								// NB this will not exist if there is a customer document included that is not an override of a vanilla one
								DomainClass vanillaDocumentClass = (vanillaDocumentClasses == null) ? null : vanillaDocumentClasses.get(documentName);
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
	private void populateModocDerivations(AbstractRepository repository, 
											Module module, 
											Document document, 
											ExtensionStrategy strategyToAssert) {
		Extends inherits = document.getExtends();
		Persistent persistent = document.getPersistent();
		ExtensionStrategy strategy = (persistent == null) ? null : persistent.getStrategy();
		boolean mapped = (persistent == null) ? false : ExtensionStrategy.mapped.equals(strategy);
		Document baseDocument = (inherits != null) ? module.getDocument(null, inherits.getDocumentName()) : null;
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
						persistent.getPersistentIdentifier().equals((baseUnmappedPersistent == null) ? null : baseUnmappedPersistent.getPersistentIdentifier())) {
					throw new MetaDataException("Document " + document.getName() + " extends document " + 
													baseUnmappedDocument.getName() + " with a strategy of " + strategy + " but the persistent identifiers are the same.");
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
													baseUnmappedDocument.getName() + " with a strategy of " + strategy + " but the persistent identifiers are different.");
				}
			}

			putModocDerivation(document, baseDocument);
			Module baseModule = repository.getModule(null, baseDocument.getOwningModuleName());
			populateModocDerivations(repository,
										baseModule,
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

	@SuppressWarnings("synthetic-access")
	private void generateVanilla(final Module module) throws Exception {
		final AbstractRepository repository = AbstractRepository.get();
		final String moduleName = module.getName();

		// clear out the domain folder
		final String packagePath = repository.MODULES_NAMESPACE + moduleName + '/' + repository.DOMAIN_NAME;
		File domainFolder = new File(SRC_PATH + packagePath + '/');
		if (domainFolder.exists()) {
			for (File domainFile : domainFolder.listFiles()) {
				domainFile.delete();
			}
		}
		else {
			domainFolder.mkdir();
		}

		// Make a orm.hbm.xml file
		File mappingFile = new File(SRC_PATH + packagePath + '/' + moduleName + "_orm.hbm.xml");
		if (UtilImpl.XML_TRACE) {
			UtilImpl.LOGGER.fine("Mapping file is " + mappingFile);
		}
		mappingFile.createNewFile();
		final StringBuilder filterDefinitions = new StringBuilder(1024);
		try (FileWriter mappingFileWriter = new FileWriter(mappingFile)) {
			createMappingFileHeader(mappingFileWriter);

			new ModuleDocumentVisitor() {
				@Override
				public void accept(Document document) throws Exception {
					String documentName = document.getName();
					TreeMap<String, DomainClass> domainClasses = moduleDocumentVanillaClasses.get(moduleName);
					DomainClass domainClass = (domainClasses == null) ? null : domainClasses.get(documentName);
					
					// Generate base
					File classFile = new File(SRC_PATH + packagePath + '/' + documentName + ".java");
					classFile.createNewFile();

					try (FileWriter fw = new FileWriter(classFile)) {
						Extends inherits = document.getExtends();
						generateJava(repository,
										null, 
										module, 
										document, 
										fw, 
										packagePath.replace('/', '.'),
										documentName, 
										(inherits != null) ? inherits.getDocumentName() : null,
										false);
					}

					if ((domainClass != null) && domainClass.isAbstract) {
						// Generate extension
						classFile = new File(SRC_PATH + packagePath + '/' + documentName + "Ext.java");
						classFile.createNewFile();
	
						try (FileWriter fw = new FileWriter(classFile)) {
							generateJava(repository,
											null, 
											module, 
											document, 
											fw, 
											packagePath.replace('/', '.'),
											documentName, 
											documentName,
											true);
						}
					}
					
					generateORM(mappingFileWriter,
									module, 
									document, 
									repository.MODULES_NAME + '.', 
									(domainClass != null) && domainClass.isAbstract,
									null,
									filterDefinitions);
				}
			}.visit(null, module);

			createMappingFileFooter(mappingFileWriter, filterDefinitions);
		}
	}

	@SuppressWarnings("synthetic-access")
	private void generateOverridden(final Customer customer, 
										final String modulesPath)
	throws Exception {
		// Make the orm.hbm.xml file
		File mappingFile = new File(modulesPath + "orm.hbm.xml");
		mappingFile.delete();
		mappingFile.createNewFile();
		final StringBuilder filterDefinitions = new StringBuilder(1024);
		try (FileWriter mappingFileWriter = new FileWriter(mappingFile)) {
			createMappingFileHeader(mappingFileWriter);

			final AbstractRepository repository = AbstractRepository.get();
			for (final Module module : customer.getModules()) {
				final String moduleName = module.getName();
				System.out.println("Module " + moduleName);
				// clear out the domain folder
				final String packagePath = repository.CUSTOMERS_NAMESPACE +
											customer.getName() + '/' + 
											repository.MODULES_NAMESPACE +
											moduleName + '/' + repository.DOMAIN_NAME;
				final File domainFolder = new File(SRC_PATH + packagePath + '/');
				if (domainFolder.exists()) {
					for (File domainFile : domainFolder.listFiles()) {
						domainFile.delete();
					}
				}

				// Get the module's document domain classes
				final TreeMap<String, DomainClass> documentClasses = moduleDocumentVanillaClasses.get(moduleName);

				new ModuleDocumentVisitor() {
					@Override
					public void accept(Document document) throws Exception {
						String documentName = document.getName();
						String modoc = moduleName + '.' + documentName;
						String documentPackagePath = ((CustomerImpl) customer).getVTable().get(modoc);

						if ((documentClasses != null) && documentPackagePath.startsWith(repository.CUSTOMERS_NAMESPACE)) {
							// this is either an override or a totally new document.
							// for an override, baseDocumentName != null
							// for a new document definition, baseDocumentName == null
							String vanillaDocumentName = documentClasses.containsKey(documentName) ? documentName : null;

							// bug out if this document is an override but doesn't add any more properties
							if (vanillaDocumentName != null) { // overridden document
								TreeMap<String, AttributeType> extraProperties = getOverriddenDocumentExtraProperties(document);
								if (extraProperties.isEmpty()) { // no extra properties in override
									generateOverriddenORM(mappingFileWriter, customer, module, document, filterDefinitions);
									return;
								}
							}

							// Make domain folder if it does not exist yet - ie create the folder on demand
							if (! domainFolder.exists()) {
								domainFolder.mkdir();
							}

							// Generate extension
							String classFileName = SRC_PATH + packagePath + '/' + documentName;
							if (vanillaDocumentName != null) {
								classFileName += "Ext";
							}
							File classFile = new File(classFileName + ".java");
							classFile.createNewFile();
							
							try (FileWriter fw = new FileWriter(classFile)) {
								generateJava(repository,
												customer, 
												module, 
												document, 
												fw,
												packagePath.replace('/', '.'), 
												documentName,
												vanillaDocumentName,
												true);
							}
							generateOverriddenORM(mappingFileWriter, customer, module, document, filterDefinitions);
						}
					}
				}.visit(customer, module);
			}			
			
			createMappingFileFooter(mappingFileWriter, filterDefinitions);
		}
	}

	private TreeMap<String, AttributeType> getOverriddenDocumentExtraProperties(Document overriddenDocument) {
		final TreeMap<String, DomainClass> vanillaDocumentClasses = moduleDocumentVanillaClasses.get(overriddenDocument.getOwningModuleName());
		
		@SuppressWarnings("synthetic-access")
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
	
	private static void createMappingFileHeader(FileWriter mappingFileWriter) throws IOException {
		mappingFileWriter.append("<?xml version=\"1.0\"?>\n");
		mappingFileWriter.append("<!DOCTYPE hibernate-mapping PUBLIC \"-//Hibernate/Hibernate Mapping DTD 3.0//EN\" \"http://hibernate.sourceforge.net/hibernate-mapping-3.0.dtd\">\n");
		mappingFileWriter.append("<hibernate-mapping default-access=\"field\">\n\n");
		mappingFileWriter.append("\t<typedef name=\"OptimisticLock\" class=\"org.skyve.impl.domain.types.OptimisticLockUserType\" />\n");
		mappingFileWriter.append("\t<typedef name=\"Decimal2\" class=\"org.skyve.impl.domain.types.Decimal2UserType\" />\n");
		mappingFileWriter.append("\t<typedef name=\"Decimal5\" class=\"org.skyve.impl.domain.types.Decimal5UserType\" />\n");
		mappingFileWriter.append("\t<typedef name=\"Decimal10\" class=\"org.skyve.impl.domain.types.Decimal10UserType\" />\n");
		mappingFileWriter.append("\t<typedef name=\"DateOnly\" class=\"org.skyve.impl.domain.types.DateOnlyUserType\" />\n");
		mappingFileWriter.append("\t<typedef name=\"DateTime\" class=\"org.skyve.impl.domain.types.DateTimeUserType\" />\n");
		mappingFileWriter.append("\t<typedef name=\"TimeOnly\" class=\"org.skyve.impl.domain.types.TimeOnlyUserType\" />\n");
		mappingFileWriter.append("\t<typedef name=\"Timestamp\" class=\"org.skyve.impl.domain.types.TimestampUserType\" />\n");
		mappingFileWriter.append("\t<typedef name=\"Enum\" class=\"org.skyve.impl.domain.types.EnumUserType\" />\n");
		mappingFileWriter.append("\t<typedef name=\"Geometry\" class=\"org.hibernatespatial.GeometryUserType\" />\n\n");
	}

	private static void createMappingFileFooter(FileWriter mappingFileWriter, StringBuilder filterDefinitions) throws IOException {
		mappingFileWriter.append(filterDefinitions.toString());
		mappingFileWriter.append("</hibernate-mapping>");
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
	private void generateORM(FileWriter fw, 
								Module module, 
								Document document, 
								String packagePathPrefix, 
								boolean forExt,
								Customer customer,
								StringBuilder filterDefinitions)
	throws Exception{
		generateORM(fw, module, document, packagePathPrefix, forExt, false, customer, filterDefinitions, "");
	}
	
	private void generateORM(FileWriter fw, 
								Module module, 
								Document document, 
								String packagePathPrefix, 
								boolean forExt, // indicates if we are processing a customer override
								boolean recursive,
								Customer customer,
								StringBuilder filterDefinitions,
								String indentation) // indents subclass definitions within the class definition
	throws Exception {
		Extends inherits = document.getExtends();
		if ((! recursive) && // not a recursive call
				(inherits != null)) { // this is a sub-class
			return;
		}

		AbstractRepository repository = AbstractRepository.get();
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
		
		System.out.println("Generate ORM for " + packagePathPrefix + moduleName + '.' + repository.DOMAIN_NAME + '.' + documentName);

		// class defn
		if ((persistent != null) && (persistent.getName() != null)) { // persistent document
			if (baseDocumentName != null) {
				if (ExtensionStrategy.joined.equals(strategy)) {
					fw.append(indent).append("\t<joined-subclass name=\"");
				}
				else if (ExtensionStrategy.single.equals(strategy)) {
					fw.append(indent).append("\t<subclass name=\"");
				}
			}
			else {
				fw.append(indent).append("\t<class name=\"");
			}
			String extensionPath = SRC_PATH + packagePathPrefix.replace('.', '/') + moduleName + '/' + documentName + '/' + documentName + "Extension.java";
			if (new File(extensionPath).exists()) {
				System.out.println("    Generate ORM using " + extensionPath);
				fw.append(packagePathPrefix).append(moduleName).append('.').append(documentName).append('.').append(documentName).append("Extension");
			}
			else {
				fw.append(packagePathPrefix).append(moduleName).append('.').append(repository.DOMAIN_NAME).append('.').append(documentName);
				if (forExt) {
					fw.append("Ext");
				}
			}
			fw.append("\" ");
	
			if ((baseDocumentName == null) || ExtensionStrategy.joined.equals(strategy)) {
				fw.append("table=\"").append(persistent.getName());
				String schemaName = persistent.getSchema();
				if (schemaName != null) {
				    fw.append("\" schema=\"").append(schemaName);
				}
				String catalogName = persistent.getCatalog();
				if (catalogName != null) {
				    fw.append("\" catalog=\"").append(catalogName);
				}
				fw.append("\" ");
			}
			
			if (ExtensionStrategy.single.equals(strategy)) {
				fw.append("discriminator-value=\"");
				String discriminator = persistent.getDiscriminator();
				if (discriminator == null) {
					fw.append(moduleName).append(documentName);
				}
				else {
					fw.append(discriminator);
				}
				fw.append("\" ");
			}
			
			fw.append("entity-name=\"");
			StringBuilder entityNameBuilder = new StringBuilder(64);
			if (customerName != null) {
				entityNameBuilder.append(customerName);
			}
			entityNameBuilder.append(moduleName).append(documentName);
			entityName = entityNameBuilder.toString();
			fw.append(entityName).append("\">\n");
			
			if ((baseDocumentName != null) && ExtensionStrategy.joined.equals(strategy)) {
				fw.append(indent).append("\t\t<key column=\"bizId\" />\n");
			}
			else if (baseDocumentName == null) {
				// map inherited properties
				fw.append(indent).append("\t\t<id name=\"bizId\" length=\"36\" />\n");
	
				if (ExtensionStrategy.single.equals(strategy)) {
					fw.append(indent).append("\t\t<discriminator column=\"bizDiscriminator\" type=\"string\" />\n");
				}
				
				fw.append(indent).append("\t\t<version name=\"bizVersion\" unsaved-value=\"null\" />\n");
				// bizLock length of 271 is 17 for the timestamp (yyyyMMddHHmmssSSS) + 254 (email address max length from RFC 5321)
				fw.append(indent).append("\t\t<property name=\"bizLock\" type=\"OptimisticLock\" length=\"271\" not-null=\"true\" />\n");
				// bizKey must be nullable as the Hibernate NOT NULL constraint check happens before 
				// HibernateListener.preInsert() and HibernateListener.preUpdate() are fired - ie before bizKey is populated.
				// HibernateListener checks for null bizKeys manually.
				fw.append(indent).append("\t\t<property name=\"bizKey\" length=\"1024\" index=\"bizKeyIndex\" not-null=\"true\" />\n");
				fw.append(indent).append("\t\t<property name=\"bizCustomer\" length=\"50\" index=\"bizCustomerIndex\" not-null=\"true\" />\n");
				fw.append(indent).append("\t\t<property name=\"bizFlagComment\" length=\"1024\" />\n");
				fw.append(indent).append("\t\t<property name=\"bizDataGroupId\" length=\"36\" />\n");
				fw.append(indent).append("\t\t<property name=\"bizUserId\" length=\"36\" index=\"bizUserIdIndex\" not-null=\"true\" />\n");
			}
			
			// map the parent property, if parent document is persistent
			String parentDocumentName = document.getParentDocumentName();
			if (parentDocumentName != null) {
				Document parentDocument = document.getParentDocument(null);
				if (parentDocument.getPersistent() != null) {
					if (parentDocumentName.equals(documentName)) { // hierarchical
						fw.append(indent).append("\t\t<property name=\"bizParentId\" length=\"36\" index=\"bizParentIdIndex\" />\n");
					}
					else {
						// Add bizOrdinal ORM
						if (document.isOrdered()) {
							fw.append(indent).append("\t\t<property name=\"bizOrdinal\" />\n");
						}
		
						// Add parent reference ORM
						String parentModuleName = module.getDocument(null, parentDocumentName).getOwningModuleName();
						fw.append(indent).append("\t\t<many-to-one name=\"parent\" entity-name=\"");
						// reference overridden document if applicable
						if (overriddenORMDocumentsPerCustomer.contains(parentModuleName + '.' + parentDocumentName)) {
							fw.append(customerName);
						}
						fw.append(parentModuleName).append(parentDocumentName);
						fw.append("\" column=\"parent_id\" insert=\"false\" update=\"false\" />\n");
					}
				}
			}
	
			generateAttributeMappings(fw, repository, customer, module, document, persistent, forExt, indent);
		}
		
		TreeMap<String, Document> derivations = modocDerivations.get(moduleName + '.' + documentName);
		if (derivations != null) {
			// Do subclasses before joined-subclasses
			for (Document derivation : derivations.values()) {
				Persistent derivationPersistent = derivation.getPersistent();
				ExtensionStrategy derivationStrategy = (derivationPersistent == null) ? null : derivationPersistent.getStrategy();
				if (ExtensionStrategy.single.equals(derivationStrategy)) {
					Module derivedModule = repository.getModule(customer, derivation.getOwningModuleName());
					generateORM(fw, 
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
					generateORM(fw, 
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
					generateORM(fw, 
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
					fw.append(indent).append("\t</joined-subclass>\n");
				}
				else if (ExtensionStrategy.single.equals(strategy)) {
					fw.append(indent).append("\t</subclass>\n");
				}
			}
			else {
				generateFilterStuff(entityName, fw, filterDefinitions, indent);
				fw.append(indent).append("\t</class>\n\n");
			}
		}
	}

	private void generateAttributeMappings(FileWriter fw,
											AbstractRepository repository,
											Customer customer,
											Module module,
											Document document,
											Persistent persistent,
											boolean forExt,
											String indentation)
	throws IOException {
		Extends inherits = document.getExtends();
		if (inherits != null) {
			Document baseDocument = module.getDocument(customer, inherits.getDocumentName());
			Persistent basePersistent = baseDocument.getPersistent();
			if ((basePersistent != null) && ExtensionStrategy.mapped.equals(basePersistent.getStrategy())) {
				Module baseModule = repository.getModule(customer, baseDocument.getOwningModuleName());
				generateAttributeMappings(fw, repository, customer, baseModule, baseDocument, persistent, forExt, indentation);
			}
		}
		
		String customerName = (customer == null) ? null : customer.getName();
		String moduleName = module.getName();
		String documentName = document.getName();
		
		// map the document defined properties
		for (Attribute attribute : document.getAttributes()) {
			if (! attribute.isPersistent()) {
				continue;
			}

			if (attribute instanceof Collection) {
				Collection collection = (Collection) attribute;

				String referencedDocumentName = collection.getDocumentName();
				Document referencedDocument = module.getDocument(customer, referencedDocumentName);
				Persistent referencedPersistent = referencedDocument.getPersistent();
				String referencedModuleName = referencedDocument.getOwningModuleName();
				
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
				boolean mapped = (referencedPersistent != null) && ExtensionStrategy.mapped.equals(referencedPersistent.getStrategy());

				if (type == CollectionType.child) {
					if (mapped) {
						throw new MetaDataException("Collection " + collection.getName() + " referencing document " + 
														referencedDocument.getOwningModuleName() + '.' + referencedDocumentName +
														" cannot be a child collection as the target document is a mapped document." +
														" Use a composed collection instead.");
					}
					fw.append(indentation).append("\t\t<bag name=\"").append(collection.getName());
					if (Boolean.TRUE.equals(collection.getOrdered())) {
						fw.append("\" order-by=\"").append(Bean.ORDINAL_NAME);
					}
					else if (orderBy != null) {
						fw.append("\" order-by=\"").append(orderBy);
					}
					fw.append("\" cascade=\"all-delete-orphan\">\n");
					fw.append(indentation).append("\t\t\t<key column=\"parent_id\" />\n");

					fw.append(indentation).append("\t\t\t<one-to-many entity-name=\"");
					// reference overridden document if applicable
					if (overriddenORMDocumentsPerCustomer.contains(referencedModuleName + '.' + referencedDocumentName)) {
						fw.append(customerName);
					}
					fw.append(referencedModuleName).append(referencedDocumentName).append("\" />\n");
					fw.append(indentation).append("\t\t</bag>\n");
				}
				else {
					if (Boolean.TRUE.equals(collection.getOrdered())) {
						fw.append(indentation).append("\t\t<list name=\"").append(collection.getName());
					}
					else {
						fw.append(indentation).append("\t\t<bag name=\"").append(collection.getName());
					}

					String catalog = persistent.getCatalog();
					if (catalog != null) {
						fw.append("\" catalog=\"").append(catalog);
					}
					String schema = persistent.getSchema();
					if (schema != null) {
						fw.append("\" schema=\"").append(schema);
					}
					fw.append("\" table=\"").append(persistent.getName()).append('_').append(collection.getName());

					if (type == CollectionType.aggregation) {
						fw.append("\" cascade=\"persist,save-update,refresh,merge\">\n");
					}
					else if (type == CollectionType.composition) {
						fw.append("\" cascade=\"all-delete-orphan\">\n");
					}
					else {
						throw new IllegalStateException("Collection type " + type + " not supported.");
					}
					fw.append(indentation).append("\t\t\t<key column=\"").append(PersistentBean.OWNER_COLUMN_NAME).append("\" />\n");
					if (Boolean.TRUE.equals(collection.getOrdered())) {
						fw.append(indentation).append("\t\t\t<list-index column=\"").append(Bean.ORDINAL_NAME).append("\"/>\n");
					}
					
					if (mapped) {
						fw.append(indentation).append("\t\t<many-to-any meta-type=\"string\" id-type=\"string\">\n");
						Map<String, Document> arcs = new TreeMap<>();
						populateArcs(referencedDocument, arcs);
						for (Entry<String, Document> entry : arcs.entrySet()) {
							Document derivedDocument = entry.getValue();
							String derivedModuleName = derivedDocument.getOwningModuleName();
							String derivedDocumentName = derivedDocument.getName();
							
							fw.append(indentation).append("\t\t\t<meta-value value=\"").append(entry.getKey());
							fw.append("\" class=\"");
							// reference overridden derived document if applicable
							if (overriddenORMDocumentsPerCustomer.contains(derivedModuleName + '.' + derivedDocumentName)) {
								fw.append(customerName);
							}
							fw.append(derivedModuleName).append(derivedDocumentName).append("\" />\n");
						}
						
						fw.append(indentation).append("\t\t\t<column name=\"");
						fw.append(collection.getName()).append("_type\" />\n");
						fw.append(indentation).append("\t\t\t<column name=\"");
						fw.append(collection.getName()).append("_id\" length=\"36\" />\n");
						fw.append(indentation).append("\t\t</many-to-any>\n");
					}
					else {
						fw.append(indentation).append("\t\t\t<many-to-many entity-name=\"");
						// reference overridden document if applicable
						if (overriddenORMDocumentsPerCustomer.contains(referencedModuleName + '.' + referencedDocumentName)) {
							fw.append(customerName);
						}
						fw.append(referencedModuleName).append(referencedDocumentName);
						fw.append("\" column=\"").append(PersistentBean.ELEMENT_COLUMN_NAME).append("\" />\n");
					}
					
					if (Boolean.TRUE.equals(collection.getOrdered())) {
						fw.append(indentation).append("\t\t</list>\n");
					}
					else {
						fw.append(indentation).append("\t\t</bag>\n");
					}
				}
			}
			else if (attribute instanceof Association) {
				Association association = (Association) attribute;

				String referencedDocumentName = association.getDocumentName();
				Document referencedDocument = module.getDocument(null, referencedDocumentName);
				Persistent referencedPersistent = referencedDocument.getPersistent();
				String referencedModuleName = referencedDocument.getOwningModuleName();

				AssociationType type = association.getType();
				if ((referencedPersistent != null) && ExtensionStrategy.mapped.equals(referencedPersistent.getStrategy())) {
					fw.append(indentation).append("\t\t<any name=\"").append(association.getName());
					fw.append("\" meta-type=\"string\" id-type=\"string\">\n");
					Map<String, Document> arcs = new TreeMap<>();
					populateArcs(referencedDocument, arcs);
					for (Entry<String, Document> entry : arcs.entrySet()) {
						Document derivedDocument = entry.getValue();
						String derivedModuleName = derivedDocument.getOwningModuleName();
						String derivedDocumentName = derivedDocument.getName();

						fw.append(indentation).append("\t\t\t<meta-value value=\"").append(entry.getKey());
						fw.append("\" class=\"");
						// reference overridden document if applicable
						if (overriddenORMDocumentsPerCustomer.contains(derivedModuleName + '.' + derivedDocumentName)) {
							fw.append(customerName);
						}
						fw.append(derivedModuleName).append(derivedDocumentName).append("\" />\n");
					}
					
					fw.append(indentation).append("\t\t\t<column name=\"");
					fw.append(association.getName()).append("_type\" />\n");
					fw.append(indentation).append("\t\t\t<column name=\"");
					fw.append(association.getName()).append("_id\" length=\"36\" />\n");
					fw.append(indentation).append("\t\t</any>\n");
				}
				else {
					fw.append(indentation).append("\t\t<many-to-one name=\"");
					fw.append(association.getName()).append("\" entity-name=\"");
					// reference overridden document if applicable
					if (overriddenORMDocumentsPerCustomer.contains(referencedModuleName + '.' + referencedDocumentName)) {
						fw.append(customerName);
					}
					fw.append(referencedModuleName).append(referencedDocumentName);
					fw.append("\" column=\"").append(association.getName());
					if (type == AssociationType.composition) {
						fw.append("_id\" cascade=\"persist,save-update,refresh,delete");
					}
					else if (type == AssociationType.aggregation) {
						fw.append("_id\" cascade=\"persist,save-update,refresh");
					}
					else {
						throw new IllegalStateException("Association type " + type + " not supported.");
					}
					// Cascade type 'merge' makes many-many relationships within the association
					// target object update (without the collection being dirty)
					// and thus causes optimistic lock exceptions when the bizLock 
					// is up-revved from the update statement.
					// Case in point is Staff --many-to-one--> User --many-to-many--> Groups,
					// all groups are up-revved, even though the collection is not dirty,
					// causing optimistic lock when Staff are saved.
					// So if lots of Staff use the same user, we're screwed.
					Boolean allowCascadeMerge = association.getAllowCascadeMerge();
					if ((allowCascadeMerge == null) && ALLOW_CASCADE_MERGE) {
						fw.append(",merge");
					}
					else if (Boolean.TRUE.equals(allowCascadeMerge)) {
						fw.append(",merge");
					}
					fw.append("\" />\n");
				}
			}
			else if (attribute instanceof Enumeration) {
				Enumeration enumeration = (Enumeration) attribute;
				
				String enumerationName = enumeration.getName();
				fw.append(indentation).append("\t\t<property name=\"").append(enumerationName);

				Integer fieldLength = persistentPropertyLengths.get(persistent.getPersistentIdentifier()).get(enumerationName);
				if (fieldLength != null) {
					fw.append("\" length=\"").append(fieldLength.toString());
				}

				IndexType index = enumeration.getIndex();
				if (IndexType.database.equals(index) || IndexType.both.equals(index)) {
					fw.append("\" index=\"");
					fw.append(moduleName).append(documentName).append(enumerationName);
				}
				fw.append("\">\n");
				
				fw.append(indentation).append("\t\t\t<type name=\"Enum\">\n");
				fw.append(indentation).append("\t\t\t\t<param name=\"enumClass\">");

				// for extension and the enumeration attribute doesn't exist in the vanilla document.
				// NB - There a 2 scenarios here...
				//		1) The enumeration is defined here - forExt is true and there is no enum in the vanilla document
				//		2) The enumeration is a reference to a definition elsewhere - in this case, forExt is true
				//			and the enumeration is not defined in the enumeration target vanilla document
				Enumeration target = enumeration.getTarget();
				Document targetDocument = target.getOwningDocument();
				@SuppressWarnings("synthetic-access")
				boolean requiresExtension = forExt &&
												(! moduleDocumentVanillaClasses.get(targetDocument.getOwningModuleName()).get(targetDocument.getName()).attributes.containsKey(target.getName()));
				if (requiresExtension)  {
					fw.append(repository.CUSTOMERS_NAME).append('.').append(customerName).append('.');
				}
				fw.append(repository.getEncapsulatingClassNameForEnumeration(enumeration));
				if (requiresExtension)  {
					fw.append("Ext");
				}
				fw.append('$').append(enumeration.toJavaIdentifier());
				fw.append("</param>\n");
				fw.append(indentation).append("\t\t\t</type>\n");
				fw.append(indentation).append("\t\t</property>\n");
			}
			else if (attribute instanceof Inverse) {
				AbstractInverse inverse = (AbstractInverse) attribute;

				// determine the inverse target metadata
				String inverseDocumentName = inverse.getDocumentName();
				Document inverseDocument = module.getDocument(null, inverseDocumentName);
				Persistent inversePersistent = inverseDocument.getPersistent();
				String inverseReferenceName = inverse.getReferenceName();
				String inverseModuleName = inverseDocument.getOwningModuleName();
				InverseRelationship inverseRelationship = inverse.getRelationship();
				Boolean cascade = inverse.getCascade();
				
				if (InverseRelationship.oneToOne.equals(inverseRelationship)) {
					fw.append("\t\t<one-to-one name=\"").append(inverse.getName());

					fw.append("\" entity-name=\"");
					// reference overridden document if applicable
					if (overriddenORMDocumentsPerCustomer.contains(inverseModuleName + '.' + inverseDocumentName)) {
						fw.append(customerName);
					}
					fw.append(inverseModuleName).append(inverseDocumentName);
					
					fw.append("\" property-ref=\"").append(inverseReferenceName);
					
					if (Boolean.TRUE.equals(cascade)) {
						fw.append("\" cascade=\"persist,save-update,refresh,merge");
					}
					
					fw.append("\" />\n");
				}
				else {
					fw.append(indentation).append("\t\t<bag name=\"").append(inverse.getName());
					if (InverseRelationship.manyToMany.equals(inverseRelationship)) {
						String catalog = inversePersistent.getCatalog();
						if (catalog != null) {
							fw.append("\" catalog=\"").append(catalog);
						}
						String schema = inversePersistent.getSchema();
						if (schema != null) {
							fw.append("\" schema=\"").append(schema);
						}
						fw.append("\" table=\"").append(inversePersistent.getName()).append('_').append(inverseReferenceName);
					}
					
					if (Boolean.TRUE.equals(cascade)) {
						fw.append("\" cascade=\"persist,save-update,refresh,merge");
					}

					fw.append("\" inverse=\"true\">\n");
	
					fw.append(indentation).append("\t\t\t<key column=\"");
					if (InverseRelationship.manyToMany.equals(inverseRelationship)) {
						fw.append("element");
					}
					else {
						fw.append(inverseReferenceName);
					}
					fw.append("_id\" />\n");
					if (InverseRelationship.manyToMany.equals(inverseRelationship)) {
						fw.append(indentation).append("\t\t\t<many-to-many entity-name=\"");
					}
					else {
						fw.append(indentation).append("\t\t\t<one-to-many entity-name=\"");
					}
	
					// reference overridden document if applicable
					if (overriddenORMDocumentsPerCustomer.contains(inverseModuleName + '.' + inverseDocumentName)) {
						fw.append(customerName);
					}
					fw.append(inverseModuleName).append(inverseDocumentName);
					
					if (InverseRelationship.manyToMany.equals(inverseRelationship)) {
						fw.append("\" column=\"").append(PersistentBean.OWNER_COLUMN_NAME);
					}
					fw.append("\" />\n");
					fw.append(indentation).append("\t\t</bag>\n");
				}
			}
			else {
				Field field = (Field) attribute;
				String fieldName = field.getName();
				fw.append(indentation).append("\t\t<property name=\"").append(fieldName);

				Integer fieldLength = persistentPropertyLengths.get(persistent.getPersistentIdentifier()).get(fieldName);
				if (fieldLength != null) {
					fw.append("\" length=\"").append(fieldLength.toString());
				}

				AttributeType type = attribute.getAttributeType();
				if (type == AttributeType.decimal2) {
					fw.append("\" type=\"").append(DECIMAL2).append("\" precision=\"20\" scale=\"2");
				}
				else if (type == AttributeType.decimal5) {
					fw.append("\" type=\"").append(DECIMAL5).append("\" precision=\"23\" scale=\"5");
				}
				else if (type == AttributeType.decimal10) {
					fw.append("\" type=\"").append(DECIMAL10).append("\" precision=\"28\" scale=\"10");
				}
				else if (type == AttributeType.date) {
					fw.append("\" type=\"").append(DATE_ONLY);
				}
				else if (type == AttributeType.dateTime) {
					fw.append("\" type=\"").append(DATE_TIME);
				}
				else if (type == AttributeType.time) {
					fw.append("\" type=\"").append(TIME_ONLY);
				}
				else if (type == AttributeType.timestamp) {
					fw.append("\" type=\"").append(TIMESTAMP);
				}
				else if (type == AttributeType.geometry) {
					fw.append("\" type=\"").append(GEOMETRY);
				}
				else if (type == AttributeType.id) {
					fw.append("\" length=\"36");
				}
				else if (type == AttributeType.content) {
					fw.append("\" length=\"36");
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
					fw.append("\" type=\"text");
				}
				IndexType index = field.getIndex();
				if (IndexType.database.equals(index) || IndexType.both.equals(index)) {
					fw.append("\" index=\"");
					fw.append(moduleName).append(documentName).append(fieldName);
				}
				fw.append("\" />\n");
			}
		}
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
												FileWriter fw,
												StringBuilder filterDefinitions,
												String indentation)
	throws IOException {
		fw.append(indentation).append("\t\t<filter name=\"").append(entityName).append("NoneFilter\" condition=\"1=0\"/>\n");
		fw.append(indentation).append("\t\t<filter name=\"").append(entityName).append("CustomerFilter\" condition=\"bizCustomer=:customerParam\"/>\n");
		fw.append(indentation).append("\t\t<filter name=\"").append(entityName).append("DataGroupIdFilter\" condition=\"bizDataGroupId=:dataGroupIdParam\"/>\n");
		fw.append(indentation).append("\t\t<filter name=\"").append(entityName).append("UserIdFilter\" condition=\"bizUserId=:userIdParam\"/>\n");

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
	
	private void generateOverriddenORM(FileWriter fw, 
										Customer customer, 
										Module module, 
										Document document,
										StringBuilder filterDefinitions)
	throws Exception {
		AbstractRepository repository = AbstractRepository.get();
		CustomerImpl internalCustomer = (CustomerImpl) customer;
		String moduleName = module.getName();
		String documentName = document.getName();
		String moduleDotDocument = moduleName + '.' + documentName;
		String packagePathPrefix = repository.MODULES_NAMESPACE;
		// if customer defined document
		if (internalCustomer.getVTable().get(moduleDotDocument).startsWith(repository.CUSTOMERS_NAMESPACE)) {
			// this is either an override or a totally new document.
			// for an override, baseDocumentName != null
			// for a new document definition, baseDocumentName == null
			TreeMap<String, DomainClass> documentClasses = moduleDocumentVanillaClasses.get(moduleName);
			String vanillaDocumentName = documentClasses.containsKey(documentName) ? documentName : null;
			if (vanillaDocumentName != null) { // overridden document
				// bug out if this document is an override but doesn't add any more properties
				TreeMap<String, AttributeType> extraProperties = getOverriddenDocumentExtraProperties(document);
				if (! extraProperties.isEmpty()) { // there exists extra properties in override
					packagePathPrefix = repository.CUSTOMERS_NAMESPACE + customer.getName() + '/' + packagePathPrefix;
				}
			}
			else { // totally new document defined as a customer override
				packagePathPrefix = repository.CUSTOMERS_NAMESPACE + customer.getName() + '/' + packagePathPrefix;
			}
		}
		packagePathPrefix = packagePathPrefix.replace('/', '.');
		
		if (! visitedOverriddenORMDocumentsPerCustomer.contains(moduleDotDocument)) {
			visitedOverriddenORMDocumentsPerCustomer.add(moduleDotDocument);
			overriddenORMDocumentsPerCustomer.add(moduleDotDocument);

			// Propagate to parent document (if applicable)
			Document parentDocument = document.getParentDocument(customer);
			if (parentDocument != null) {
				if (! parentDocument.getName().equals(documentName)) { // exclude hierarchical
					String parentModuleName = parentDocument.getOwningModuleName();
					String parentDocumentName = parentDocument.getName();
					String parentModuleDotDocument = parentModuleName + '.' + parentDocumentName;
					System.out.println("\t" + parentModuleDotDocument);
					Module parentModule = customer.getModule(parentModuleName);
					generateOverriddenORM(fw, customer, parentModule, parentDocument, filterDefinitions);
				}
			}

			// Propagate to exported references
			List<ExportedReference> refs = internalCustomer.getExportedReferences(document);
			System.out.println(documentName + " has refs");
			if (refs != null) {
				for (ExportedReference ref : refs) {
					String refModuleName = ref.getModuleName();
					String refDocumentName = ref.getDocumentName();
					String refModuleDotDocument = refModuleName + '.' + refDocumentName;
					System.out.println("\t" + ref.getReferenceFieldName() + " = " + refModuleDotDocument);
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
								generateOverriddenORM(fw, customer, derivedModule, derivedDocument, filterDefinitions);
							}
						}
						else {
							generateOverriddenORM(fw, customer, refModule, refDocument, filterDefinitions);
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
					generateOverriddenORM(fw, customer, refModule, refDocument, filterDefinitions);
				}
			}

			TreeMap<String, DomainClass> domainClasses = moduleDocumentVanillaClasses.get(moduleName);
			DomainClass domainClass = (domainClasses == null) ? null : domainClasses.get(documentName);
			generateORM(fw,
							module,
							document,
							packagePathPrefix,
							// extension class in use if this is a customer override of an existing domain class
							(domainClass != null) && (packagePathPrefix.startsWith(repository.CUSTOMERS_NAME)),
							true,
							customer,
							filterDefinitions,
							"");
		}
	}

	private static final TreeMap<String, AttributeType> generateDocumentPropertyNames(Document document) {
		TreeMap<String, AttributeType> result = new TreeMap<>();

		for (Attribute attribute : document.getAttributes()) {
			if (! attribute.getName().equals(Bean.BIZ_KEY)) {
				result.put(attribute.getName(), attribute.getAttributeType());
			}
		}
		
		for (String conditionName : ((DocumentImpl) document).getConditionNames()) {
			result.put(conditionName, AttributeType.bool);
		}

		return result;
	}

	private void populatePropertyLengths(AbstractRepository repository, 
											Customer customer, 
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
				populatePropertyLengths(repository, customer, baseModule, baseDocument, persistentIdentifier);
			}

			TreeMap<String, Integer> propertyLengths = persistentPropertyLengths.get(persistentIdentifier);
			if (propertyLengths == null) {
				propertyLengths = new TreeMap<>();
				persistentPropertyLengths.put(persistentIdentifier, propertyLengths);
			}

			for (Attribute attribute : document.getAttributes()) {
				// Note - do this for persistent and non-persistent attributes
				// in case a persistent attribute references a non-persistent one
				// (like enumerations do).
				int length = Integer.MIN_VALUE;
				if (attribute instanceof LengthField) {
					length = ((LengthField) attribute).getLength();
				}
				else if (attribute instanceof Enumeration) {
					// Find the maximum code length
					Enumeration enumeration = (Enumeration) attribute;
					for (EnumeratedValue value : enumeration.getValues()) {
						int valueLength = value.getCode().length();
						if (valueLength > length) {
							length = valueLength;
						}
					}
				}
				if (length >= 0) {
					String attributeName = attribute.getName();
					Integer existingLength = propertyLengths.get(attributeName);
					if ((existingLength == null) || (existingLength.intValue() < length)) {
						propertyLengths.put(attributeName, new Integer(length));
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
	private static void appendEnumDefinition(Enumeration enumeration,
												String typeName,
												StringBuilder enums) {
		String doc = enumeration.getDocumentation();
		if (doc == null) {
			doc = enumeration.getDescription();
		}
		if (doc == null) {
			doc = enumeration.getDisplayName();
		}
		if (doc == null) {
			doc = enumeration.getName();
		}
		enums.append("\t/**\n");
		enums.append("\t * ").append(doc).append('\n');
		enums.append("\t **/\n");
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
		enums.setLength(enums.length() - 2);  // remove last '\n' and ','
		
		// members
		enums.append(";\n\n");
//		enums.append("\t\t/** @hidden */\n");
		enums.append("\t\tprivate String code;\n");
//		enums.append("\t\t/** @hidden */\n");
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
		
		// toDescription()
		enums.append("\t\t@Override\n");
		enums.append("\t\tpublic String toDescription() {\n");
		enums.append("\t\t\treturn description;\n");
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
		
		// fromDescription
		enums.append("\t\tpublic static ").append(typeName).append(" fromDescription(String description) {\n");
		enums.append("\t\t\t").append(typeName).append(" result = null;\n\n");
		enums.append("\t\t\tfor (").append(typeName).append(" value : values()) {\n");
		enums.append("\t\t\t\tif (value.description.equals(description)) {\n");
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
								Module module,
								String packagePath,
								Set<String> imports,
								StringBuilder attributes,
								StringBuilder methods) {
		String propertyClassName = reference.getDocumentName();
		Document propertyDocument = module.getDocument(customer, propertyClassName);
		String propertyPackageName = propertyDocument.getOwningModuleName();
		String name = reference.getName();
		boolean deprecated = reference.isDeprecated();

		if (overriddenReference) { // overridden reference to concrete implementation 
			return; // this already exists on the base class - don't override it.
		}

		String propertyPackagePath = "modules." + propertyPackageName + ".domain";
		// Check for overridden only in customer folder (ie no vanilla document) and change package path accordingly
		Map<String, DomainClass> documentVanillaClasses = moduleDocumentVanillaClasses.get(propertyPackageName);
		if ((documentVanillaClasses == null) || (documentVanillaClasses.get(propertyClassName) == null)) {
			propertyPackagePath = "customers." + customer.getName() + '.' + propertyPackagePath;
		}
		if (! propertyPackagePath.equals(packagePath)) {
			imports.add(propertyPackagePath + '.' + propertyClassName);
		}

		String methodName = name.substring(0, 1).toUpperCase() + name.substring(1);

		if (reference instanceof Collection) {
			imports.add("java.util.List");
			imports.add("java.util.ArrayList");

			attributeJavadoc(reference, attributes);
			if (deprecated) {
				attributes.append("\t@Deprecated\n");
			}
			attributes.append("\tprivate List<").append(propertyClassName).append("> ").append(name);
			attributes.append(" = new ArrayList<>();\n");

			// Accessor method
			accessorJavadoc(reference, methods, false);
			if (overriddenReference) { // method in base class
				methods.append("\n\t@Override");
			}
			if (deprecated) {
				methods.append("\n\t@Deprecated");
			}
			methods.append("\n\t@XmlElement");
			methods.append("\n\tpublic List<").append(propertyClassName).append("> get").append(methodName).append("() {\n");
			methods.append("\t\treturn ").append(name).append(";\n");
			methods.append("\t}\n");
			
			// Mapped Accessor method
			accessorJavadoc(reference, methods, true);
			if (overriddenReference) { // method in base class
				methods.append("\n\t@Override");
			}
			if (deprecated) {
				methods.append("\n\t@Deprecated");
			}
			methods.append("\n\tpublic ").append(propertyClassName).append(" get").append(methodName).append("ElementById(String bizId) {\n");
			methods.append("\t\treturn getElementById(").append(name).append(", bizId);\n");
			methods.append("\t}\n");
			
			// Mapped Mutator method
			mutatorJavadoc(reference, methods, true);
			if (overriddenReference) { // method in base class
				methods.append("\n\t@Override");
			}
			if (deprecated) {
				methods.append("\n\t@Deprecated");
			}
			methods.append("\n\tpublic void set").append(methodName);
			methods.append("ElementById(@SuppressWarnings(\"unused\") String bizId, ").append(propertyClassName).append(" element) {\n");
			methods.append("\t\t setElementById(").append(name).append(", element);\n");
			methods.append("\t}\n");
		}
		else { // this is an association Attribute
			attributeJavadoc(reference, attributes);
			if (deprecated) {
				attributes.append("\t@Deprecated\n");
			}
			attributes.append("\tprivate ").append(propertyClassName).append(" ").append(name);
			attributes.append(" = null;\n");

			// Accessor method
			accessorJavadoc(reference, methods, false);
			if (overriddenReference) { // method in base class
				methods.append("\n\t@Override");
			}
			if (deprecated) {
				methods.append("\n\t@Deprecated");
			}
			methods.append("\n\tpublic ").append(propertyClassName).append(" get").append(methodName).append("() {\n");
			methods.append("\t\treturn ").append(name).append(";\n");
			methods.append("\t}\n");

			// Mutator method
			mutatorJavadoc(reference, methods, false);
			if (overriddenReference) { // method in base class
				methods.append("\n\t@Override");
			}
			if (deprecated) {
				methods.append("\n\t@Deprecated");
			}
			methods.append("\n\t@XmlElement");
			methods.append("\n\tpublic void set").append(methodName).append('(');
			methods.append(propertyClassName).append(' ').append(name).append(") {\n");
			if (reference.isTrackChanges()) {
				methods.append("\t\tpreset(").append(name).append("PropertyName, ").append(name).append(");\n");
			}
			methods.append("\t\tthis.").append(name).append(" = ").append(name).append(";\n");
			methods.append("\t}\n");
		}
	}
	
	private void addInverse(AbstractInverse inverse,
								boolean overriddenInverse,
								Customer customer,
								Module module,
								String packagePath,
								Set<String> imports,
								StringBuilder attributes,
								StringBuilder methods) {
		String propertyClassName = inverse.getDocumentName();
		String propertyPackageName = module.getDocument(customer, propertyClassName).getOwningModuleName();
		String name = inverse.getName();
		boolean many = (! InverseRelationship.oneToOne.equals(inverse.getRelationship()));
		boolean deprecated = inverse.isDeprecated();

		String propertyPackagePath = "modules." + propertyPackageName + ".domain";
		// Check for overridden only in customer folder (ie no vanilla document) and change package path accordingly
		Map<String, DomainClass> documentVanillaClasses = moduleDocumentVanillaClasses.get(propertyPackageName);
		if ((documentVanillaClasses == null) || (documentVanillaClasses.get(propertyClassName) == null)) {
			propertyPackagePath = "customers." + customer.getName() + '.' + propertyPackagePath;
		}
		if (! propertyPackagePath.equals(packagePath)) {
			imports.add(propertyPackagePath + '.' + propertyClassName);
		}

		String methodName = name.substring(0, 1).toUpperCase() + name.substring(1);

		if (many) {
			imports.add("java.util.List");
			imports.add("java.util.ArrayList");
		}
		
		attributeJavadoc(inverse, attributes);
		if (deprecated) {
			attributes.append("\t@Deprecated\n");
		}
		if (many) {
			attributes.append("\tprivate List<").append(propertyClassName).append("> ").append(name);
			attributes.append(" = new ArrayList<>();\n");
		}
		else {
			attributes.append("\tprivate ").append(propertyClassName).append(" ").append(name).append(";\n");
		}
		
		// Accessor method
		accessorJavadoc(inverse, methods, false);
		if (overriddenInverse) { // method in base class
			methods.append("\n\t@Override");
		}
		if (deprecated) {
			methods.append("\n\t@Deprecated");
		}
		methods.append("\n\t@XmlElement");
		if (many) {
			methods.append("\n\tpublic List<").append(propertyClassName).append("> get").append(methodName).append("() {\n");
		}
		else {
			methods.append("\n\tpublic ").append(propertyClassName).append(" get").append(methodName).append("() {\n");
		}
		methods.append("\t\treturn ").append(name).append(";\n");
		methods.append("\t}\n");
		
		// Mapped Accessor method
		if (many) {
			accessorJavadoc(inverse, methods, true);
			if (overriddenInverse) { // method in base class
				methods.append("\n\t@Override");
			}
			if (deprecated) {
				methods.append("\n\t@Deprecated");
			}
			methods.append("\n\tpublic ").append(propertyClassName).append(" get").append(methodName).append("ElementById(String bizId) {\n");
			methods.append("\t\treturn getElementById(").append(name).append(", bizId);\n");
			methods.append("\t}\n");
		}
		// Mutator method
		else {
			mutatorJavadoc(inverse, methods, false);
			if (overriddenInverse) { // method in base class
				methods.append("\n\t@Override");
			}
			if (deprecated) {
				methods.append("\n\t@Deprecated");
			}
			methods.append("\n\tpublic void set").append(methodName).append("(");
			methods.append(propertyClassName).append(' ').append(name).append(") {\n");
			methods.append("\t\tthis.").append(name).append(" = ").append(name).append(";\n");
			methods.append("\t}\n");
		}
	}
	
	@SuppressWarnings("synthetic-access")
	private void generateJava(AbstractRepository repository,
								Customer customer,
								Module module,
								Document document,
								FileWriter fw,
								String packagePath,
								String documentName,
								String baseDocumentName,
								boolean overridden)
	throws IOException {
		System.out.println("Generate class for " + packagePath + '.' + documentName);
		Persistent persistent = document.getPersistent();
		fw.append("package ").append(packagePath).append(";\n\n");

		Set<String> imports = new TreeSet<>();
		StringBuilder statics = new StringBuilder(1024);
		StringBuilder enums = new StringBuilder(1024);
		StringBuilder attributes = new StringBuilder(1024);
		StringBuilder methods = new StringBuilder(2048);

		TreeMap<String, DomainClass> documentClasses = moduleDocumentVanillaClasses.get(module.getName());
		DomainClass documentClass = (documentClasses == null) ? null : documentClasses.get(documentName);

		// Document and module names

		if ((! overridden) || (baseDocumentName == null)) { // not an extension
			imports.add("javax.xml.bind.annotation.XmlTransient");
			imports.add("org.skyve.CORE");
			
			statics.append("\t/** @hidden */\n");
			statics.append("\tpublic static final String MODULE_NAME = \"").append(module.getName()).append("\";\n");
			statics.append("\t/** @hidden */\n");
			statics.append("\tpublic static final String DOCUMENT_NAME = \"").append(document.getName()).append("\";\n\n");

			methods.append("\n\t@Override");
			methods.append("\n\t@XmlTransient");
			methods.append("\n\tpublic String getBizModule() {\n");
			methods.append("\t\treturn ").append(documentName).append(".MODULE_NAME;\n");
			methods.append("\t}\n");
	
			methods.append("\n\t@Override");
			methods.append("\n\t@XmlTransient");
			methods.append("\n\tpublic String getBizDocument() {\n");
			methods.append("\t\treturn ").append(documentName).append(".DOCUMENT_NAME;\n");
			methods.append("\t}\n");
	
			methods.append("\n\tpublic static ").append(documentName).append(" newInstance() throws Exception {\n");
			methods.append("\t\treturn CORE.getUser().getCustomer().getModule(MODULE_NAME).getDocument(CORE.getUser().getCustomer(), DOCUMENT_NAME).newInstance(CORE.getUser());\n");
			methods.append("\t}\n");

			String bizKeyMethodCode = ((DocumentImpl) document).getBizKeyMethodCode();
			if (bizKeyMethodCode != null) {
				methods.append("\n\t@Override");
				methods.append("\n\t@XmlTransient");
				methods.append("\n\tpublic String getBizKey() {\n");
				methods.append(bizKeyMethodCode).append("\n");
				methods.append("\t}\n");
			}

			methods.append("\n\t@Override");
			methods.append("\n\tpublic boolean equals(Object o) {\n");
			methods.append("\t\treturn ((o instanceof ").append(documentName);
			methods.append(") && \n\t\t\t\t\tthis.getBizId().equals(((");
			methods.append(documentName).append(") o).getBizId()));\n");
			methods.append("\t}\n");
		}
		
		for (Attribute attribute : document.getAttributes()) {
			imports.add("javax.xml.bind.annotation.XmlElement");

			String name = attribute.getName();
			boolean deprecated = attribute.isDeprecated();
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
				statics.append(attribute.getName()).append("\";\n");

				// Generate imports
	
				AttributeType type = attribute.getAttributeType();
				String methodName = name.substring(0, 1).toUpperCase() + name.substring(1);
				String propertySimpleClassName;
				if (attribute instanceof Enumeration) {
					Enumeration enumeration = (Enumeration) attribute;
					propertySimpleClassName = enumeration.toJavaIdentifier();
	
					if (enumeration.getAttributeRef() != null) { // this is a reference
						if (enumeration.getDocumentRef() != null) { // references a different document
							StringBuilder fullyQualifiedEnumName = new StringBuilder(64);
							fullyQualifiedEnumName.append(AbstractRepository.get().getEncapsulatingClassNameForEnumeration(enumeration));
							fullyQualifiedEnumName.append('.').append(enumeration.toJavaIdentifier());
							imports.add(fullyQualifiedEnumName.toString());
						}
					}
					else {
						imports.add("org.skyve.domain.types.Enumeration");
						imports.add("org.skyve.metadata.model.document.Bizlet.DomainValue");
						imports.add("java.util.List");
						imports.add("java.util.ArrayList");
						imports.add("javax.xml.bind.annotation.XmlEnum");
						
						appendEnumDefinition(enumeration, propertySimpleClassName, enums);
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
								packagePath,
								imports,
								attributes,
								methods);
					continue;
				}
				else {
					Class<?> propertyClass = type.getImplementingType();
					String propertyClassName = propertyClass.getName();
					propertySimpleClassName = propertyClass.getSimpleName();
	
					if (! propertyClassName.startsWith("java.lang")) {
						imports.add(propertyClassName);
					}
				}
			
				// attribute declaration
				attributeJavadoc(attribute, attributes);
				if (deprecated) {
					attributes.append("\t@Deprecated\n");
				}
				attributes.append("\tprivate ").append(propertySimpleClassName).append(' ').append(name);
	
				// add attribute definition / default value if required
				String defaultValue = ((Field) attribute).getDefaultValue();
				if (defaultValue != null) {
					if (AttributeType.bool.equals(type) || 
							AttributeType.integer.equals(type) ||
							AttributeType.longInteger.equals(type)) {
						attributes.append(" = new ").append(propertySimpleClassName);
						attributes.append('(').append(defaultValue).append(')');
					}
					else if (AttributeType.colour.equals(type) || 
								AttributeType.markup.equals(type) || 
								AttributeType.memo.equals(type) || 
								AttributeType.text.equals(type)) {
						attributes.append(" = \"").append(defaultValue).append('"');
					}
					else if (AttributeType.enumeration.equals(type)) {
						attributes.append(" = ").append(propertySimpleClassName).append('.').append(defaultValue);
					}
					else {
						attributes.append(" = new ").append(propertySimpleClassName);
						attributes.append("(\"").append(defaultValue).append("\")");
					}
				}
				attributes.append(";\n");
	
				// Accessor method
				accessorJavadoc(attribute, methods, false);
				if (overridden && 
						(baseDocumentName != null) && // base class exists
						(documentClass != null) && 
						documentClass.attributes.containsKey(name)) { // method in base class
					methods.append("\n\t@Override");
				}
				if (deprecated) {
					methods.append("\n\t@Deprecated");
				}
				methods.append("\n\tpublic ").append(propertySimpleClassName).append(" get").append(methodName).append("() {\n");
				methods.append("\t\treturn ").append(name).append(";\n");
				methods.append("\t}\n");
	
				// Mutator method
				mutatorJavadoc(attribute, methods, false);
				if (overridden && 
						(baseDocumentName != null) && // base class exists
						(documentClass != null) && 
						documentClass.attributes.containsKey(name)) { // method in base class
					methods.append("\n\t@Override");
				}
				if (deprecated) {
					methods.append("\n\t@Deprecated");
				}
				if (AttributeType.date.equals(type)) {
					imports.add("javax.xml.bind.annotation.XmlSchemaType");
					imports.add("javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter");
					imports.add("org.skyve.impl.domain.types.jaxb.DateOnlyMapper");
					methods.append("\n\t@XmlSchemaType(name = \"date\")");
					methods.append("\n\t@XmlJavaTypeAdapter(DateOnlyMapper.class)");
				}
				else if (AttributeType.time.equals(type)) {
					imports.add("javax.xml.bind.annotation.XmlSchemaType");
					imports.add("javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter");
					imports.add("org.skyve.impl.domain.types.jaxb.TimeOnlyMapper");
					methods.append("\n\t@XmlSchemaType(name = \"time\")");
					methods.append("\n\t@XmlJavaTypeAdapter(TimeOnlyMapper.class)");
				}
				else if (AttributeType.dateTime.equals(type)) {
					imports.add("javax.xml.bind.annotation.XmlSchemaType");
					imports.add("javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter");
					imports.add("org.skyve.impl.domain.types.jaxb.DateTimeMapper");
					methods.append("\n\t@XmlSchemaType(name = \"dateTime\")");
					methods.append("\n\t@XmlJavaTypeAdapter(DateTimeMapper.class)");
				}
				else if (AttributeType.timestamp.equals(type)) {
					imports.add("javax.xml.bind.annotation.XmlSchemaType");
					imports.add("javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter");
					imports.add("org.skyve.impl.domain.types.jaxb.TimestampMapper");
					methods.append("\n\t@XmlSchemaType(name = \"dateTime\")");
					methods.append("\n\t@XmlJavaTypeAdapter(TimestampMapper.class)");
				}
				else if (AttributeType.decimal2.equals(type)) {
					imports.add("javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter");
					imports.add("org.skyve.impl.domain.types.jaxb.Decimal2Mapper");
					methods.append("\n\t@XmlJavaTypeAdapter(Decimal2Mapper.class)");
				}
				else if (AttributeType.decimal5.equals(type)) {
					imports.add("javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter");
					imports.add("org.skyve.impl.domain.types.jaxb.Decimal5Mapper");
					methods.append("\n\t@XmlJavaTypeAdapter(Decimal5Mapper.class)");
				}
				else if (AttributeType.decimal10.equals(type)) {
					imports.add("javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter");
					imports.add("org.skyve.impl.domain.types.jaxb.Decimal10Mapper");
					methods.append("\n\t@XmlJavaTypeAdapter(Decimal10Mapper.class)");
				}
				else if (AttributeType.geometry.equals(type)) {
					imports.add("javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter");
					imports.add("org.skyve.impl.domain.types.jaxb.GeometryMapper");
					methods.append("\n\t@XmlJavaTypeAdapter(GeometryMapper.class)");
				}
				methods.append("\n\t@XmlElement");
				methods.append("\n\tpublic void set").append(methodName).append('(');
				methods.append(propertySimpleClassName).append(' ').append(name).append(") {\n");
				if (attribute.isTrackChanges()) {
					methods.append("\t\tpreset(").append(name).append("PropertyName, ").append(name).append(");\n");
				}
				methods.append("\t\tthis.").append(name).append(" = ").append(name).append(";\n");
				methods.append("\t}\n");
			}
		}

		String parentDocumentName = document.getParentDocumentName();

		if (parentDocumentName != null) {
			if (parentDocumentName.equals(documentName)) { // hierarchical
				imports.add("java.util.List");
				imports.add("org.skyve.domain.HierarchicalBean");
				imports.add("org.skyve.CORE");
				imports.add("org.skyve.domain.Bean");
				imports.add("org.skyve.domain.HierarchicalBean");
				imports.add("org.skyve.persistence.DocumentQuery");
				imports.add("org.skyve.persistence.Persistence");
			}
			else {
				imports.add("modules." + module.getName() + ".domain." + parentDocumentName);
				imports.add("org.skyve.domain.ChildBean");
			}
		}
		// indicates if the base document has <BaseDocument>Extension.java defined in the document folder.
		boolean baseDocumentExtensionClassExists = false;
		if (baseDocumentName != null) {
			Document baseDocument = module.getDocument(customer, baseDocumentName);
			String baseDocumentExtensionPath = String.format("%s%s%s/%s/%sExtension.java", 
																SRC_PATH, 
																repository.MODULES_NAMESPACE,
																baseDocument.getOwningModuleName(),
																baseDocumentName,
																baseDocumentName);
			baseDocumentExtensionClassExists = new File(baseDocumentExtensionPath).exists();
			if (baseDocumentExtensionClassExists) {
				imports.add("modules." + baseDocument.getOwningModuleName() + '.' + baseDocumentName + '.' + baseDocumentName + "Extension");
			}
			else {
				imports.add("modules." + baseDocument.getOwningModuleName() + ".domain." + baseDocumentName);
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
							(! documentClass.attributes.containsKey(conditionName))) {
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
					methods.append("\n\t/**");
					methods.append("\n\t * ").append(description);
					methods.append("\n\t */");

					methods.append("\n\t@XmlTransient");
					if (overriddenCondition) {
						methods.append("\n\t@Override");
					}
					methods.append("\n\tpublic boolean is").append(Character.toUpperCase(conditionName.charAt(0)));
					methods.append(conditionName.substring(1)).append("() {\n");
					methods.append("\t\treturn (").append(condition.getExpression()).append(");\n");
					methods.append("\t}\n");

					if (overriddenCondition) {
						methods.append("\n\t@Override");
					}
					methods.append("\n\tpublic boolean isNot").append(Character.toUpperCase(conditionName.charAt(0)));
					methods.append(conditionName.substring(1)).append("() {\n");
					methods.append("\t\treturn (! is").append(Character.toUpperCase(conditionName.charAt(0)));
					methods.append(conditionName.substring(1)).append("());\n");
					methods.append("\t}\n");
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
				methods.append("\n\t@Override\n");
				methods.append("\tpublic String getBizParentId() {\n");
				methods.append("\t\treturn bizParentId;\n");
				methods.append("\t}\n");
	
				// Mutator method
				methods.append("\n\t@Override\n");
				methods.append("\t@XmlElement\n");
				methods.append("\tpublic void setBizParentId(String bizParentId) {\n");
				methods.append("\t\tpreset(HierarchicalBean.PARENT_ID, bizParentId);\n");
				methods.append("\t\tthis.bizParentId = bizParentId;\n");
				methods.append("\t}\n");

				// Traversal method
				methods.append("\n\t@Override\n");
				methods.append("\tpublic ").append(documentName).append(" getParent() {\n");
				methods.append("\t\t").append(documentName).append(" result = null;\n\n");
				methods.append("\t\tif (bizParentId != null) {\n");
				methods.append("\t\t\tPersistence p = CORE.getPersistence();\n");
				methods.append("\t\t\tDocumentQuery q = p.newDocumentQuery(").append(documentName).append(".MODULE_NAME, ").append(documentName).append(".DOCUMENT_NAME);\n");
				methods.append("\t\t\tq.getFilter().addEquals(Bean.DOCUMENT_ID, bizParentId);\n");
				methods.append("\t\t\tresult = q.retrieveBean();\n");
				methods.append("\t\t}\n\n");
				methods.append("\t\treturn result;\n");
				methods.append("\t}\n");
				
				// Traversal method
				methods.append("\n\t@Override\n");
				methods.append("\t@XmlTransient\n");
				methods.append("\tpublic List<").append(documentName).append("> getChildren() {\n");
				methods.append("\t\tPersistence p = CORE.getPersistence();\n");
				methods.append("\t\tDocumentQuery q = p.newDocumentQuery(").append(documentName).append(".MODULE_NAME, ").append(documentName).append(".DOCUMENT_NAME);\n");
				methods.append("\t\tq.getFilter().addEquals(HierarchicalBean.PARENT_ID, bizParentId);\n");
				methods.append("\t\treturn q.beanResults();\n");
				methods.append("\t}\n");
			}
			else {
				attributes.append("\tprivate ").append(parentDocumentName).append(" parent;\n\n");
	
				// Accessor method
				methods.append("\n\t@Override\n");
				methods.append("\tpublic ").append(parentDocumentName).append(" getParent() {\n");
				methods.append("\t\treturn parent;\n");
				methods.append("\t}\n");
	
				// Mutator method
				methods.append("\n\t@Override\n");
				methods.append("\t@XmlElement\n");
				methods.append("\tpublic void setParent(");
				methods.append(parentDocumentName).append(" parent) {\n");
				methods.append("\t\tpreset(ChildBean.PARENT_NAME, parent);\n");
				methods.append("\t\tthis.parent = ").append(" parent;\n");
				methods.append("\t}\n");
	
				// BizOrdinal property
				imports.add("org.skyve.domain.Bean");
				attributes.append("\tprivate Integer bizOrdinal;\n\n");
	
				// Accessor method
				methods.append("\n\t@Override\n");
				methods.append("\tpublic Integer getBizOrdinal() {\n");
				methods.append("\t\treturn bizOrdinal;\n");
				methods.append("\t}\n");
	
				// Mutator method
				methods.append("\n\t@Override\n");
				methods.append("\t@XmlElement\n");
				methods.append("\tpublic void setBizOrdinal(Integer bizOrdinal) {\n");
				methods.append("\t\tpreset(Bean.ORDINAL_NAME, bizOrdinal);\n");
				methods.append("\t\tthis.bizOrdinal = ").append(" bizOrdinal;\n");
				methods.append("\t}\n");
			}
		}

		for (String importClassName : imports) {
			fw.append("import ").append(importClassName).append(";\n");
		}

		// Generate/Include UML doc
		String description = document.getDocumentation();
		if (description == null) {
			description = document.getDescription();
		}
		if (description == null) {
			description = document.getName();
		}
		fw.append("\n/**");
		fw.append("\n * ").append(description);
		fw.append("\n * \n");

		for (Attribute attribute : document.getAttributes()) {
			if (attribute instanceof Enumeration) {
				Enumeration enumeration = (Enumeration) attribute;
				fw.append(" * @depend - - - ").append(enumeration.toJavaIdentifier()).append('\n');
			}
		}

		for (String referenceName : document.getReferenceNames()) {
			Reference reference = document.getReferenceByName(referenceName);
			ReferenceType type = reference.getType();
			boolean required = reference.isRequired();
			if (AssociationType.aggregation.equals(type)) {
				fw.append(" * @navhas n ").append(referenceName).append(required ? " 1 " : " 0..1 ").append(reference.getDocumentName()).append('\n');
			}
			else if (AssociationType.composition.equals(type)) {
				fw.append(" * @navcomposed n ").append(referenceName).append(required ? " 1 " : " 0..1 ").append(reference.getDocumentName()).append('\n');
			}
			else {
				if (CollectionType.aggregation.equals(type)) {
					fw.append(" * @navhas n ");
				}
				else if (CollectionType.composition.equals(type)) {
					fw.append(" * @navcomposed n ");
				}
				else if (CollectionType.child.equals(type)) {
					fw.append(" * @navcomposed 1 ");
				}
				fw.append(referenceName);
				Collection collection = (Collection) reference;
				Integer min = collection.getMinCardinality();
				fw.append(' ').append(min.toString()).append("..");
				Integer max = collection.getMaxCardinality();
				if (max == null) {
					fw.append("n ");
				}
				else {
					fw.append(max.toString()).append(' ');
				}
				fw.append(reference.getDocumentName()).append('\n');
			}
		}
		if (persistent == null) {
			fw.append(" * @stereotype \"transient");
		}
		else {
			fw.append(" * @stereotype \"persistent");
		}
		if (parentDocumentName != null) {
			fw.append(" child\"\n");
//			fw.append(" * @navassoc 1 parent 1 ").append(parentDocumentName).append('\n');
		}
		else {
			fw.append("\"\n");
		}
		
		fw.append(" */\n");
		
		
		// generate class body
		fw.append("@XmlType");
		fw.append("\n@XmlRootElement");
		fw.append("\npublic ");
		if (baseDocumentName == null) {
			TreeMap<String, DomainClass> domainClasses = moduleDocumentVanillaClasses.get(module.getName());
			DomainClass domainClass = (domainClasses == null) ? null : domainClasses.get(documentName);
			if ((domainClass != null) && (domainClass.isAbstract)) {
				fw.append("abstract ");
			}
		}
		fw.append("class ").append(documentName);
		if (baseDocumentName != null) { // extension
			if (overridden) {
				fw.append("Ext");
			}
			
			if (baseDocumentExtensionClassExists) {
				fw.append(" extends ").append(baseDocumentName).append("Extension");
			}
			else {
				fw.append(" extends ").append(baseDocumentName);
			}
		}
		else {
			fw.append(" extends Abstract").append((persistent == null) ? "TransientBean" : "PersistentBean");
		}
		if (parentDocumentName != null) {
			if (parentDocumentName.equals(documentName)) { // hierarchical
				fw.append(" implements HierarchicalBean<").append(parentDocumentName).append('>');
			}
			else {
				fw.append(" implements ChildBean<").append(parentDocumentName).append('>');
			}
		}
		fw.append(" {\n");

		fw.append("\t/**\n");
		fw.append("\t * For Serialization\n");
		fw.append("\t * @hidden\n");
		fw.append("\t */\n");
		fw.append("\tprivate static final long serialVersionUID = 1L;\n\n");

		fw.append(statics).append("\n");
		if (enums.length() > 0) {
			fw.append(enums);
		}
		fw.append(attributes);
		fw.append(methods);

		fw.append("}\n");
	}
	
	private static void attributeJavadoc(Attribute attribute, StringBuilder toAppendTo) {
		String description = attribute.getDocumentation();
		if (description == null) {
			description = attribute.getDescription();
		}
		if (description != null) {
			toAppendTo.append("\t/**\n");
			toAppendTo.append("\t * ").append(description).append("\n");
			toAppendTo.append("\t **/\n");
		}
	}
	
	private static void accessorJavadoc(Attribute attribute, StringBuilder toAppendTo, boolean mapped) {
		toAppendTo.append("\n\t/**\n");
		toAppendTo.append("\t * {@link #").append(attribute.getName()).append("} accessor.\n");
		if (mapped) {
			toAppendTo.append("\t * \n");
			toAppendTo.append("\t * @param bizId\tThe bizId of the element in the list.\n");
		}
		toAppendTo.append("\t **/");
	}

	private static void mutatorJavadoc(Attribute attribute, StringBuilder toAppendTo, boolean mapped) {
		toAppendTo.append("\n\t/**\n");
		toAppendTo.append("\t * {@link #").append(attribute.getName()).append("} mutator.\n");
		toAppendTo.append("\t * \n");
		if (mapped) {
			toAppendTo.append("\t * @param bizId\tThe bizId of the element in the list.\n");
		}
		toAppendTo.append("\t * @param ").append(attribute.getName()).append("\tThe new value to set.\n");
		toAppendTo.append("\t **/");
	}
}
