package org.skyve.toolchain;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Base64;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import java.util.stream.Stream;

import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugins.annotations.Execute;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.cache.CSRFTokenCacheConfig;
import org.skyve.cache.ConversationCacheConfig;
import org.skyve.cache.GeoIPCacheConfig;
import org.skyve.cache.SessionCacheConfig;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.model.document.field.LengthField;
import org.skyve.impl.metadata.repository.DefaultRepository;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.impl.metadata.user.SuperUser;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.persistence.RDBMSDynamicPersistence;
import org.skyve.impl.persistence.hibernate.HibernateContentPersistence;
import org.skyve.impl.report.freemarker.FreemarkerReportUtil;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.Attribute.UsageType;
import org.skyve.metadata.model.Extends;
import org.skyve.metadata.model.document.Association;
import org.skyve.metadata.model.document.Association.AssociationType;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.metadata.model.document.Collection;
import org.skyve.metadata.model.document.Collection.CollectionType;
import org.skyve.metadata.model.document.Condition;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.DomainType;
import org.skyve.metadata.model.document.Interface;
import org.skyve.metadata.model.document.Reference;
import org.skyve.metadata.model.document.UniqueConstraint;
import org.skyve.metadata.module.JobMetaData;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.metadata.module.query.QueryDefinition;
import org.skyve.metadata.repository.ProvidedRepository;
import org.skyve.metadata.user.Role;
import org.skyve.persistence.DataStore;
import org.skyve.toolchain.freemarker.PlantUMLDirective;
import org.skyve.util.FileUtil;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Generates the Skyve system documentation report and exports it as a PDF.
 *
 * <p>Threading: this mojo mutates global Skyve bootstrap state, repository wiring, caches, and persistence
 * configuration; it must be treated as thread-confined.
 */
@Mojo(name = "systemDocumentation") //, requiresDependencyResolution = ResolutionScope.TEST)
@Execute(phase = LifecyclePhase.PROCESS_RESOURCES)
@SuppressWarnings("java:S1192") // Repeated literals are deliberate system-documentation metadata keys.
public class SystemDocumentationMojo extends AbstractSkyveMojo {
	private static final Logger LOGGER = LoggerFactory.getLogger(SystemDocumentationMojo.class);

	private static final String REPORT_TITLE = "System Documentation";
	private static final String REPORT_FILENAME = "systemDocumentation.html";
	private static final String DEFAULT_COLOUR = "#106FA9";
	private static final String DEFAULT_DARK_COLOUR = "#042840";
	private static final String DEFAULT_LIGHT_COLOUR = "#d5e9f2";

	/**
	 * Relative source directory.
	 */
	@Parameter(required = true, defaultValue = "src/main/java/")
	private String srcDir;

	/**
	 * Relative generated directory.
	 */
	@Parameter(required = true, defaultValue = "src/generated/java/")
	private String generatedDir;

	/**
	 * Relative test directory.
	 */
	@Parameter(required = true, defaultValue = "src/test/java/")
	private String testDir;

	/**
	 * Relative generated test directory.
	 */
	@Parameter(required = true, defaultValue = "src/generatedTest/java/")
	private String generatedTestDir;

	@Parameter(required = true, defaultValue = "skyve")
	private String customer;
	
	@Parameter(property = "excludedModules")
	private String excludedModules;

	/**
	 * Bootstraps a temporary reporting environment, renders the HTML report, and copies the generated PDF into
	 * the target directory.
	 *
	 * <p>Side effects: mutates Skyve global singletons, creates temporary files, and writes
	 * {@code target/system-documentation.pdf}.
	 *
	 * @throws MojoExecutionException if report generation fails
	 */
	@Override
	@SuppressWarnings("java:S2696") // set statics here as this is single threaded
	public void execute() throws MojoExecutionException {
		try {
			configureClasspath(srcDir);
			
	        String template = null;
	        try (InputStream is = getClass().getResourceAsStream(REPORT_FILENAME)) {
	        	template = new String(is.readAllBytes(), StandardCharsets.UTF_8);
	        }
			
	        FreemarkerReportUtil.init();
			FreemarkerReportUtil.addTemplate(REPORT_FILENAME, template);
			FreemarkerReportUtil.addDirective("plantUmlImage", new PlantUMLDirective());
			
			String driver = "org.h2.Driver";
			String url = "jdbc:h2:mem:doco";
			String name = "user";
			String pwd = "password";
			String dialect = "org.skyve.impl.persistence.hibernate.dialect.H2SpatialDialect";
			UtilImpl.DATA_STORE = new DataStore(driver, url, name, pwd, dialect);
			UtilImpl.DATA_STORES.put("doco", UtilImpl.DATA_STORE);
			UtilImpl.CONTENT_DIRECTORY = "./target/";

			UtilImpl.CONVERSATION_CACHE = new ConversationCacheConfig(10, 0, 0, 60);
			UtilImpl.CSRF_TOKEN_CACHE = new CSRFTokenCacheConfig(10, 0, 0, 60);
			UtilImpl.SESSION_CACHE = new SessionCacheConfig(10, 0, 0, 60);
			UtilImpl.GEO_IP_CACHE = new GeoIPCacheConfig(10, 0, 0, 60);

				ProvidedRepositoryFactory.set(newRepository());
				Customer c = getCustomer(customer);
				if (c == null) {
					throw new IllegalStateException("Customer " + customer + " does not exist");
				}

			final SuperUser user = new SuperUser();
			user.setCustomerName(customer);
			user.setName("DocoUser");
			user.setId("DocoUser");

			AbstractPersistence.IMPLEMENTATION_CLASS = HibernateContentPersistence.class;
			AbstractPersistence.DYNAMIC_IMPLEMENTATION_CLASS = RDBMSDynamicPersistence.class;

				final AbstractPersistence persistence = getPersistence();
				persistence.setUser(user);

				File temp = pdf();
				copy(temp, new File("./target/system-documentation.pdf"));
				delete(new File("./target/temp"));
			
		}
		catch (Exception e) {
			LOGGER.error("Failed to generate system documentation.");
			throw new MojoExecutionException("Failed to generate system documentation.", e);
		}
	}
	
	/**
	 * Renders the system documentation report to a PDF file.
	 *
	 * @return the generated PDF file on disk
	 * @throws Exception if report rendering fails
	 */
	public File pdf() throws Exception {
		Map<String, Object> parameters = prepareParameters();
		return EXT.getReporting().createFreemarkerReportPDF(REPORT_FILENAME, parameters, "system-documentation");
	}

	/**
	 * Builds the data model consumed by the system documentation template.
	 *
	 * @return the template parameter map
	 * @throws IOException if report assets cannot be loaded
	 */
	@SuppressWarnings({"java:S3776", "java:S6541"}) // complexity OK
	private Map<String, Object> prepareParameters() throws IOException {
		Map<String, Object> result = new HashMap<>();
		Customer c = getCustomer();

		String[] excludedModuleNames = (excludedModules == null) ? new String[0] : excludedModules.split(",");

		// Populate modules
		List<Map<String, Object>> modules = new ArrayList<>();
		for (Module module : c.getModules()) {
			String moduleName = module.getName();

			// Check if this module is excluded
			boolean excluded = (excludedModuleNames.length > 0) &&
									Stream.of(excludedModuleNames).anyMatch(moduleName::equals);
			if (excluded) {
				continue;
			}
			
			Map<String, Object> moduleData = new HashMap<>();
			moduleData.put("name", module.getLocalisedTitle());

			// Prepare documents
			List<Map<String, Object>> documents = new ArrayList<>();
			for (String documentName : module.getDocumentRefs().keySet()) {
				Document document = module.getDocument(c, documentName);
				Map<String, Object> documentData = new HashMap<>();
				documentData.put("name", document.getName());
				documentData.put("singularAlias", document.getSingularAlias());
				documentData.put("documentation", document.getDocumentation());
				documentData.put("diagram", generateDiagram(document, c));

				// Include attributes
				List<Map<String, Object>> attributes = new ArrayList<>();
				for (Attribute attribute : document.getAttributes()) {
					Map<String, Object> attributeData = new HashMap<>();
					attributeData.put("name", attribute.getName());
					attributeData.put("displayName", attribute.getLocalisedDisplayName());
					attributeData.put("type", attribute.getAttributeType());
					attributeData.put("required", Boolean.valueOf(attribute.isRequired()));
					attributeData.put("persistent", Boolean.valueOf(attribute.isPersistent()));
					attributeData.put("description", attribute.getLocalisedDescription());
					attributeData.put("documentation", attribute.getDocumentation());
					attributeData.put("deprecated", Boolean.valueOf(attribute.isDeprecated()));

					// Set size if we have a length
					Integer size = Integer.valueOf(0);
					if (attribute instanceof LengthField field) {
						size = Integer.valueOf(field.getLength());
					}
					
					// Set size and values if this is an enumeration or a constant domain
					DomainType domainType = attribute.getDomainType();
					if (AttributeType.enumeration.equals(attribute.getAttributeType()) ||
							DomainType.constant.equals(domainType)) {
						List<String> values = new ArrayList<>();

						try {
							for (DomainValue val : ((DocumentImpl) document).getDomainValues((CustomerImpl) c,
																								domainType,
																								attribute,
																								null,
																								false)) {
								StringBuilder sb = new StringBuilder(128);
								sb.append(val.getLocalisedDescription()).append(" (").append(val.getCode()).append(")");
								values.add(sb.toString());
								if (val.getCode().length() > size.intValue()) {
									size = Integer.valueOf(val.getCode().length());
								}
							}
						}
						catch (Exception e) {
							LOGGER.error("Cannot determine domain values for {}.{}.{}", moduleName, documentName, attribute.getName(), e);
						}
						attributeData.put("values", values);
					}
					attributeData.put("size", size);
					attributes.add(attributeData);
				}
				documentData.put("attributes", attributes);

				// Include references
				List<Map<String, Object>> references = new ArrayList<>();
				for (String referenceName : document.getReferenceNames()) {
					Map<String, Object> referenceData = new HashMap<>();
					Reference reference = document.getReferenceByName(referenceName);
					referenceData.put("name", referenceName);
					referenceData.put("type", reference.getType());
					referenceData.put("document", reference.getDocumentName());
					referenceData.put("required", Boolean.valueOf(reference.isRequired()));
					referenceData.put("description", reference.getDescription());
					referenceData.put("documentation", reference.getDocumentation());
					referenceData.put("deprecated", Boolean.valueOf(reference.isDeprecated()));
					references.add(referenceData);
				}
				documentData.put("references", references);

				// Include conditions
				List<Map<String, Object>> conditions = new ArrayList<>();
				for (String conditionName : document.getConditionNames()) {
					Map<String, Object> conditionData = new HashMap<>();
					Condition condition = document.getCondition(conditionName);
					if (condition != null) {
						conditionData.put("name", conditionName);
						conditionData.put("documentation", condition.getDocumentation());
						conditions.add(conditionData);
					}
				}
				documentData.put("conditions", conditions);

				// Include constraints
				List<Map<String, Object>> constraints = new ArrayList<>();
				for (UniqueConstraint constraint : document.getUniqueConstraints()) {
					Map<String, Object> constraintData = new HashMap<>();
					constraintData.put("name", constraint.getName());
					constraintData.put("description", constraint.getDescription());
					constraintData.put("scope", constraint.getScope());
					StringBuilder fields = new StringBuilder(128);
					for (String fieldName : constraint.getFieldNames()) {
						if (! fields.isEmpty()) {
							fields.append(", ");
						}
						Attribute attribute = document.getPolymorphicAttribute(c, fieldName);
						if (attribute != null) {
							fields.append(attribute.getLocalisedDisplayName());
						}
						else {
							fields.append(fieldName);
						}
					}
					constraintData.put("references", fields.toString());
					constraints.add(constraintData);
				}
				documentData.put("constraints", constraints);

				// Include actions
				List<Map<String, Object>> actions = new ArrayList<>();
				for (String actionName : document.getDefinedActionNames()) {
					Map<String, Object> actionData = new HashMap<>();
					actionData.put("name", actionName);
				}
				documentData.put("actions", actions);

				documents.add(documentData);
			}
			moduleData.put("documents", documents);

			// Add module queries
			List<Map<String, Object>> queries = new ArrayList<>();
			for (QueryDefinition query : module.getMetadataQueries()) {
				Map<String, Object> queryData = new HashMap<>();
				queryData.put("name", query.getName());
				queryData.put("drivingDocument",
								(query instanceof MetaDataQueryDefinition metaDataQuery) ? 
									metaDataQuery.getDocumentName() : 
									null);
				queryData.put("description", query.getLocalisedDescription());
				queryData.put("documentation", query.getDocumentation());
				queries.add(queryData);
			}
			moduleData.put("queries", queries);

			// Add module roles
			List<Map<String, Object>> roles = new ArrayList<>();
			for (Role role : module.getRoles()) {
				Map<String, Object> rowData = new HashMap<>();
				rowData.put("name", role.getName());
				rowData.put("description", role.getLocalisedDescription());
				rowData.put("documentation", role.getDocumentation());
				queries.add(rowData);
			}
			moduleData.put("roles", roles);

			// Add module jobs
			List<Map<String, Object>> jobs = new ArrayList<>();
			for (JobMetaData job : module.getJobs()) {
				Map<String, Object> jobData = new HashMap<>();
				jobData.put("name", job.getName());
				jobData.put("displayName", job.getLocalisedDisplayName());
				jobData.put("className", job.getClassName());
				jobs.add(jobData);
			}
			moduleData.put("jobs", jobs);

			modules.add(moduleData);
		}

		// Add modules to the data model
		Map<String, Object> data = new HashMap<>();
		data.put("modules", modules);

		String version = UtilImpl.processStringValue(project.getVersion());
		String description = UtilImpl.processStringValue(project.getDescription());
		if (description == null) {
			description = UtilImpl.processStringValue(project.getName());
		}
		if (description == null) {
			description = UtilImpl.processStringValue(project.getArtifactId());
		}
		
        String base64Logo = null;
        try (InputStream is = getClass().getResourceAsStream("skyve-logo-black.png")) {
        	base64Logo = Base64.getEncoder().encodeToString(is.readAllBytes());
        }

		// put all the datasets into the root
		result.put("title", REPORT_TITLE);
		result.put("logo", base64Logo);
		result.put("data", data);
		result.put("pomVersion", (version == null) ? "Unknown" : version);
		result.put("projectName", (description == null) ? "Unnamed System" : description);
		result.put("defaultColor", DEFAULT_COLOUR);
		result.put("defaultDarkColor", DEFAULT_DARK_COLOUR);
		result.put("defaultLightColor", DEFAULT_LIGHT_COLOUR);
		
		return result;
	}

	@SuppressWarnings("static-method") // test seam
	Customer getCustomer() {
		return CORE.getCustomer();
	}

	@SuppressWarnings("static-method") // test seam
	Customer getCustomer(String customerName) {
		return ProvidedRepositoryFactory.get().getCustomer(customerName);
	}

	@SuppressWarnings("static-method") // test seam
	ProvidedRepository newRepository() {
		return new DefaultRepository();
	}

	@SuppressWarnings("static-method") // test seam
	AbstractPersistence getPersistence() {
		return AbstractPersistence.get();
	}

	@SuppressWarnings("static-method") // test seam
	void copy(File source, File destination) throws IOException {
		FileUtil.copy(source, destination);
	}

	@SuppressWarnings("static-method") // test seam
	void delete(File file) throws IOException {
		FileUtil.delete(file);
	}
	
	/**
	 * Generates a PlantUML diagram for the supplied document when the document has relationships to model.
	 *
	 * @param document the document to describe
	 * @param customer the current customer context used for attribute resolution
	 * @return the generated PlantUML markup, or {@code null} when no relationships are present
	 */
	@SuppressWarnings("java:S3776") // complexity OK
	private static String generateDiagram(final Document document, final Customer customer) {
		boolean noDiagram = true; // determines whether to return a diagram or null

		String documentName = "\"" + document.getName() + "\"";

		Set<String> uniqueRelatedNames = new TreeSet<>();

		StringBuilder relations = new StringBuilder();

		// append any associations and collections
		for (Attribute att : document.getAllAttributes(customer)) {
			// Skip view relations
			if (UsageType.view.equals(att.getUsage())) {
				continue;
			}

			if (att instanceof Association assoc) {
				noDiagram = false;

				String associationDocumentName = "\"" + assoc.getDocumentName() + "\"";
				uniqueRelatedNames.add(associationDocumentName);

				relations.append(documentName);
				relations.append(AssociationType.aggregation.equals(assoc.getType()) ? " o--> " : " *--> ");
				relations.append((assoc.isRequired()) ? "\"1\" " : "\"0..1\" ");
				relations.append(associationDocumentName).append(" : ").append(assoc.getName()).append("\n");
			}
			else if (att instanceof Collection coll) {
				noDiagram = false;

				String collectionDocumentName = "\"" + coll.getDocumentName() + "\"";
				uniqueRelatedNames.add(collectionDocumentName);

				relations.append(documentName);
				relations.append(CollectionType.aggregation.equals(coll.getType()) ? " o--> " : " *--> ");

				Integer maxCardinality = coll.getMaxCardinality();
				String maxCardinalityString = (maxCardinality != null) ? maxCardinality.toString() : "*";
				relations.append('\"');
				if (! coll.isRequired()) {
					relations.append("0..");
				}
				relations.append(maxCardinalityString).append("\" ");

				relations.append(collectionDocumentName).append(" : ").append(coll.getName()).append("\n");
			}
		}

		// model the parent if there is one
		String parentDocument = document.getParentDocumentName();
		if (parentDocument != null) {
			noDiagram = false;

			String parentDocumentName = "\"" + parentDocument + "\"";
			uniqueRelatedNames.add(parentDocumentName);
			relations.append(documentName).append(" o--> \"0..1\" ").append(parentDocumentName).append(" : parent\n");
		}
		
		// model extends
		Extends inherits = document.getExtends();
		if (inherits != null) {
			noDiagram = false;

			String extendsDocumentName = "\"" + inherits.getDocumentName() + "\"";
			uniqueRelatedNames.add(extendsDocumentName);
			relations.append(extendsDocumentName).append(" <|-- ").append(documentName).append("\n");
		}

		// model implements
		for (Interface implement : document.getInterfaces()) {
			noDiagram = false;

			String implementsClassName = "\"" + implement.getInterfaceName() + "\"";
			uniqueRelatedNames.add(implementsClassName);
			relations.append(implementsClassName).append(" <|.. ").append(documentName).append("\n");
		}
		
		if (noDiagram) {
			return null;
		}

		return outputDiagram(document, (parentDocument != null), uniqueRelatedNames, relations.toString());
	}
	
	/**
	 * Wraps the PlantUML relation block in a full diagram definition.
	 *
	 * @param document the primary document being rendered
	 * @param child whether the document has a parent relationship
	 * @param relatedNames the related document and interface names to declare
	 * @param relations the relation statements to append
	 * @return the full PlantUML diagram text
	 */
	private static String outputDiagram(Document document,
											boolean child,
											Set<String> relatedNames,
											String relations) {
		StringBuilder result = new StringBuilder(512);

		result.append("!pragma layout smetana\n");
		result.append("@startuml\n");
		result.append("hide members\n");
		result.append("hide methods\n");

		// append this document name
		result.append("class ").append(document.getName());
		result.append(document.isPersistable() ? "<<Persistent" : "<<Non-Persistent");
		if (child) {
			result.append(" Child");
		}
		result.append(">>\n");
		
		// append the referenced document names
		for (String name : relatedNames) {
			@SuppressWarnings("java:S2692") // must be > 0
			boolean interfaceName = (name.indexOf('.') > 0); // interface class name, not a document
			result.append(interfaceName ? "interface " : "class ");
			result.append(name).append("\n");
		}

		// append relations
		result.append(relations);

		result.append("@enduml");

		return result.toString();
	}
}
