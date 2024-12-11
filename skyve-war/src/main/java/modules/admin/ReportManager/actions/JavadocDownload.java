package modules.admin.ReportManager.actions;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.skyve.CORE;
import org.skyve.content.MimeType;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.model.document.field.LengthField;
import org.skyve.metadata.controller.Download;
import org.skyve.metadata.controller.DownloadAction;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.metadata.model.document.Condition;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.DomainType;
import org.skyve.metadata.model.document.Reference;
import org.skyve.metadata.model.document.UniqueConstraint;
import org.skyve.metadata.module.JobMetaData;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.metadata.module.query.QueryDefinition;
import org.skyve.metadata.user.Role;
import org.skyve.report.Reporting;
import org.skyve.web.WebContext;

import jakarta.inject.Inject;
import modules.admin.ReportManager.ReportManagerExtension;

/**
 * This class is used to generate and download the application's javadoc in a user friendly PDF, similar to the javadoc run
 * configuration.
 */
public class JavadocDownload extends DownloadAction<ReportManagerExtension> {

	private static String REPORT_TITLE = "System Documentation";
	private static String REPORT_NAME = "applicationJavadoc.html";
	private static String DEFAULT_COLOR = "#106FA9";
	private static String DEFAULT_DARK_COLOR = "#042840";
	private static String DEFAULT_LIGHT_COLOR = "#d5e9f2";
	
	@Inject
	private transient Reporting reportService;

	@Override
	public void prepare(ReportManagerExtension bean, WebContext webContext) throws Exception {
		// Nothing to do here for now

	}

	@Override
	public Download download(ReportManagerExtension bean, WebContext webContext) throws Exception {

		File pdfFile = getJavadocPDFFile(bean);
		return new Download(String.format("%s.pdf", REPORT_TITLE), pdfFile, MimeType.pdf);
	}

	/**
	 * Generates a PDF for the application. Assumes there's a file named 'applicationJavadoc.ftl' in the directory.
	 * 
	 * @param bean
	 * 
	 * @return A File to the generated PDF on disk
	 * @throws Exception
	 */
	public File getJavadocPDFFile(ReportManagerExtension bean) throws Exception {

		Map<String, Object> root = new HashMap<>();

		Customer customer = CORE.getCustomer();

		List<Map<String, Object>> modules = new ArrayList<>();
		
		// Example: Populate modules with document data
		for (org.skyve.metadata.module.Module module : customer.getModules()) {
			Map<String, Object> moduleData = new HashMap<>();
			moduleData.put("name", module.getLocalisedTitle());
			
			// Prepare documents
			List<Map<String, Object>> documents = new ArrayList<>();
			for (String documentName : module.getDocumentRefs()
					.keySet()) {
				Document document = module.getDocument(customer, documentName);
				Map<String, Object> documentData = new HashMap<>();
				documentData.put("name", document.getName());
				documentData.put("singularAlias", document.getSingularAlias());
				documentData.put("documentation", document.getDocumentation());

				// Include attributes
				List<Map<String, Object>> attributes = new ArrayList<>();
				for (Attribute attribute : document.getAttributes()) {
					Map<String, Object> attributeData = new HashMap<>();
					attributeData.put("name", attribute.getName());
					attributeData.put("displayName", attribute.getLocalisedDisplayName());
					attributeData.put("type", attribute.getAttributeType());
					attributeData.put("type", attribute.getAttributeType());
					attributeData.put("required", Boolean.valueOf(attribute.isRequired()));
					attributeData.put("persistent", Boolean.valueOf(attribute.isPersistent()));
					attributeData.put("description", attribute.getLocalisedDescription());
					attributeData.put("documentation", attribute.getDocumentation());
					attributeData.put("deprecated", Boolean.valueOf(attribute.isDeprecated()));

					Integer size = Integer.valueOf(0);
					if (attribute instanceof LengthField) {
						LengthField lengthField = (LengthField) attribute;
						size = Integer.valueOf(lengthField.getLength());
					}
					if (AttributeType.enumeration.equals(attribute.getAttributeType())
							|| DomainType.constant.equals(attribute.getDomainType())) {
						List<String> values = new ArrayList<>();
						try {
							for (DomainValue val : ((DocumentImpl) document).getDomainValues((CustomerImpl) customer,
									attribute.getDomainType(), attribute, null, false)) {
								StringBuilder sb = new StringBuilder();
								sb.append(val.getLocalisedDescription())
										.append(" (")
										.append(val.getCode())
										.append(")");
								values.add(sb.toString());
								if (val.getCode()
										.length() > size.intValue()) {
									size = Integer.valueOf(val.getCode()
											.length());
								}
							}
							attributeData.put("values", values);
						} catch (@SuppressWarnings("unused") Exception e) {
							System.err.println(String.format(
									"cannot get domain values for module %s, document %s & attribute %s. This can occur when a database is required to determine the values. ",
									module.getName(), document.getName(), attribute.getName()));
						}
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
					conditionData.put("name", conditionName);
					conditionData.put("documentation", condition.getDocumentation());
					conditions.add(conditionData);
				}
				documentData.put("conditions", conditions);

				// Include constraints
				List<Map<String, Object>> constraints = new ArrayList<>();
				for (UniqueConstraint constraint : document.getUniqueConstraints()) {
					Map<String, Object> constraintData = new HashMap<>();
					constraintData.put("name", constraint.getName());
					constraintData.put("description", constraint.getDescription());
					constraintData.put("scope", constraint.getScope());
					StringBuilder s = new StringBuilder();
					for (String r : constraint.getFieldNames()) {
						if (s.toString()
								.length() > 0) {
							s.append(", ");
						}
						Attribute a = getAttributeFromFieldName(document, r);
						if (a != null) {
							s.append(a.getLocalisedDisplayName());
						} else {
							s.append(r);
						}
					}
					constraintData.put("references", s.toString());
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
						(query instanceof MetaDataQueryDefinition) ? ((MetaDataQueryDefinition) query).getDocumentName() : null);
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

		// put all the datasets into the root
		root.put("title", REPORT_TITLE);
		root.put("data", data);
		root.put("defaultColor", DEFAULT_COLOR);
		root.put("defaultDarkColor", DEFAULT_DARK_COLOR);
		root.put("defaultLightColor", DEFAULT_LIGHT_COLOR);

		File pdfFile = null;
		pdfFile = reportService.createFreemarkerBeanReportPDF(bean, REPORT_NAME, root, REPORT_TITLE);

		return pdfFile;
	}

	/**
	 * Returns an attribute from a binding string within the context of a
	 * document.
	 * 
	 * @param d
	 * @param f
	 * @return
	 */
	private static Attribute getAttributeFromFieldName(Document d, String f) {
		// TODO - do this properly for all bindings
		Attribute result = null;
		System.out.println("getAttributeFromFieldName " + d.getName() + f);
		for (Attribute a : d.getAttributes()) {
			if (f.equals(a.getName())) {
				result = a;
				System.out.println("found attribute " + result.getLocalisedDisplayName());
				break;
			}
		}
		return result;
	}

}
