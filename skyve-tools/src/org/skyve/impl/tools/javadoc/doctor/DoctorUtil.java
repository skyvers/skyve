package org.skyve.impl.tools.javadoc.doctor;

import java.io.FileOutputStream;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.List;

import org.skyve.domain.Bean;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.model.document.field.LengthField;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.impl.metadata.repository.LocalDesignRepository;
import org.skyve.impl.metadata.user.ActionPrivilege;
import org.skyve.impl.metadata.user.DocumentPrivilege;
import org.skyve.impl.metadata.user.Privilege;
import org.skyve.impl.metadata.user.RoleImpl;
import org.skyve.impl.tools.javadoc.doctor.DocSection.SectionType;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.Attribute.UsageType;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.metadata.model.document.Condition;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.DomainType;
import org.skyve.metadata.model.document.Reference;
import org.skyve.metadata.model.document.UniqueConstraint;
import org.skyve.metadata.module.JobMetaData;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.BizQLDefinition;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.metadata.module.query.MetaDataQueryProjectedColumn;
import org.skyve.metadata.module.query.QueryDefinition;
import org.skyve.metadata.module.query.SQLDefinition;
import org.skyve.metadata.repository.ProvidedRepository;
import org.skyve.metadata.module.query.MetaDataQueryColumn;
import org.skyve.metadata.user.DocumentPermission;
import org.skyve.metadata.user.Role;

/**
 * DoctorUtil contains methods for the creation of documentation, based on the
 * configuration options defined in the Configuration document.
 * <p>
 * 
 * 
 * @author Rob
 * 
 */
public class DoctorUtil {

	public static enum OutputFormat {
		debug, template, htmlSet, htmlDoc
	}

	public static final List<DomainValue> OUTPUT_FORMATS = new ArrayList<>();

	static {
		OUTPUT_FORMATS.add(new DomainValue(OutputFormat.debug.toString(), "Debug to System.out"));
		OUTPUT_FORMATS.add(new DomainValue(OutputFormat.htmlSet.toString(), "HTML Project"));
		OUTPUT_FORMATS.add(new DomainValue(OutputFormat.htmlDoc.toString(), "HTML Document"));
	}

	/**
	 * Renders all modules for the customer.
	 * <p>
	 * 
	 * @param config
	 *            - the documentation configuration
	 * @param customer
	 * @param out
	 *            - the print stream
	 * @throws Exception
	 */
	public static void renderCustomer(Customer customer, PrintStream out) throws Exception {

		// Create title page with index of modules

		// Copy resources to local location folder structure

		// For each module, create documentation
		for (Module module : customer.getModules()) {
			renderModule(customer, module, out);
		}

	}

	/**
	 * Render the module as documentation.
	 * 
	 * @param config
	 *            - the documentation configuration
	 * @param customer
	 * @param module
	 * @param out
	 *            - the print stream
	 * @throws Exception
	 */
	public static void renderModule(Customer customer, Module module, PrintStream out) throws Exception {

		// Create title page
		String localisedTitle = module.getLocalisedTitle();
		DocHeader header = new DocHeader(localisedTitle);
//		header.setRibbonBarLocation("ribbonBar.jpg"); // TODO
		out.println(header.toHTML());

		// Module description
		DocSection overview = new DocSection(createIndentifier(customer.getName(), module.getName(), "moduleOverview"));
		overview.setSectionTitle("Overview");
		overview.setSectionType(SectionType.SubChapter);
		overview.getHtmlContent().add(module.getDocumentation());
		out.println(overview.toHTML());

		// Index
		DocSection toc = new DocSection(createIndentifier(customer.getName(), module.getName(), "moduleTableOfContents"));
		toc.setSectionTitle("Outline");
		toc.getHtmlContent().add("This overview includes an index of:");
		out.println(toc.toHTML());
		DocList tocList = new DocList(false);
		if (!module.getDocumentRefs().keySet().isEmpty()) {
			tocList.getItems().add("Module Documents");
		}
		if (!module.getMetadataQueries().isEmpty()) {
			tocList.getItems().add("Module Queries ");
		}
		if (!module.getRoles().isEmpty()) {
			tocList.getItems().add("Module Roles");
		}
		if (!module.getJobs().isEmpty()) {
			tocList.getItems().add("Module Jobs");
		}
		out.println(tocList.toHTML());

		// index of documents
		if (!module.getDocumentRefs().keySet().isEmpty()) {
			DocTable table = new DocTable(createIndentifier(customer.getName(), module.getName(), "indexOfDocuments"));
			table.setTitle("Module Documents");
			table.getHtmlContent().add("The module " + localisedTitle + " has the following documents defined:");
			table.setHeaderValues("Document", "Description", "Documentation");

			for (String documentName : module.getDocumentRefs().keySet()) {
				Document document = module.getDocument(customer, documentName);
				// document.getName()
				table.setRowValues(document.getName(), document.getLocalisedDescription(), document.getDocumentation());
			}
			out.println(table.toHTML(true));
		}

		// Queries Index
		if (!module.getMetadataQueries().isEmpty()) {
			DocTable table = new DocTable(createIndentifier(customer.getName(), module.getName(), "indexOfQueries"));
			table.setTitle("Queries");
			table.getHtmlContent().add("The module " + localisedTitle + " has the following queries defined:");
			table.setHeaderValues("Query", "Driving Document", "Description", "Documentation");

			for (QueryDefinition q : module.getMetadataQueries()) {
				table.setRowValues(q.getName(),
									(q instanceof MetaDataQueryDefinition) ? ((MetaDataQueryDefinition) q).getDocumentName() : null, 
									q.getLocalisedDescription(),
									q.getDocumentation());
			}
			out.println(table.toHTML(true));
		}

		// Index of roles
		if (!module.getRoles().isEmpty()) {
			DocTable table = new DocTable(createIndentifier(customer.getName(), module.getName(), "indexOfRoles"));
			table.setTitle("Roles");
			table.getHtmlContent().add("The module " + localisedTitle + " has the following roles defined:");
			table.setHeaderValues("Field", "Description", "Doumentation");
			for (Role r : module.getRoles()) {
				table.setRowValues(r.getName(), r.getLocalisedDescription(), r.getDocumentation());
			}
			out.println(table.toHTML(true));
		}

		// Index of Jobs
		if (!module.getJobs().isEmpty()) {
			DocTable table = new DocTable(createIndentifier(customer.getName(), module.getName(), "indexOfJobs"));
			table.setTitle("Jobs");
			table.getHtmlContent().add("The module " + module.getName() + " has the following off-line jobs defined.");
			table.setHeaderValues("Name", "Job", "Class");
			for (JobMetaData j : module.getJobs()) {
				table.setRowValues(j.getName(), j.getLocalisedDisplayName(), j.getClassName());
			}
			out.println(table.toHTML(true));
		}

		// Document documentation
		DocSection docSection = new DocSection(createIndentifier(customer.getName(), module.getName(), "documents"));
		docSection.setSectionTitle("Module Documents");
		docSection.setSectionType(SectionType.Chapter);
		out.println(docSection.toHTML());

		if (!module.getDocumentRefs().keySet().isEmpty()) {
			// For each document, create documentation
			for (String documentName : module.getDocumentRefs().keySet()) {
				Document document = module.getDocument(customer, documentName);
				renderDocument(customer, module, document, out);
			}
		}

		// Query documentation
		DocSection qSection = new DocSection(createIndentifier(customer.getName(), module.getName(), "queries"));
		qSection.setSectionTitle("Module Queries");
		qSection.setSectionType(SectionType.Chapter);
		out.println(qSection.toHTML());

		if (!module.getMetadataQueries().isEmpty()) {
			// For each query, create documentation
			for (QueryDefinition q : module.getMetadataQueries()) {
				renderQuery(customer, module, q, out);
			}
		}

		// Role documentation
		DocSection rSection = new DocSection(createIndentifier(customer.getName(), module.getName(), "roles"));
		rSection.setSectionTitle("Roles");
		rSection.setSectionType(SectionType.Chapter);
		out.println(rSection.toHTML());

		if (!module.getRoles().isEmpty()) {
			// For each role, create documentation
			for (Role r : module.getRoles()) {
				renderRole(customer, module, r, out);
			}
		}
	}

	/**
	 * Render the document as documentation.
	 * 
	 * @param config
	 *            - the documentation configuration
	 * @param customer
	 * @param module
	 * @param document
	 * @param out
	 *            - the print stream
	 * @throws Exception
	 */
	public static void renderDocument(Customer customer, Module module, Document document, PrintStream out) throws Exception {

		DocSection title = new DocSection(createIndentifier(customer.getName(), module.getName(), document.getName(), "documentTitle"));
		title.setSectionType(SectionType.SubChapter);
		title.setSectionTitle(document.getLocalisedPluralAlias());
		title.getHtmlContent().add(document.getLocalisedDescription());
		title.getHtmlContent().add("<p/>");
		if (document.getParentDocumentName() != null) {
			title.getHtmlContent().add(document.getName() + " is a child of " + document.getParentDocumentName());
		}
		out.println(title.toHTML());
		
		// doc
		DocSection overview = new DocSection(createIndentifier(customer.getName(), module.getName(), document.getName(), "documentOverview"));
		overview.setSectionTitle("Overview");
		overview.getHtmlContent().add(document.getDocumentation());
		out.println(overview.toHTML());

		// List document attributes
		if (!document.getAttributes().isEmpty()) {
			DocTable table = new DocTable(createIndentifier(customer.getName(), module.getName(), document.getName(), "attributeList"));
			table.setTitle("Attributes");
			table.getHtmlContent().add(document.getPluralAlias() + " have the following attributes:");
			table.setHeaderValues("Attribute Name", "Display Name" , "Type", "Size", "Required", "Persistent", "Description", "Documentation", "Values", "Usage", "Deprecated");

			for (Attribute attribute : document.getAttributes()) {
				if (Bean.BIZ_KEY.equals(attribute.getName())) {
					// do nothing
				} else {
					// get constant domain values
					int fieldLen = 0;
					DocList valueList = new DocList(false);
					if(AttributeType.enumeration.equals(attribute.getAttributeType())
							|| DomainType.constant.equals(attribute.getDomainType())){
						try {
							for(DomainValue val: ((DocumentImpl) document).getDomainValues((CustomerImpl) customer, attribute.getDomainType(), attribute, null, false) ){
								StringBuilder sb = new StringBuilder();
								sb.append(val.getLocalisedDescription()).append(" (").append(val.getCode()).append(")");
								valueList.getItems().add(sb.toString());
								if(val.getCode().length()>fieldLen){
									fieldLen = val.getCode().length();
								}
							}
						}
						catch (@SuppressWarnings("unused") Exception e) {
							System.err.println(String.format("cannot get domain values for module %s, document %s & attribute %s. This can occur when a database is required to determine the values. ",
																module.getName(), document.getName(), attribute.getName()));
						}
					}
					
					if(attribute instanceof LengthField){
						LengthField lengthField = (LengthField) attribute;
						fieldLen = lengthField.getLength();
					}
					
					String size = (fieldLen==0?"":Integer.toString(fieldLen));
					UsageType usage = attribute.getUsage();
					table.setRowValues(attribute.getName(), 
										attribute.getLocalisedDisplayName(),
										attribute.getAttributeType().toString(),
										size,
										String.valueOf(attribute.isRequired()),
										String.valueOf(attribute.isPersistent()),
										attribute.getLocalisedDescription(),
										attribute.getDocumentation(),
										valueList.toHTML(),
										(usage == null) ? null : usage.toString(),
										String.valueOf(attribute.isDeprecated()));
				}
			}
			out.println(table.toHTML(true));
		}

		// List References
		if (!document.getReferenceNames().isEmpty()) {
			DocTable table = new DocTable(createIndentifier(customer.getName(), module.getName(), document.getName(), "referenceList"));
			table.setTitle("References");
			table.getHtmlContent().add(document.getPluralAlias() + " have the following references to other documents:");
			table.setHeaderValues("Reference", "Type", "Document", "Required", "Description", "Documentation", "Usage", "Deprecated");
			for (String referenceName : document.getReferenceNames()) {
				Reference reference = document.getReferenceByName(referenceName);
				UsageType usage = reference.getUsage();
				table.setRowValues(reference.getLocalisedDisplayName(), 
									reference.getAttributeType().toString(), 
									reference.getDocumentName(), 
									String.valueOf(reference.isRequired()), 
									reference.getLocalisedDescription(), 
									reference.getDocumentation(), 
									(usage == null) ? null : usage.toString(),
									String.valueOf(reference.isDeprecated()));
			}
			out.println(table.toHTML(true));
		}

		// List Conditions
		if (!document.getConditionNames().isEmpty()) {
			DocTable table = new DocTable(createIndentifier(customer.getName(), module.getName(), document.getName(), "conditionList"));
			table.setTitle("Conditions");
			table.getHtmlContent().add(document.getPluralAlias() + " have the following conditions specified:");
			table.setHeaderValues("Name", "Description", "Documentation", "Usage");
			for (String conditionName : document.getConditionNames()) {
				Condition condition = document.getCondition(conditionName);
				UsageType usage = condition.getUsage();
				table.setRowValues(conditionName, 
									condition.getDescription(), 
									condition.getDocumentation(), 
									(usage == null) ? null : usage.toString());
			}
			out.println(table.toHTML(true));
		}

		// List Constraints
		if (!document.getUniqueConstraints().isEmpty()) {
			DocTable table = new DocTable(createIndentifier(customer.getName(), module.getName(), document.getName(), "uniqueConstraintList"));
			table.setTitle("UniqueConstraints");
			table.getHtmlContent().add(document.getPluralAlias() + " have the following uniqueness constraints specified:");
			table.setHeaderValues("Constraint", "Description", "Scope", "Uniqueness");
			for (UniqueConstraint u : document.getUniqueConstraints()) {
				StringBuilder s = new StringBuilder();
				for (String r : u.getFieldNames()) {
					if (s.toString().length() > 0) {
						s.append(", ");
					}
					Attribute a = getAttributeFromFieldName(document, r);
					if (a != null) {
						s.append(a.getLocalisedDisplayName());
					} else {
						s.append(r);
					}
				}
				table.setRowValues(u.getName(), 
									u.getLocalisedDescription(),
									u.getScope().toString(),
									s.toString());
			}
			out.println(table.toHTML(true));
		}

		// List Actions
		if (!document.getDefinedActionNames().isEmpty()) {
			DocSection section = new DocSection(createIndentifier(customer.getName(), module.getName(), document.getName(), "actionList"));
			section.setSectionTitle("Actions");
			out.println(section.toHTML());

			DocList actionList = new DocList(false);
			for (String actionName : document.getDefinedActionNames()) {
				actionList.getItems().add(actionName);
			}
			out.println(actionList.toHTML());
		}

		// List Reports

		// List Classes

	}

	/**
	 * Renders the query as documentation.
	 * 
	 * @param config
	 * @param customer
	 * @param module
	 * @param q
	 * @param out
	 * @throws Exception
	 */
	public static void renderQuery(Customer customer, Module module, QueryDefinition q, PrintStream out) throws Exception {
		// Documentation for Query
		DocSection section = new DocSection(createIndentifier(customer.getName(), module.getName(), q.getName() + "Overview"));
		section.setSectionType(SectionType.SubChapter);
		if (q instanceof MetaDataQueryDefinition) {
			section.setSectionTitle("Document Query " + q.getName());
		}
		else if (q instanceof SQLDefinition) {
			section.setSectionTitle("SQL Query " + q.getName());
		}
		else if (q instanceof BizQLDefinition) {
			section.setSectionTitle("BizQL Query " + q.getName());
		}
		section.getHtmlContent().add(q.getLocalisedDescription());
		section.getHtmlContent().add(q.getDocumentation());
		out.println(section.toHTML());

		// Field List
		if (q instanceof MetaDataQueryDefinition) {
			MetaDataQueryDefinition dq = (MetaDataQueryDefinition) q;
			DocTable table = new DocTable(createIndentifier(customer.getName(), module.getName(), q.getName() + "queryFieldList"));
			table.setTitle("Columns");
			table.setHeaderValues("Field", "Description", "Expression", "Filter", "Order");
			for (MetaDataQueryColumn c : dq.getColumns()) {
	
				// See if we can find the document binding
				String binding = null;
				String expression = null;
				Document d = module.getDocument(customer, dq.getDocumentName());
				String name = c.getBinding();
				if (name == null) {
					name = c.getName();
				}
				Attribute a = getAttributeFromFieldName(d, name);
				if (a != null) {
					binding = a.getLocalisedDisplayName();
				} else {
					binding = name;
				}
				if (c instanceof MetaDataQueryProjectedColumn) {
					expression = ((MetaDataQueryProjectedColumn) c).getExpression();
				}
				
				table.setRowValues(binding, c.getLocalisedDisplayName(), expression, c.getFilterExpression(), titleCase(c.getSortOrder() == null ? "" : c.getSortOrder().toString()));
			}
			out.println(table.toHTML(true));
		}
	}

	/**
	 * Render the role as documentation.
	 * 
	 * @param config
	 * @param customer
	 * @param module
	 * @param r
	 * @param out
	 * @throws Exception
	 */
	public static void renderRole(Customer customer, Module module, Role r, PrintStream out) throws Exception {
		// Documentation for Role
		DocSection section = new DocSection(createIndentifier(customer.getName(), module.getName(), r.getName() + "Overview"));
		section.setSectionTitle(r.getName());
		section.setSectionType(SectionType.SubChapter);
		section.getHtmlContent().add(r.getDocumentation());
		out.println(section.toHTML());

		// Priveleges List
		DocTable table = new DocTable(createIndentifier(customer.getName(), module.getName(), r.getName() + "roleDocumentList"));
		table.setTitle("Document Priveleges");
		table.getHtmlContent().add(r.getLocalisedDescription());
		table.getHtmlContent().add(r.getDocumentation());
		table.getHtmlContent().add("The role " + r.getName() + " has the following privileges:");

		table.setHeaderValues("Document", "Read", "Create", "Update", "Delete", "Actions");
		for (Privilege p : ((RoleImpl) r).getPrivileges()) {
			if (p instanceof DocumentPrivilege) {
				DocumentPermission permission = ((DocumentPrivilege) p).getPermission();

				// generate Action permissions for this document
				DocList actionList = new DocList(false);
				for (Privilege ap : ((RoleImpl) r).getPrivileges()) {
					if (ap instanceof ActionPrivilege) {
						if (p.getName().equals(((ActionPrivilege) ap).getDocumentName())) {
							actionList.getItems().add(ap.getName());
						}
					}
				}

				table.setRowValues(p.getName(), getCheckHTML(permission.canRead()), getCheckHTML(permission.canCreate()), getCheckHTML(permission.canUpdate()), getCheckHTML(permission.canDelete()), actionList.toHTML());
			}
		}
		out.println(table.toHTML(true));

		// DocTable tableCP = new DocTable(createIndentifier(customer.getName(),
		// module.getName(), r.getName() + "roleContentPermissions"));
		// tableCP.setTitle("Content Permissions");
		// tableCP.getHtmlContent().add("The role " + r.getName() +
		// " has the following content permissions.");
		// tableCP.setHeaderValues("Document", "Attribute");
		// for (ContentPermission cp : r.getContentPermissions()) {
		// tableCP.setRowValues(cp.getDocumentName(), cp.getAttributeName());
		// }
		// out.println(tableCP.toHTML(true));
		//
		// DocTable tableCR = new DocTable(createIndentifier(customer.getName(),
		// module.getName(), r.getName() + "roleContentRestricutions"));
		// tableCR.setTitle("Content Restrictions");
		// tableCP.getHtmlContent().add("The role " + r.getName() +
		// " has the following content restrictions.");
		// tableCR.setHeaderValues("Document", "Attribute");
		// for (ContentRestriction cr : r.getContentRestrictions()) {
		// table.setRowValues(cr.getDocumentName(), cr.getAttributeName());
		// }
		// out.println(tableCR.toHTML(true));
	}

	/**
	 * Concatenates an array of Strings to a single non-spaced string useful as
	 * an identifier.
	 * 
	 * @param s
	 *            - the array of Strings
	 * @return - the identifier
	 */
	public static String createIndentifier(String... s) {

		StringBuilder id = new StringBuilder();

		for (int i = 0; i < s.length; i++) {
			String x = s[i].replaceAll(" ", "").toLowerCase();
			if (x.length() > 0) {
				if (id.length() > 0) {
					id.append("_");
				}
				id.append(x);
			}
		}

		return id.toString();
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

	/**
	 * getCheckHTML returns an HTML image tag representing the boolean state of
	 * true, represented by the Check icon reference specified in the
	 * configuration document.
	 * 
	 * @param c
	 * @param b
	 * @return
	 */
	private static String getCheckHTML(boolean b) {
		String result = null;
		if (b) {
			result = "<div style=\"text-align:center;\">X</div>"; // TODO
		}

		return result;
	}

	/**
	 * Returns a title case version of the String.
	 * 
	 * @param s
	 * @return
	 */
	private static String titleCase(String s) {
		if (s.length() > 1) {
			return s.substring(0, 1).toUpperCase() + s.substring(1);
		}
		return s.toUpperCase();
	}
	
	public static void main(String[] args)
	throws Exception {
		ProvidedRepositoryFactory.set(new LocalDesignRepository());
		ProvidedRepository repository = ProvidedRepositoryFactory.get();
		
		Customer customer = repository.getCustomer(args[1]);
		try (PrintStream overview = new PrintStream(new FileOutputStream(String.format("%s%soverview.html", args[0], ProvidedRepository.MODULES_NAMESPACE)))) {
			overview.println("<!DOCTYPE html>");
			overview.println("<html>");
			overview.println("<head>");
			overview.println("<title>Skyve Javadoc</title>");
			overview.println("</head>");
			overview.println("<body>");
			overview.println("<p>");
			overview.println("Please click on the links below to each Skyve module or browse the javadoc package links underneath...<br/><br/>");

			for (int i = 2, l = args.length; i < l; i++) {
				Module module = customer.getModule(args[i]);

				overview.print("&nbsp;<a href=\"modules/");
				overview.print(module.getName());
				overview.print("/domain/package-summary.html\">");
				overview.print(module.getLocalisedTitle());
				overview.println("</a><br/>");

				try (PrintStream ps = new PrintStream(new FileOutputStream(args[0] + 
																			ProvidedRepository.MODULES_NAMESPACE + 
																			module.getName() + '/' + 
																			ProvidedRepository.DOMAIN_NAME + 
																			"/package.html"))) {
					ps.println("<html><head/><body>");
					ps.append("<p>").append((module.getDocumentation() != null) ? module.getDocumentation() : module.getLocalisedTitle()).append("</p><style>");
					ps.println("body { font-family : Verdana, Arial; font-size:85%; }");
					ps.println("/* Page title */");
					ps.println(".pageHeader { border : 1px solid; border-color:#DDDDDD; font-size: 140%; font-weight: bold; color: #000000;	margin-bottom: 20px; text-wrap: normal; word-wrap: break-word; }");
					ps.println("/* Tables */");
					ps.println(".tableTitle { font-size: 100%; font-weight: bold; margin-bottom: 0.5em; margin-top: 0.5em;	padding-left:0px; padding-top:10px; padding-bottom:0px; vertical-align: middle; }");
					ps.println("table { border : 1px solid;	border-color:#DDDDDD; }");
					ps.println("table.dataTable { font-size: 90%; }");
					ps.println(".tableroweven { background-color:  #FFFFFF; }");
					ps.println(".tablerowodd { background-color:  #DDDDDD; }");
					ps.println("thead { font-weight: bold; vertical-align: top; }");
					ps.println("/* Lists */");
					ps.println("li { font-size: 100%; }");
					ps.println("/* Sections */");
					ps.println(".chapterTitle, h1 { font-size: 130%; font-weight: bold; margin-bottom: 0.5em; margin-top: 0.5em; padding-left:0px; padding-top:30px; padding-bottom:0px; vertical-align: middle; }"); 
					ps.println(".subChapterTitle, h2 { font-size: 120%; font-weight: bold; margin-bottom: 0.5em; margin-top: 0.5em;	padding-left:0px; padding-top:30px; padding-bottom:0px; vertical-align: middle; }");
					ps.println(".sectionTitle , h3 { font-size: 110%; font-weight: bold; margin-bottom: 0.5em; margin-top: 0.5em; padding-left:0px; padding-top:20px; padding-bottom:0px; vertical-align: middle; }"); 
					ps.println(".subTitle , h4 { font-size: 100%; font-weight: bold; margin-bottom: 0.5em; margin-top: 0.5em; padding-left:0px; padding-top:10px; padding-bottom:0px; vertical-align: middle; }");
					ps.println("/* Header and Footer */");
					ps.println(".footer { text-align: center; font-size: smaller; margin-top: 1em; border-top: 1px solid; border-color:#DDDDDD; }");
					ps.println("</style>");
					renderModule(customer, module, ps);
					ps.println("</body></html>");
				}
			}
			
			overview.println("</p>");
			overview.println("</body>");
			overview.println("</html>");
		}
	}
}
