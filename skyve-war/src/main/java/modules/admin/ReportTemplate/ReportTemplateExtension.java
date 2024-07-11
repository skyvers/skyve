package modules.admin.ReportTemplate;

import java.util.List;

import org.skyve.CORE;
import org.skyve.domain.app.admin.ReportDataset.DatasetType;
import org.skyve.domain.messages.ValidationException;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;

import com.cronutils.descriptor.CronDescriptor;
import com.cronutils.model.CronType;
import com.cronutils.model.definition.CronDefinition;
import com.cronutils.model.definition.CronDefinitionBuilder;
import com.cronutils.parser.CronParser;

import modules.admin.ReportDataset.ReportDatasetExtension;
import modules.admin.ReportParameter.ReportParameterExtension;
import modules.admin.UserProxy.UserProxyExtension;
import modules.admin.domain.Generic;
import modules.admin.domain.ReportDataset;
import modules.admin.domain.ReportTemplate;
import modules.admin.domain.UserProxy;

public class ReportTemplateExtension extends ReportTemplate {
	private static final long serialVersionUID = -7147172221052954971L;

	private static final CronDefinition CRON_DEFINITION = CronDefinitionBuilder.instanceDefinitionFor(CronType.QUARTZ);

	private static final String DEFAULT_RECORD_DATASET_NAME = "records";
	private static final String DEFAULT_TITLE_DATASET_NAME = "title";
	private static final String DEFAULT_TEMPLATE_HEADER_NAME = "headerPortrait.ftlh";

	/**
	 * Removes any assigned schedules from this report template.
	 */
	public void clearSchedules() {
		setScheduled(Boolean.FALSE);
		setCronExpression(null);
		setStartTime(null);
		setEndTime(null);
		setRunAs(null);
		getUsersToEmail().clear();
	}

	/**
	 * Returns a text description of the cron schedule for this report.
	 */
	@Override
	public String getScheduleDescription() {
		try {
			return CronDescriptor.instance().describe(new CronParser(CRON_DEFINITION).parse(getCronExpression()));
		} catch (@SuppressWarnings("unused") Exception e) {
			return null;
		}
	}

	/**
	 * Generate markup for a new Freemarker report when a new report is being created.
	 */
	public void generateInitialFreemarkerTemplate() {
		// make sure we have a document
		if (getGenerateDocumentName() == null) {
			throw new ValidationException(ReportTemplate.generateDocumentNamePropertyName, "Document name is required");
		}

		// get the attributes for the document
		Module module = CORE.getCustomer().getModule(getGenerateModuleName());
		Document document = module.getDocument(CORE.getCustomer(), getGenerateDocumentName());
		List<? extends Attribute> attributes = document.getAttributes();
		
		StringBuilder tableHeaderRows = new StringBuilder();
		StringBuilder tableDetailRows = new StringBuilder();

		for (Attribute a : attributes) {
			// make sure the attribute is not transient, a collection or content
			if (a.getAttributeType() == AttributeType.collection
					|| a.getAttributeType() == AttributeType.content
					|| a.getAttributeType() == AttributeType.image
					|| !a.isPersistent()) {
				continue;
			}

			tableHeaderRows.append(generateTableHeaderRow(a.getLocalisedDisplayName()));
			tableDetailRows.append(generateTableDetailRow(a.getName()));
		}

		// build the template
		String template = String.format("<!-- report header and styles -->\n" +
				"<#include \"%s\">\n\n" +
				"<!-- report body -->\n" +
				"	<#list %s>\n" +
				"		<table class=\"table\">\n" +
				"			<thead>\n" +
				"				<!-- column titles -->\n" +
				"				<tr>\n" +
				"%s" +
				"				</tr>\n" +
				"			</thead>\n" +
				"			<tbody>\n" +
				"			<#items as bean>\n" +
				"				<!--  detail -->\n" +
				"				<tr>\n" +
				"%s" +
				"				</tr>\n" +
				"			</#items>\n" +
				"			</tbody>\n" +
				"		</table>\n" +
				"	<#else>\n" +
				"		No records.\n" +
				"	</#list>\n" +
				"</body>\n" +
				"</html>",
				DEFAULT_TEMPLATE_HEADER_NAME,
				DEFAULT_RECORD_DATASET_NAME,
				tableHeaderRows.toString(),
				tableDetailRows.toString());

		setTemplate(template);
	}

	public void generateInitialDataset() {
		// make sure we have a document
		if (getGenerateDocumentName() == null) {
			throw new ValidationException(ReportTemplate.generateDocumentNamePropertyName, "Document name is required");
		}

		// create the initial datasets
		ReportDatasetExtension recordDataset = ReportDataset.newInstance();
		recordDataset.setDatasetName(DEFAULT_RECORD_DATASET_NAME);
		recordDataset.setDatasetType(DatasetType.bizQL);
		recordDataset.setQuery(String.format("select a\nfrom {%s.%s} a", getGenerateModuleName(), getGenerateDocumentName()));

		ReportDatasetExtension titleDataset = ReportDataset.newInstance();
		titleDataset.setDatasetName(DEFAULT_TITLE_DATASET_NAME);
		titleDataset.setDatasetType(DatasetType.constant);
		titleDataset.setQuery(getName());

		addDatasetsElement(recordDataset);
		addDatasetsElement(titleDataset);
	}

	private static String generateTableDetailRow(final String attributeBinding) {
		return String.format("\t\t\t\t\t<td>\n"
				+ "\t\t\t\t\t\t<@format bean=bean binding=\"%s\" />\n"
				+ "\t\t\t\t\t</td>\n", attributeBinding);
	}

	private static String generateTableHeaderRow(String displayName) {
		return String.format("\t\t\t\t\t<th>%s</th>\n", displayName);
	}

	/**
	 * Does this report template contain any required paramters? This is used to determine
	 * if the report can be scheduled or not.
	 * 
	 * @return True if this report template has required parameters
	 */
	public boolean hasRequiredParameters() {
		if (getParameters().size() > 0) {
			for (ReportParameterExtension param : getParameters()) {
				if (Boolean.TRUE.equals(param.getRequired())) {
					return true;
				}
			}
		}

		return false;
	}

	/**
	 * Creates a {@link Generic} for each persistent user in the <code>usersToEmail</code>
	 * collection so that it can be manipulated in the view.
	 */
	public void restoreScheduledUsers() {
		for (UserProxy u : getUsersToEmail()) {
			Generic g = Generic.newInstance();
			g.setId1(u.getBizId());
			g.setText5001(u.getContact().getEmail1());
			getEditUsersToEmail().add(g);
		}
	}

	/**
	 * Stores a persistent {@link UserProxy} association for each Generic in the
	 * <code>editUsersToEmail</code> collection.
	 */
	public void updateScheduledUsers() {
		// clear the existing persistent collection
		getUsersToEmail().clear();

		for (Generic g : getEditUsersToEmail()) {
			UserProxyExtension u = CORE.getPersistence().retrieve(UserProxy.MODULE_NAME, UserProxy.DOCUMENT_NAME, g.getId1());
			getUsersToEmail().add(u);
		}
	}
}
