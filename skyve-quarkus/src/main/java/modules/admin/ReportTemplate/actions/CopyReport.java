package modules.admin.ReportTemplate.actions;

import org.skyve.CORE;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.util.Util;
import org.skyve.web.WebContext;

import modules.admin.ReportDataset.ReportDatasetExtension;
import modules.admin.ReportParameter.ReportParameterExtension;
import modules.admin.ReportTemplate.ReportTemplateExtension;
import modules.admin.domain.ReportTemplate;

public class CopyReport implements ServerSideAction<ReportTemplate> {

	private static final String COPY_PREFIX = "Copy of ";
	@Override
	public ServerSideActionResult<ReportTemplate> execute(ReportTemplate bean, WebContext webContext)
			throws Exception {

		// allow copying the report, even if there are changes, 
		// as the creator may have been making changes, then decided to create a copy instead
		
		ReportTemplateExtension newReport = ReportTemplate.newInstance();

		newReport.setTemplateName(COPY_PREFIX + bean.getTemplateName());
		newReport.setTemplate(bean.getTemplate());
		newReport.setModuleName(bean.getModuleName());
		newReport.setDocumentName(bean.getDocumentName());
		newReport.setReportName(bean.getReportName());

		newReport.setName(COPY_PREFIX + bean.getName());
		if (bean.getDescription() != null) {
			newReport.setDescription(COPY_PREFIX + bean.getDescription());
		}
		newReport.setEnabled(bean.getEnabled());
		newReport.setIncludeFragment(bean.getIncludeFragment());
		newReport.setReportType(bean.getReportType());
		newReport.setOutputFormat(bean.getOutputFormat());
		
		// duplicate data sets
		for(ReportDatasetExtension d: bean.getDatasets()) {
			ReportDatasetExtension newDataset = Util.cloneToTransientBySerialization(d);
			newDataset.setParent(newReport);
			newReport.getDatasets().add(newDataset);
		}
		
		// duplicate parameters
		for(ReportParameterExtension p: bean.getParameters()) {
			ReportParameterExtension newParameter = Util.cloneToTransientBySerialization(p);
			newParameter.setParent(newReport);
			newReport.getParameters().add(newParameter);
		}
		
		// return the new report
		newReport = CORE.getPersistence().save(newReport);
		return new ServerSideActionResult<>(newReport);
	}

}
