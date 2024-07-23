package modules.admin.ReportTemplate.actions;

import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;

import modules.admin.ReportTemplate.ReportTemplateExtension;
import modules.admin.domain.ReportTemplate.GenerateExisting;
import modules.admin.domain.ReportTemplate.ReportType;
import modules.admin.domain.ReportTemplate.WizardState;

public class Next implements ServerSideAction<ReportTemplateExtension> {

	@Override
	public ServerSideActionResult<ReportTemplateExtension> execute(ReportTemplateExtension bean, WebContext webContext)
			throws Exception {

		if (WizardState.enterDetails == bean.getWizardState()) {
			bean.setWizardState(WizardState.enterMarkup);

			// reset the state in case the user has clicked Next and Back
			resetState(bean);

			// if we are generating a freemarker template, generate it now
			if (bean.getReportType() == ReportType.freemarker && bean.getGenerateExisting() == GenerateExisting.generate) {
				bean.generateInitialFreemarkerTemplate();
				bean.generateInitialDataset();
			}
		}

		return new ServerSideActionResult<>(bean);
	}

	/**
	 * Reset the markup field state to the initial state.
	 */
	private static void resetState(ReportTemplateExtension bean) {
		bean.getParameters().clear();
		bean.getDatasets().clear();
		bean.setTemplate(null);

		// clear any set jasper fields
		bean.setModuleName(null);
		bean.setDocumentName(null);
		bean.setReportName(null);
		bean.setMode(null);
	}
}
