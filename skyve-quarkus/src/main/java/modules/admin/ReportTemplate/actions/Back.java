package modules.admin.ReportTemplate.actions;

import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;

import modules.admin.ReportTemplate.ReportTemplateExtension;
import modules.admin.domain.ReportTemplate.WizardState;

public class Back implements ServerSideAction<ReportTemplateExtension> {

	@Override
	public ServerSideActionResult<ReportTemplateExtension> execute(ReportTemplateExtension bean, WebContext webContext)
			throws Exception {

		if (WizardState.enterMarkup == bean.getWizardState()) {
			bean.setWizardState(WizardState.enterDetails);
		}

		return new ServerSideActionResult<>(bean);
	}

}
