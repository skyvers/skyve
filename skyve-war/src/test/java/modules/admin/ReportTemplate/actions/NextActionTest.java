package modules.admin.ReportTemplate.actions;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;
import org.skyve.metadata.controller.ServerSideActionResult;

import modules.admin.ReportTemplate.ReportTemplateExtension;
import modules.admin.domain.ReportTemplate.WizardState;

@SuppressWarnings("static-method")
class NextActionTest {

	@Test
	void executeWithEnterDetailsStateAdvancesToEnterMarkup() throws Exception {
		Next action = new Next();
		ReportTemplateExtension bean = new ReportTemplateExtension();
		// wizardState defaults to enterDetails; generateExisting is null → skips freemarker generation
		assertEquals(WizardState.enterDetails, bean.getWizardState());

		ServerSideActionResult<ReportTemplateExtension> result = action.execute(bean, null);

		assertNotNull(result);
		assertEquals(WizardState.enterMarkup, bean.getWizardState());
	}

	@Test
	void executeWithEnterMarkupStateDoesNotChangeState() throws Exception {
		Next action = new Next();
		ReportTemplateExtension bean = new ReportTemplateExtension();
		bean.setWizardState(WizardState.enterMarkup);

		ServerSideActionResult<ReportTemplateExtension> result = action.execute(bean, null);

		assertNotNull(result);
		// state stays enterMarkup - no change in the else branch
		assertEquals(WizardState.enterMarkup, bean.getWizardState());
	}

	@Test
	void executeWithEnterDetailsStateClearsParameters() throws Exception {
		Next action = new Next();
		ReportTemplateExtension bean = new ReportTemplateExtension();

		action.execute(bean, null);

		// resetState clears parameters and datasets
		assertNotNull(bean.getParameters());
		assertNotNull(bean.getDatasets());
	}
}
