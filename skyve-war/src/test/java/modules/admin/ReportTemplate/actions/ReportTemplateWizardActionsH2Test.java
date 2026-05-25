package modules.admin.ReportTemplate.actions;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.sail.mock.MockWebContext;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import modules.admin.ReportTemplate.ReportTemplateExtension;
import modules.admin.domain.ReportTemplate;
import modules.admin.domain.ReportTemplate.WizardState;
import util.AbstractH2Test;

/**
 * Tests for simple ReportTemplate wizard actions: Back, Next, AddUserToEmail, CopyReport.
 */
public class ReportTemplateWizardActionsH2Test extends AbstractH2Test {

	private DataBuilder db;
	private MockWebContext webContext;

	@BeforeEach
	void setup() {
		db = new DataBuilder().fixture(FixtureType.crud);
		webContext = new MockWebContext();
	}

	// ---- Back action ----

	@Test
	void backFromEnterMarkupStateGoesToEnterDetails() throws Exception {
		ReportTemplateExtension bean = db.build(ReportTemplate.MODULE_NAME, ReportTemplate.DOCUMENT_NAME);
		bean.setWizardState(WizardState.enterMarkup);

		Back action = new Back();
		ServerSideActionResult<ReportTemplateExtension> result = action.execute(bean, webContext);

		assertThat(result, is(notNullValue()));
		assertEquals(WizardState.enterDetails, result.getBean().getWizardState());
	}

	@Test
	void backFromEnterDetailsStateKeepsState() throws Exception {
		ReportTemplateExtension bean = db.build(ReportTemplate.MODULE_NAME, ReportTemplate.DOCUMENT_NAME);
		bean.setWizardState(WizardState.enterDetails);

		Back action = new Back();
		ServerSideActionResult<ReportTemplateExtension> result = action.execute(bean, webContext);

		assertThat(result, is(notNullValue()));
		// Should remain in enterDetails as Back only transitions from enterMarkup
		assertEquals(WizardState.enterDetails, result.getBean().getWizardState());
	}

	// ---- Next action ----

	@Test
	void nextFromEnterDetailsStateGoesToEnterMarkup() throws Exception {
		ReportTemplateExtension bean = db.build(ReportTemplate.MODULE_NAME, ReportTemplate.DOCUMENT_NAME);
		bean.setWizardState(WizardState.enterDetails);

		Next action = new Next();
		ServerSideActionResult<ReportTemplateExtension> result = action.execute(bean, webContext);

		assertThat(result, is(notNullValue()));
		assertEquals(WizardState.enterMarkup, result.getBean().getWizardState());
	}

	@Test
	void nextFromEnterDetailsResetsState() throws Exception {
		ReportTemplateExtension bean = db.build(ReportTemplate.MODULE_NAME, ReportTemplate.DOCUMENT_NAME);
		bean.setWizardState(WizardState.enterDetails);
		bean.setTemplate("some markup");

		Next action = new Next();
		ServerSideActionResult<ReportTemplateExtension> result = action.execute(bean, webContext);

		assertThat(result, is(notNullValue()));
		// State should be reset: template cleared, collections cleared
		assertThat(result.getBean().getTemplate(), is(nullValue()));
		assertThat(result.getBean().getParameters().isEmpty(), is(true));
		assertThat(result.getBean().getDatasets().isEmpty(), is(true));
	}

	@Test
	void nextFromEnterMarkupStateKeepsMarkupState() throws Exception {
		ReportTemplateExtension bean = db.build(ReportTemplate.MODULE_NAME, ReportTemplate.DOCUMENT_NAME);
		bean.setWizardState(WizardState.enterMarkup);

		Next action = new Next();
		ServerSideActionResult<ReportTemplateExtension> result = action.execute(bean, webContext);

		assertThat(result, is(notNullValue()));
		// Only transitions from enterDetails
		assertEquals(WizardState.enterMarkup, result.getBean().getWizardState());
	}

	// ---- AddUserToEmail action ----

	@Test
	void addUserToEmailWithNullUserThrowsValidationException() throws Exception {
		ReportTemplate bean = db.build(ReportTemplate.MODULE_NAME, ReportTemplate.DOCUMENT_NAME);
		bean.setNewUserToEmail(null);

		AddUserToEmail action = new AddUserToEmail();
		org.junit.jupiter.api.Assertions.assertThrows(
				org.skyve.domain.messages.ValidationException.class,
				() -> action.execute(bean, webContext));
	}

	// ---- CopyReport action ----

	@Test
	void copyReportCreatesNewReportWithCopyPrefix() throws Exception {
		ReportTemplateExtension bean = db.build(ReportTemplate.MODULE_NAME, ReportTemplate.DOCUMENT_NAME);
		bean.setName("Original Report");
		bean.setTemplateName("OriginalTemplate");
		bean.setDescription("Original description");

		CopyReport action = new CopyReport();
		ServerSideActionResult<ReportTemplate> result = action.execute(bean, webContext);

		assertThat(result, is(notNullValue()));
		assertThat(result.getBean(), is(notNullValue()));
		assertThat(result.getBean().getName().startsWith("Copy of"), is(true));
		assertThat(result.getBean().getTemplateName().startsWith("Copy of"), is(true));
	}

	@Test
	void copyReportWithNullDescriptionDoesNotCopyDescription() throws Exception {
		ReportTemplateExtension bean = db.build(ReportTemplate.MODULE_NAME, ReportTemplate.DOCUMENT_NAME);
		bean.setName("Report No Description");
		bean.setTemplateName("Template");
		bean.setDescription(null);

		CopyReport action = new CopyReport();
		ServerSideActionResult<ReportTemplate> result = action.execute(bean, webContext);

		assertThat(result, is(notNullValue()));
		assertThat(result.getBean().getDescription(), is(nullValue()));
	}
}
