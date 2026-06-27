package modules.admin.ReportTemplate.actions;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.Test;
import org.skyve.domain.app.admin.ReportParameter.Type;
import org.skyve.domain.messages.ValidationException;

import modules.admin.ReportParameter.ReportParameterExtension;
import modules.admin.ReportTemplate.ReportTemplateExtension;

@SuppressWarnings("static-method")
class DownloadJasperReportTest {
	@Test
	void prepareWithNoParametersDoesNotThrow() {
		ReportTemplateExtension template = new ReportTemplateExtension();

		assertDoesNotThrow(() -> new DownloadJasperReport().prepare(template, null));
	}

	@Test
	void prepareThrowsWhenRequiredParameterHasNoInputOrDefault() {
		DownloadJasperReport action = new DownloadJasperReport();
		ReportTemplateExtension template = new ReportTemplateExtension();
		ReportParameterExtension parameter = new ReportParameterExtension();
		parameter.setName("asAtDate");
		parameter.setType(Type.date);
		parameter.setRequired(Boolean.TRUE);
		template.getParameters().add(parameter);

		ValidationException exception = assertThrows(ValidationException.class,
				() -> action.prepare(template, null));

		assertThat(exception.getMessages().get(0).getText(), containsString("Value for parameter 'asAtDate' is required"));
	}

	@Test
	void prepareThrowsWhenIntegerParameterInputIsInvalid() {
		DownloadJasperReport action = new DownloadJasperReport();
		ReportTemplateExtension template = new ReportTemplateExtension();
		ReportParameterExtension parameter = new ReportParameterExtension();
		parameter.setName("count");
		parameter.setType(Type.integer);
		parameter.setReportInputValue("not-a-number");
		template.getParameters().add(parameter);

		ValidationException exception = assertThrows(ValidationException.class,
				() -> action.prepare(template, null));

		assertThat(exception.getMessages().get(0).getText(), containsString("Expected a whole number"));
	}
}
