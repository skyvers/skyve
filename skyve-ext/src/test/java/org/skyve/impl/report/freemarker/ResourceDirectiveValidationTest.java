package org.skyve.impl.report.freemarker;

import static org.junit.jupiter.api.Assertions.assertThrows;

import java.util.Map;

import org.junit.jupiter.api.Test;

import freemarker.template.TemplateDirectiveBody;
import freemarker.template.TemplateModel;
import freemarker.template.TemplateModelException;
import freemarker.template.TemplateScalarModel;

@SuppressWarnings("static-method")
class ResourceDirectiveValidationTest {
	@Test
	void executeRejectsMissingParametersLoopVariablesAndNestedBody() {
		ResourceDirective directive = new ResourceDirective();
		Map<String, TemplateModel> params = Map.of("filename", scalar("logo.png"));
		TemplateDirectiveBody body = env -> {
			// no-op
		};

		assertThrows(TemplateModelException.class, () -> directive.execute(null, Map.of(), new TemplateModel[0], null));
		assertThrows(TemplateModelException.class, () -> directive.execute(null, params, new TemplateModel[] { scalar("loop") }, null));
		assertThrows(TemplateModelException.class, () -> directive.execute(null, params, new TemplateModel[0], body));
	}

	@Test
	void executeRejectsInvalidParameterTypesAndUnsupportedParameters() {
		ResourceDirective directive = new ResourceDirective();

		assertThrows(TemplateModelException.class, () -> directive.execute(null, Map.of("filename", marker()), new TemplateModel[0], null));
		assertThrows(TemplateModelException.class, () -> directive.execute(null, Map.of("module", marker()), new TemplateModel[0], null));
		assertThrows(TemplateModelException.class, () -> directive.execute(null, Map.of("other", scalar("value")), new TemplateModel[0], null));
	}

	@Test
	void executeParsesScalarParametersBeforeNeedingEnvironment() {
		ResourceDirective directive = new ResourceDirective();
		Map<String, TemplateModel> params = Map.of(
				"filename", scalar("logo.png"),
				"module", scalar("admin"));

		assertThrows(NullPointerException.class, () -> directive.execute(null, params, new TemplateModel[0], null));
	}

	private static TemplateScalarModel scalar(String value) {
		return () -> value;
	}

	private static TemplateModel marker() {
		return new TemplateModel() {
			// marker model only
		};
	}
}
