package org.skyve.impl.report.freemarker;

import static org.junit.jupiter.api.Assertions.assertThrows;

import java.util.Map;

import org.junit.jupiter.api.Test;

import freemarker.template.TemplateBooleanModel;
import freemarker.template.TemplateDirectiveBody;
import freemarker.template.TemplateModel;
import freemarker.template.TemplateModelException;
import freemarker.template.TemplateScalarModel;

@SuppressWarnings("static-method")
class DescriptionDirectiveTest {
	@Test
	void executeRejectsMissingParametersLoopVariablesAndNestedBody() {
		DescriptionDirective directive = new DescriptionDirective();
		Map<String, TemplateModel> params = Map.of("binding", scalar("name"));
		TemplateDirectiveBody body = env -> {
			// no-op
		};

		assertThrows(TemplateModelException.class, () -> directive.execute(null, Map.of(), new TemplateModel[0], null));
		assertThrows(TemplateModelException.class, () -> directive.execute(null, params, new TemplateModel[] { scalar("loop") }, null));
		assertThrows(TemplateModelException.class, () -> directive.execute(null, params, new TemplateModel[0], body));
	}

	@Test
	void executeRejectsInvalidParameterTypes() {
		DescriptionDirective directive = new DescriptionDirective();

		assertThrows(TemplateModelException.class, () -> directive.execute(null, Map.of("bean", scalar("not a bean")), new TemplateModel[0], null));
		assertThrows(TemplateModelException.class, () -> directive.execute(null, Map.of("binding", bool(true)), new TemplateModel[0], null));
		assertThrows(TemplateModelException.class, () -> directive.execute(null, Map.of("escape", marker()), new TemplateModel[0], null));
		assertThrows(TemplateModelException.class, () -> directive.execute(null, Map.of("other", scalar("value")), new TemplateModel[0], null));
	}

	@Test
	void executeAcceptsEscapeParameterTypesBeforeRequiringEnvironment() {
		DescriptionDirective directive = new DescriptionDirective();

		assertThrows(NullPointerException.class, () -> directive.execute(null, Map.of("escape", scalar("false")), new TemplateModel[0], null));
		assertThrows(NullPointerException.class, () -> directive.execute(null, Map.of("escape", bool(false)), new TemplateModel[0], null));
	}

	private static TemplateScalarModel scalar(String value) {
		return () -> value;
	}

	private static TemplateBooleanModel bool(boolean value) {
		return () -> value;
	}

	private static TemplateModel marker() {
		return new TemplateModel() {
			// marker model only
		};
	}
}
