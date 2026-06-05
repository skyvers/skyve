package org.skyve.impl.report.freemarker;

import static org.junit.jupiter.api.Assertions.assertThrows;

import java.util.Map;

import org.junit.jupiter.api.Test;

import freemarker.template.TemplateDirectiveBody;
import freemarker.template.TemplateModel;
import freemarker.template.TemplateModelException;
import freemarker.template.TemplateScalarModel;

@SuppressWarnings("static-method")
class DisplayNameDirectiveTest {
	@Test
	void executeRejectsMissingParametersLoopVariablesAndNestedBody() {
		DisplayNameDirective directive = new DisplayNameDirective();
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
		DisplayNameDirective directive = new DisplayNameDirective();

		assertThrows(TemplateModelException.class, () -> directive.execute(null, Map.of("bean", scalar("not a bean")), new TemplateModel[0], null));
		assertThrows(TemplateModelException.class, () -> directive.execute(null, Map.of("binding", marker()), new TemplateModel[0], null));
		assertThrows(TemplateModelException.class, () -> directive.execute(null, Map.of("other", scalar("value")), new TemplateModel[0], null));
	}

	@Test
	void executeAcceptsBindingBeforeRequiringEnvironment() {
		DisplayNameDirective directive = new DisplayNameDirective();

		assertThrows(NullPointerException.class, () -> directive.execute(null, Map.of("binding", scalar("name")), new TemplateModel[0], null));
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
