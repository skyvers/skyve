package org.skyve.impl.report.freemarker;

import static org.junit.jupiter.api.Assertions.assertThrows;

import java.util.LinkedHashMap;
import java.util.Map;

import org.junit.jupiter.api.Test;

import freemarker.template.TemplateBooleanModel;
import freemarker.template.TemplateDirectiveBody;
import freemarker.template.TemplateModel;
import freemarker.template.TemplateModelException;
import freemarker.template.TemplateScalarModel;

@SuppressWarnings("static-method")
class FormatDirectiveTest {
	@Test
	void executeRejectsMissingParameters() {
		FormatDirective directive = new FormatDirective();

		assertThrows(TemplateModelException.class, () -> directive.execute(null, Map.of(), new TemplateModel[0], null));
	}

	@Test
	void executeRejectsLoopVariables() {
		FormatDirective directive = new FormatDirective();
		Map<String, TemplateModel> params = Map.of("binding", scalar("name"));

		assertThrows(TemplateModelException.class, () -> directive.execute(null, params, new TemplateModel[] { scalar("loop") }, null));
	}

	@Test
	void executeRejectsNestedBody() {
		FormatDirective directive = new FormatDirective();
		Map<String, TemplateModel> params = Map.of("binding", scalar("name"));
		TemplateDirectiveBody body = env -> {
			// no-op
		};

		assertThrows(TemplateModelException.class, () -> directive.execute(null, params, new TemplateModel[0], body));
	}

	@Test
	void executeRejectsNullBeanParameter() {
		FormatDirective directive = new FormatDirective();
		Map<String, TemplateModel> params = new LinkedHashMap<>();
		params.put("bean", nullModel());

		assertThrows(TemplateModelException.class, () -> directive.execute(null, params, new TemplateModel[0], null));
	}

	@Test
	void executeRejectsNonBeanParameter() {
		FormatDirective directive = new FormatDirective();
		Map<String, TemplateModel> params = new LinkedHashMap<>();
		params.put("bean", scalar("not a bean"));

		assertThrows(TemplateModelException.class, () -> directive.execute(null, params, new TemplateModel[0], null));
	}

	@Test
	void executeRejectsNonScalarBinding() {
		FormatDirective directive = new FormatDirective();
		Map<String, TemplateModel> params = Map.of("binding", bool(true));

		assertThrows(TemplateModelException.class, () -> directive.execute(null, params, new TemplateModel[0], null));
	}

	@Test
	void executeRejectsNonScalarExpression() {
		FormatDirective directive = new FormatDirective();
		Map<String, TemplateModel> params = Map.of("expression", bool(true));

		assertThrows(TemplateModelException.class, () -> directive.execute(null, params, new TemplateModel[0], null));
	}

	@Test
	void executeRejectsBothBindingAndExpression() {
		FormatDirective directive = new FormatDirective();
		Map<String, TemplateModel> params = new LinkedHashMap<>();
		params.put("binding", scalar("name"));
		params.put("expression", scalar("{name}"));

		assertThrows(TemplateModelException.class, () -> directive.execute(null, params, new TemplateModel[0], null));
	}

	@Test
	void executeRejectsNonBooleanEscape() {
		FormatDirective directive = new FormatDirective();
		Map<String, TemplateModel> params = Map.of("escape", new TemplateModel() {
			// marker model only
		});

		assertThrows(TemplateModelException.class, () -> directive.execute(null, params, new TemplateModel[0], null));
	}

	@Test
	void executeRejectsUnsupportedParameter() {
		FormatDirective directive = new FormatDirective();
		Map<String, TemplateModel> params = Map.of("other", scalar("value"));

		assertThrows(TemplateModelException.class, () -> directive.execute(null, params, new TemplateModel[0], null));
	}

	private static TemplateScalarModel scalar(String value) {
		return () -> value;
	}

	private static TemplateBooleanModel bool(boolean value) {
		return () -> value;
	}

	private static TemplateModel nullModel() {
		return new TemplateModel() {
			// DeepUnwrap treats an unknown marker model as null.
		};
	}
}
