package org.skyve.impl.report.freemarker;

import static org.junit.jupiter.api.Assertions.assertThrows;

import java.util.Map;

import org.junit.jupiter.api.Test;

import freemarker.template.TemplateDirectiveBody;
import freemarker.template.TemplateModel;
import freemarker.template.TemplateModelException;
import freemarker.template.TemplateNumberModel;
import freemarker.template.TemplateScalarModel;

@SuppressWarnings("static-method")
class ContentDirectiveValidationTest {
	@Test
	void executeRejectsMissingParametersLoopVariablesAndNestedBody() {
		ContentDirective directive = new ContentDirective();
		Map<String, TemplateModel> params = Map.of("attribute", scalar("image"));
		TemplateDirectiveBody body = env -> {
			// no-op
		};

		assertThrows(TemplateModelException.class, () -> directive.execute(null, Map.of(), new TemplateModel[0], null));
		assertThrows(TemplateModelException.class, () -> directive.execute(null, params, new TemplateModel[] { scalar("loop") }, null));
		assertThrows(TemplateModelException.class, () -> directive.execute(null, params, new TemplateModel[0], body));
	}

	@Test
	void executeRejectsInvalidParameterTypes() {
		ContentDirective directive = new ContentDirective();

		assertThrows(TemplateModelException.class, () -> directive.execute(null, Map.of("bean", scalar("not a bean")), new TemplateModel[0], null));
		assertThrows(TemplateModelException.class, () -> directive.execute(null, Map.of("module", marker()), new TemplateModel[0], null));
		assertThrows(TemplateModelException.class, () -> directive.execute(null, Map.of("document", marker()), new TemplateModel[0], null));
		assertThrows(TemplateModelException.class, () -> directive.execute(null, Map.of("attribute", marker()), new TemplateModel[0], null));
		assertThrows(TemplateModelException.class, () -> directive.execute(null, Map.of("height", marker()), new TemplateModel[0], null));
		assertThrows(TemplateModelException.class, () -> directive.execute(null, Map.of("width", marker()), new TemplateModel[0], null));
		assertThrows(TemplateModelException.class, () -> directive.execute(null, Map.of("class", marker()), new TemplateModel[0], null));
		assertThrows(TemplateModelException.class, () -> directive.execute(null, Map.of("style", marker()), new TemplateModel[0], null));
		assertThrows(TemplateModelException.class, () -> directive.execute(null, Map.of("other", scalar("value")), new TemplateModel[0], null));
	}

	@Test
	void executeAcceptsScalarAndNumericDimensionsThenRequiresMissingParameters() {
		ContentDirective directive = new ContentDirective();
		Map<String, TemplateModel> params = Map.of(
				"attribute", scalar("image"),
				"document", scalar("User"),
				"module", scalar("admin"),
				"height", number(Integer.valueOf(10)),
				"width", scalar("20"),
				"class", scalar("photo"),
				"style", scalar("border:0"));

		assertThrows(TemplateModelException.class, () -> directive.execute(null, params, new TemplateModel[0], null));
	}

	private static TemplateScalarModel scalar(String value) {
		return () -> value;
	}

	private static TemplateNumberModel number(Number value) {
		return () -> value;
	}

	private static TemplateModel marker() {
		return new TemplateModel() {
			// marker model only
		};
	}
}
