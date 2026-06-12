package org.skyve.toolchain.freemarker;

import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;

import java.io.StringWriter;
import java.util.LinkedHashMap;
import java.util.Map;

import org.junit.jupiter.api.Test;

import freemarker.template.Configuration;
import freemarker.template.SimpleScalar;
import freemarker.template.Template;
import freemarker.template.TemplateDirectiveBody;
import freemarker.template.TemplateModel;
import freemarker.template.TemplateModelException;

@SuppressWarnings("static-method")
class PlantUMLDirectiveTest {

	@Test
	void executeWritesGeneratedImageForMarkupParameter() throws Exception {
		PlantUMLDirective directive = new PlantUMLDirective();
		StringWriter out = new StringWriter();
		Template template = new Template("plantuml-test", "<@plant markup=markup />",
				new Configuration(Configuration.VERSION_2_3_32));
		Map<String, Object> dataModel = new LinkedHashMap<>();
		dataModel.put("plant", directive);
		dataModel.put("markup", """
				@startuml
				Alice -> Bob
				@enduml
				""");

		template.process(dataModel, out);

		String html = out.toString();
		assertTrue(html.startsWith("<img src='data:image/png;base64,"));
		assertTrue(html.endsWith("' />"));
	}

	@Test
	void executeRejectsMissingParameters() {
		PlantUMLDirective directive = new PlantUMLDirective();

		TemplateModelException exception = assertThrows(TemplateModelException.class,
				() -> directive.execute(null, Map.of(), new TemplateModel[0], null));

		assertTrue(exception.getMessage().contains("requires parameters"));
	}

	@Test
	void executeRejectsLoopVariables() {
		PlantUMLDirective directive = new PlantUMLDirective();
		Map<String, TemplateModel> params = Map.of("markup", new SimpleScalar("@startuml\n@enduml"));

		TemplateModelException exception = assertThrows(TemplateModelException.class,
				() -> directive.execute(null, params, new TemplateModel[] {mock(TemplateModel.class)}, null));

		assertTrue(exception.getMessage().contains("doesn't allow loop variables"));
	}

	@Test
	void executeRejectsNestedContent() {
		PlantUMLDirective directive = new PlantUMLDirective();
		Map<String, TemplateModel> params = Map.of("markup", new SimpleScalar("@startuml\n@enduml"));

		TemplateModelException exception = assertThrows(TemplateModelException.class,
				() -> directive.execute(null, params, new TemplateModel[0], mock(TemplateDirectiveBody.class)));

		assertTrue(exception.getMessage().contains("doesn't allow nested content"));
	}

	@Test
	void executeRejectsInvalidParameters() {
		PlantUMLDirective directive = new PlantUMLDirective();
		Object[][] cases = {
				{Map.of("unknown", new SimpleScalar("value")), "Unsupported parameter"},
				{Map.of("markup", mock(TemplateModel.class)), "'markup' parameter must be a String"},
				{Map.of("bean", new SimpleScalar("not-a-bean")), "'bean' parameter must be a Skyve bean"},
				{Map.of("binding", mock(TemplateModel.class)), "'binding' parameter must be a String"},
				{Map.of("binding", new SimpleScalar("markup")), "Expected bean and binding, or markup"}
		};
		for (Object[] testCase : cases) {
			@SuppressWarnings("unchecked")
			Map<String, TemplateModel> params = (Map<String, TemplateModel>) testCase[0];
			TemplateModelException exception = assertThrows(TemplateModelException.class,
					() -> directive.execute(null, params, new TemplateModel[0], null));
			assertTrue(exception.getMessage().contains((String) testCase[1]));
		}
	}
}
