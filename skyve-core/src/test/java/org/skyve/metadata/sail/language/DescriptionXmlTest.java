package org.skyve.metadata.sail.language;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.List;

import org.junit.jupiter.api.Test;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.MetaDataException;

/**
 * The {@code <description>} of a suite, a workflow, an automation and an interaction: through sail.xsd, tidied, and
 * marshalled back. This depends on skyve-core/src/test/resources/schemas/sail.xsd being in sync with
 * skyve-war/src/main/java/schemas/sail.xsd.
 */
@SuppressWarnings("static-method")
class DescriptionXmlTest {
	private static final String WORKFLOW = """
			<workflow xmlns="http://www.skyve.org/xml/sail">
				<description>
					Ben claims a taxi fare,
					and Rob approves it.

					The claim then shows on the next pay run for {run}.
				</description>
				<automation uxui="external" userAgentType="desktop">
					<description>Ben lodges the claim.</description>
					<interaction name="Lodge">
						<description>
							Signs in, adds the line
							and submits.
						</description>
						<method><comment>a</comment></method>
					</interaction>
					<interaction name="No description"><method><comment>b</comment></method></interaction>
				</automation>
			</workflow>
			""";

	private static final String SUITE = """
			<suite xmlns="http://www.skyve.org/xml/sail">
				<description>
					The admin module, end to end.
				</description>
				<member file="a.xml"/>
			</suite>
			""";

	@Test
	void eachLevelUnmarshalsItsDescriptionTidied() {
		Workflow workflow = XMLMetaData.unmarshalSAILWorkflowString(WORKFLOW);
		Automation automation = workflow.getAutomations().get(0);
		List<Interaction> interactions = automation.getInteractions();
		assertAll(() -> assertEquals("Ben claims a taxi fare, and Rob approves it.\n\nThe claim then shows on the next pay run for {run}.",
										workflow.getDescription()),
					() -> assertEquals("Ben lodges the claim.", automation.getDescription()),
					() -> assertEquals("Signs in, adds the line and submits.", interactions.get(0).getDescription()),
					() -> assertNull(interactions.get(1).getDescription()),
					() -> assertEquals("The admin module, end to end.", XMLMetaData.unmarshalSAILSuiteString(SUITE, "suite.xml").getDescription()));
	}

	@Test
	void aWorkflowMarshalsItsDescriptionsAndReadsThemBack() {
		String xml = XMLMetaData.marshalSAILWorkflow(XMLMetaData.unmarshalSAILWorkflowString(WORKFLOW));
		Workflow again = XMLMetaData.unmarshalSAILWorkflowString(xml);
		Automation automation = again.getAutomations().get(0);
		assertAll(() -> assertTrue(xml.contains("description>"), xml),
					() -> assertEquals(XMLMetaData.unmarshalSAILWorkflowString(WORKFLOW).getDescription(), again.getDescription()),
					() -> assertEquals("Ben lodges the claim.", automation.getDescription()),
					() -> assertEquals("Signs in, adds the line and submits.", automation.getInteractions().get(0).getDescription()));
	}

	@Test
	void anAutomationMarshalsItsDescription() {
		Automation automation = XMLMetaData.unmarshalSAILWorkflowString(WORKFLOW).getAutomations().get(0);
		Automation again = XMLMetaData.unmarshalSAILString(XMLMetaData.marshalSAIL(automation));
		assertAll(() -> assertEquals("Ben lodges the claim.", again.getDescription()),
					() -> assertEquals("Signs in, adds the line and submits.", again.getInteractions().get(0).getDescription()));
	}

	@Test
	void theSchemaWantsTheDescriptionFirstAndOnce() {
		String late = """
				<automation xmlns="http://www.skyve.org/xml/sail" uxui="desktop" userAgentType="desktop">
					<interaction name="a"><method><comment>a</comment></method><description>late</description></interaction>
				</automation>
				""";
		String twice = """
				<workflow xmlns="http://www.skyve.org/xml/sail">
					<description>one</description>
					<description>two</description>
					<automation uxui="desktop" userAgentType="desktop">
						<interaction name="a"><method><comment>a</comment></method></interaction>
					</automation>
				</workflow>
				""";
		String afterViewRoles = """
				<suite xmlns="http://www.skyve.org/xml/sail">
					<viewRoles user="tim"><role>admin.BasicUser</role></viewRoles>
					<description>late</description>
					<member file="a.xml"/>
				</suite>
				""";
		assertAll(() -> assertThrows(MetaDataException.class, () -> XMLMetaData.unmarshalSAILString(late)),
					() -> assertThrows(MetaDataException.class, () -> XMLMetaData.unmarshalSAILWorkflowString(twice)),
					() -> assertThrows(MetaDataException.class, () -> XMLMetaData.unmarshalSAILSuiteString(afterViewRoles, "suite.xml")));
	}

	@Test
	void anEmptyDescriptionIsNone() {
		Workflow workflow = XMLMetaData.unmarshalSAILWorkflowString(WORKFLOW.replaceFirst("(?s)<description>.*?</description>", "<description>  \n  </description>"));
		assertNull(workflow.getDescription());
	}
}
