package org.skyve.metadata.sail.language;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.impl.util.XMLMetaData.SAILRoot;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.sail.language.Automation.TestStrategy;
import org.skyve.metadata.sail.language.Suite.Evidence;

/**
 * Unmarshals and marshals {@code <suite>} and the workflow {@code testStrategy} default through sail.xsd, and tells SAIL
 * roots apart. This depends on skyve-core/src/test/resources/schemas/sail.xsd being in sync with
 * skyve-war/src/main/java/schemas/sail.xsd.
 */
@SuppressWarnings("static-method")
class SuiteXmlTest {
	private static final String SUITE = """
			<?xml version="1.0" encoding="UTF-8"?>
			<!-- the admin suite -->
			<suite xmlns="http://www.skyve.org/xml/sail" testStrategy="Assert" evidence="video">
				<viewRoles user="sail{run}">
					<role>admin.BasicUser</role>
					<role>admin.ContactViewer</role>
				</viewRoles>
				<viewRoles user="tim">
					<role>admin.BasicUser</role>
				</viewRoles>
				<setUp file="00-precondition.xml"/>
				<setUp file="00-backup.xml"/>
				<member file="01-session.xml"/>
				<member file="03-security-admin.xml" lockTestStrategy="false"/>
				<member file="04-devops.xml" requires="03-security-admin.xml" lockTestStrategy="true"/>
				<restore file="99-restore.xml" requires="00-precondition.xml  00-backup.xml"/>
			</suite>
			""";

	private static final String HEAD = "<suite xmlns=\"http://www.skyve.org/xml/sail\">";
	private static final String TAIL = "</suite>";

	private static final String WORKFLOW = """
			<workflow xmlns="http://www.skyve.org/xml/sail" testStrategy="Verify">
				<automation uxui="external" userAgentType="desktop">
					<interaction name="a"><method><comment>a</comment></method></interaction>
				</automation>
				<automation uxui="desktop" userAgentType="desktop" testStrategy="None">
					<interaction name="b"><method><comment>b</comment></method></interaction>
				</automation>
			</workflow>
			""";

	private static final String AUTOMATION = """
			<automation xmlns="http://www.skyve.org/xml/sail" uxui="desktop" userAgentType="desktop">
				<interaction name="a"><method><comment>a</comment></method></interaction>
			</automation>
			""";

	@Test
	void unmarshalsASuite() {
		assertSuite(XMLMetaData.unmarshalSAILSuiteString(SUITE, "admin/suite.xml"));
	}

	@Test
	void unmarshalsASuiteFile(@TempDir Path dir) throws IOException {
		Path file = dir.resolve("suite.xml");
		Files.writeString(file, SUITE, StandardCharsets.UTF_8);
		assertSuite(XMLMetaData.unmarshalSAILSuiteFile(file.toString()));
	}

	@Test
	void marshalsAndUnmarshalsASuite() {
		String xml = XMLMetaData.marshalSAILSuite(XMLMetaData.unmarshalSAILSuiteString(SUITE));
		assertAll(() -> assertTrue(xml.contains("<suite "), xml),
					() -> assertTrue(xml.contains("requires=\"00-precondition.xml 00-backup.xml\""), xml),
					() -> assertSuite(XMLMetaData.unmarshalSAILSuiteString(xml)));
	}

	@Test
	void aSuiteThatIsNotValidAgainstTheSchemaDoesNotMarshal() {
		Suite suite = new Suite();
		MetaDataException e = assertThrows(MetaDataException.class, () -> XMLMetaData.marshalSAILSuite(suite));
		assertEquals("Could not marshal SAIL suite", e.getMessage());
	}

	@Test
	void aMinimalSuiteHasNoDefaults() {
		Suite suite = XMLMetaData.unmarshalSAILSuiteString(HEAD + "<member file=\"a.xml\"/>" + TAIL);
		OrdinarySuiteMember member = suite.getMembers().get(0);
		assertAll(() -> assertNull(suite.getTestStrategy()),
					() -> assertNull(suite.getEvidence()),
					() -> assertNull(suite.getRestore()),
					() -> assertTrue(suite.getViewRoles().isEmpty()),
					() -> assertTrue(suite.getSetUps().isEmpty()),
					() -> assertTrue(member.getRequires().isEmpty()),
					() -> assertNull(member.getLockTestStrategy()),
					() -> assertFalse(member.isTestStrategyLocked()));
	}

	@Test
	void aMinimalSuiteMarshalsWithoutEmptyAttributes() {
		String xml = XMLMetaData.marshalSAILSuite(XMLMetaData.unmarshalSAILSuiteString(HEAD + "<member file=\"a.xml\"/>" + TAIL));
		assertAll(() -> assertFalse(xml.contains("requires"), xml),
					() -> assertFalse(xml.contains("lockTestStrategy"), xml),
					() -> assertFalse(xml.contains("testStrategy"), xml));
	}

	@Test
	void everyEvidenceValueUnmarshals() {
		for (Evidence evidence : Evidence.values()) {
			Suite suite = XMLMetaData.unmarshalSAILSuiteString("<suite xmlns=\"http://www.skyve.org/xml/sail\" evidence=\"" +
																	evidence + "\"><member file=\"a.xml\"/>" + TAIL);
			assertEquals(evidence, suite.getEvidence());
		}
	}

	@Test
	void theSchemaRejectsWhatTheModelCannotHold() {
		assertAll(() -> assertSchemaRejects("<suite xmlns=\"http://www.skyve.org/xml/sail\" evidence=\"some\"><member file=\"a.xml\"/>" + TAIL),
					() -> assertSchemaRejects(HEAD + TAIL),
					() -> assertSchemaRejects(HEAD + "<setUp file=\"a.xml\"/>" + TAIL),
					() -> assertSchemaRejects(HEAD + "<setUp file=\"a.xml\" lockTestStrategy=\"true\"/><member file=\"b.xml\"/>" + TAIL),
					() -> assertSchemaRejects(HEAD + "<member file=\"b.xml\"/><restore file=\"r.xml\" lockTestStrategy=\"true\"/>" + TAIL),
					() -> assertSchemaRejects(HEAD + "<member/>" + TAIL),
					() -> assertSchemaRejects(HEAD + "<member file=\"b.xml\"/><setUp file=\"a.xml\"/>" + TAIL),
					() -> assertSchemaRejects(HEAD + "<member file=\"b.xml\"/><restore file=\"r.xml\"/><restore file=\"s.xml\"/>" + TAIL),
					() -> assertSchemaRejects(HEAD + "<viewRoles user=\"u\"/><member file=\"b.xml\"/>" + TAIL),
					() -> assertSchemaRejects(HEAD + "<viewRoles><role>r</role></viewRoles><member file=\"b.xml\"/>" + TAIL),
					() -> assertSchemaRejects(HEAD + "<member file=\"b.xml\" lockTestStrategy=\"yes\"/>" + TAIL));
	}

	@Test
	void aSuiteIsNotAWorkflowOrAnAutomation() {
		assertAll(() -> assertThrows(MetaDataException.class, () -> XMLMetaData.unmarshalSAILWorkflowString(SUITE)),
					() -> assertThrows(MetaDataException.class, () -> XMLMetaData.unmarshalSAILString(SUITE)),
					() -> assertSchemaRejects(WORKFLOW));
	}

	@Test
	void aValidationErrorFromAStringNamesTheSuiteUnwrapped() {
		String xml = HEAD + "<member file=\"a.xml\" requires=\"b.xml\"/>" + TAIL;
		MetaDataException named = assertThrows(MetaDataException.class, () -> XMLMetaData.unmarshalSAILSuiteString(xml, "sail/x/suite.xml"));
		MetaDataException unnamed = assertThrows(MetaDataException.class, () -> XMLMetaData.unmarshalSAILSuiteString(xml));
		assertAll(() -> assertEquals("SAIL suite sail/x/suite.xml: <member> a.xml requires b.xml, which is not a member of the suite",
										named.getMessage()),
					() -> assertNull(named.getCause()),
					() -> assertEquals("SAIL suite (string): <member> a.xml requires b.xml, which is not a member of the suite",
										unnamed.getMessage()));
	}

	@Test
	void aValidationErrorFromAFileNamesTheFile(@TempDir Path dir) throws IOException {
		Path file = dir.resolve("suite.xml");
		Files.writeString(file, HEAD + "<member file=\"a.xml\"/><member file=\"a.xml\"/>" + TAIL, StandardCharsets.UTF_8);
		MetaDataException e = assertThrows(MetaDataException.class, () -> XMLMetaData.unmarshalSAILSuiteFile(file.toString()));
		assertEquals("SAIL suite " + file + ": a.xml is listed more than once", e.getMessage());
	}

	@Test
	void eachValidationErrorIsReportedFromXml() {
		assertAll(() -> assertInvalid("a <member> has no file", HEAD + "<member file=\" \"/>" + TAIL),
					() -> assertInvalid("<setUp> a.xml requires itself", HEAD + "<setUp file=\"a.xml\" requires=\"a.xml\"/><member file=\"b.xml\"/>" + TAIL),
					() -> assertInvalid("<restore> r.xml requires b.xml, which is not a set-up member; a restore can require only set-up members",
											HEAD + "<member file=\"b.xml\"/><restore file=\"r.xml\" requires=\"b.xml\"/>" + TAIL),
					() -> assertInvalid("<member> c.xml is in a cycle of requires: b.xml -> c.xml -> b.xml",
											HEAD + "<member file=\"b.xml\" requires=\"c.xml\"/><member file=\"c.xml\" requires=\"b.xml\"/>" + TAIL),
					() -> assertInvalid("<member> b.xml requires c.xml, which runs after it (<member> c.xml); members run in order",
											HEAD + "<member file=\"b.xml\" requires=\"c.xml\"/><member file=\"c.xml\"/>" + TAIL),
					() -> assertInvalid("a <viewRoles> has no user", HEAD + "<viewRoles user=\"\"><role>r</role></viewRoles><member file=\"b.xml\"/>" + TAIL),
					() -> assertInvalid("<viewRoles> for user u is given more than once",
											HEAD + "<viewRoles user=\"u\"><role>r</role></viewRoles><viewRoles user=\"u\"><role>s</role></viewRoles>" +
												"<member file=\"b.xml\"/>" + TAIL),
					() -> assertInvalid("<viewRoles> for user u has a blank <role>",
											HEAD + "<viewRoles user=\"u\"><role> </role></viewRoles><member file=\"b.xml\"/>" + TAIL));
	}

	@Test
	void aMissingSuiteFileIsReported(@TempDir Path dir) {
		String file = dir.resolve("missing.xml").toString();
		MetaDataException e = assertThrows(MetaDataException.class, () -> XMLMetaData.unmarshalSAILSuiteFile(file));
		assertEquals("Could not unmarshal SAIL suite at " + file, e.getMessage());
	}

	@Test
	void aWorkflowsTestStrategyIsTheDefaultForItsAutomations() {
		Workflow workflow = XMLMetaData.unmarshalSAILWorkflowString(WORKFLOW);
		List<Automation> automations = workflow.getAutomations();
		assertAll(() -> assertEquals(TestStrategy.Verify, workflow.getTestStrategy()),
					() -> assertNull(automations.get(0).getTestStrategy()),
					() -> assertEquals(TestStrategy.Verify, workflow.getEffectiveTestStrategy(automations.get(0))),
					() -> assertEquals(TestStrategy.None, workflow.getEffectiveTestStrategy(automations.get(1))));
	}

	@Test
	void aWorkflowsTestStrategyMarshalsAndUnmarshals() {
		String xml = XMLMetaData.marshalSAILWorkflow(XMLMetaData.unmarshalSAILWorkflowString(WORKFLOW));
		assertAll(() -> assertTrue(xml.contains("testStrategy=\"Verify\""), xml),
					() -> assertEquals(TestStrategy.Verify, XMLMetaData.unmarshalSAILWorkflowString(xml).getTestStrategy()));
	}

	@Test
	void aWorkflowWithoutTestStrategyAndASingleAutomationHaveNoDefault() {
		Workflow workflow = XMLMetaData.unmarshalSAILWorkflowString(AUTOMATION);
		Automation automation = workflow.getAutomations().get(0);
		assertAll(() -> assertNull(workflow.getTestStrategy()),
					() -> assertNull(workflow.getEffectiveTestStrategy(automation)),
					() -> assertSchemaRejects("<workflow xmlns=\"http://www.skyve.org/xml/sail\" testStrategy=\"Maybe\">" +
												AUTOMATION + "</workflow>"));
	}

	@Test
	void tellsTheSailRootsApart() {
		assertAll(() -> assertEquals(SAILRoot.SUITE, XMLMetaData.sailRootOfString(SUITE)),
					() -> assertEquals(SAILRoot.WORKFLOW, XMLMetaData.sailRootOfString(WORKFLOW)),
					() -> assertEquals(SAILRoot.AUTOMATION, XMLMetaData.sailRootOfString(AUTOMATION)),
					() -> assertEquals(SAILRoot.SUITE, XMLMetaData.sailRootOfString("<s:suite xmlns:s=\"http://www.skyve.org/xml/sail\"/>")));
	}

	@Test
	void tellsTheRootOfAFile(@TempDir Path dir) throws IOException {
		Path file = dir.resolve("workflow.xml");
		Files.writeString(file, WORKFLOW, StandardCharsets.UTF_8);
		assertEquals(SAILRoot.WORKFLOW, XMLMetaData.sailRootOfFile(file.toString()));
	}

	@Test
	void rejectsRootsThatAreNotSuitesWorkflowsOrAutomations() {
		assertAll(() -> assertRootMessage("SAIL (string) has root {http://www.skyve.org/xml/sail}interaction, not a <suite>, <workflow> or " +
											"<automation> in http://www.skyve.org/xml/sail",
											"<interaction xmlns=\"http://www.skyve.org/xml/sail\" name=\"i\"/>"),
					() -> assertRootMessage("SAIL (string) has root {http://www.skyve.org/xml/view}suite, not a <suite>, <workflow> or " +
											"<automation> in http://www.skyve.org/xml/sail",
											"<suite xmlns=\"http://www.skyve.org/xml/view\"/>"),
					() -> assertRootMessage("SAIL (string) has root {}suite, not a <suite>, <workflow> or <automation> in " +
											"http://www.skyve.org/xml/sail",
											"<suite/>"),
					() -> assertRootMessage("Could not read the root element of SAIL (string)", "not xml"),
					() -> assertRootMessage("Could not read the root element of SAIL (string)", ""));
	}

	@Test
	void aMissingFileHasNoRoot(@TempDir Path dir) {
		String file = dir.resolve("missing.xml").toString();
		MetaDataException e = assertThrows(MetaDataException.class, () -> XMLMetaData.sailRootOfFile(file));
		assertEquals("Could not read SAIL at " + file, e.getMessage());
	}

	@Test
	void aDoctypeIsNotFollowed() {
		String xml = "<?xml version=\"1.0\"?><!DOCTYPE suite SYSTEM \"http://localhost:1/never.dtd\">" + SUITE.substring(SUITE.indexOf("<suite"));
		assertEquals(SAILRoot.SUITE, XMLMetaData.sailRootOfString(xml));
	}

	private static void assertSuite(Suite suite) {
		List<ViewRoles> viewRoles = suite.getViewRoles();
		List<OrdinarySuiteMember> members = suite.getMembers();
		SuiteMember restore = suite.getRestore();
		assertAll(() -> assertEquals(TestStrategy.Assert, suite.getTestStrategy()),
					() -> assertEquals(Evidence.video, suite.getEvidence()),
					() -> assertEquals(2, viewRoles.size()),
					() -> assertEquals("sail{run}", viewRoles.get(0).getUser()),
					() -> assertEquals(List.of("admin.BasicUser", "admin.ContactViewer"), viewRoles.get(0).getRoles()),
					() -> assertEquals("tim", viewRoles.get(1).getUser()),
					() -> assertEquals(List.of("00-precondition.xml", "00-backup.xml"),
										suite.getSetUps().stream().map(SuiteMember::getFile).toList()),
					() -> assertEquals(List.of("01-session.xml", "03-security-admin.xml", "04-devops.xml"),
										members.stream().map(SuiteMember::getFile).toList()),
					() -> assertTrue(members.get(0).getRequires().isEmpty()),
					() -> assertFalse(members.get(0).isTestStrategyLocked()),
					() -> assertEquals(Boolean.FALSE, members.get(1).getLockTestStrategy()),
					() -> assertFalse(members.get(1).isTestStrategyLocked()),
					() -> assertEquals(List.of("03-security-admin.xml"), members.get(2).getRequires()),
					() -> assertTrue(members.get(2).isTestStrategyLocked()),
					() -> assertEquals("99-restore.xml", (restore == null) ? null : restore.getFile()),
					() -> assertEquals(List.of("00-precondition.xml", "00-backup.xml"), (restore == null) ? null : restore.getRequires()),
					() -> assertTrue((restore != null) && restore.isTestStrategyLocked()),
					() -> assertTrue(suite.getSetUps().get(0).isTestStrategyLocked()));
	}

	private static void assertSchemaRejects(String xml) {
		MetaDataException e = assertThrows(MetaDataException.class, () -> XMLMetaData.unmarshalSAILSuiteString(xml), xml);
		assertTrue(e.getMessage().startsWith("Could not unmarshal SAIL suite (string)"), e.getMessage());
	}

	private static void assertInvalid(String expected, String xml) {
		MetaDataException e = assertThrows(MetaDataException.class, () -> XMLMetaData.unmarshalSAILSuiteString(xml), xml);
		assertEquals("SAIL suite (string): " + expected, e.getMessage());
	}

	private static void assertRootMessage(String expected, String xml) {
		MetaDataException e = assertThrows(MetaDataException.class, () -> XMLMetaData.sailRootOfString(xml), xml);
		assertEquals(expected, e.getMessage());
	}
}
