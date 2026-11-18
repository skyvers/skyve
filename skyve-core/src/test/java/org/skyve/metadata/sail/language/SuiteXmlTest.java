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
 * Unmarshals {@code <suite>} through sail.xsd, and tells SAIL roots apart. This depends on
 * skyve-core/src/test/resources/schemas/sail.xsd being in sync with skyve-war/src/main/java/schemas/sail.xsd.
 */
@SuppressWarnings("static-method")
class SuiteXmlTest {
	private static final String NAME = "sail/admin/suite.xml";

	private static final String SUITE = """
			<?xml version="1.0" encoding="UTF-8"?>
			<!-- the admin suite -->
			<suite xmlns="http://www.skyve.org/xml/sail" testStrategy="Assert" evidence="video">
				<viewRoles user="sail{run}">
					<role>admin.BasicUser</role>
					<role>
						admin.ContactViewer
					</role>
				</viewRoles>
				<viewRoles user="tim">
					<role>admin.BasicUser</role>
				</viewRoles>
				<setUp file="00-precondition.xml"/>
				<setUp file="00-backup.xml"/>
				<member file="pf/01-session.xml"/>
				<member file="sc/01-session.xml"/>
				<member file="pf/03-security-admin.xml" lockTestStrategy="false"/>
				<member file="sc/04-devops.xml" requires="pf/03-security-admin.xml" lockTestStrategy="true"/>
				<restore file="99-restore.xml" requires="00-precondition.xml  00-backup.xml"/>
			</suite>
			""";

	private static final String HEAD = "<suite xmlns=\"http://www.skyve.org/xml/sail\">";
	private static final String TAIL = "</suite>";
	private static final String XSI = "xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" ";

	private static final String WORKFLOW = """
			<workflow xmlns="http://www.skyve.org/xml/sail">
				<automation uxui="external" userAgentType="desktop">
					<interaction name="a"><method><comment>a</comment></method></interaction>
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
		assertSuite(XMLMetaData.unmarshalSAILSuiteString(SUITE, NAME));
	}

	@Test
	void unmarshalsASuiteFile(@TempDir Path dir) throws IOException {
		Path file = dir.resolve("suite.xml");
		Files.writeString(file, SUITE, StandardCharsets.UTF_8);
		assertSuite(XMLMetaData.unmarshalSAILSuiteFile(file.toString()));
	}

	@Test
	void aMinimalSuiteHasNoDefaults() {
		Suite suite = XMLMetaData.unmarshalSAILSuiteString(HEAD + "<member file=\"a.xml\"/>" + TAIL, NAME);
		OrdinarySuiteMember member = suite.getMembers().get(0);
		assertAll(() -> assertNull(suite.getTestStrategy()),
					() -> assertNull(suite.getEvidence()),
					() -> assertNull(suite.getBrowserPerMember(), "absent: the browser is handed on"),
					() -> assertNull(suite.getRestore()),
					() -> assertTrue(suite.getViewRoles().isEmpty()),
					() -> assertTrue(suite.getSetUps().isEmpty()),
					() -> assertTrue(member.getRequires().isEmpty()),
					() -> assertNull(member.getLockTestStrategy()),
					() -> assertFalse(suite.isTestStrategyLocked(member)));
	}

	@Test
	void browserPerMemberUnmarshalsAndTheSchemaChecksIt() {
		String head = "<suite xmlns=\"http://www.skyve.org/xml/sail\" browserPerMember=\"";
		String tail = "\"><member file=\"a.xml\"/>" + TAIL;
		assertAll(() -> assertEquals(Boolean.TRUE, XMLMetaData.unmarshalSAILSuiteString(head + "true" + tail, NAME).getBrowserPerMember()),
					() -> assertEquals(Boolean.FALSE, XMLMetaData.unmarshalSAILSuiteString(head + "false" + tail, NAME).getBrowserPerMember()),
					() -> assertSchemaRejects(head + "yes" + tail));
	}

	@Test
	void everyEvidenceValueUnmarshals() {
		for (Evidence evidence : Evidence.values()) {
			Suite suite = XMLMetaData.unmarshalSAILSuiteString("<suite xmlns=\"http://www.skyve.org/xml/sail\" evidence=\"" +
																	evidence + "\"><member file=\"a.xml\"/>" + TAIL, NAME);
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
	void xsiTypeCannotMakeASetUpOrRestoreAnOrdinaryMember() {
		String tns = "xmlns:tns=\"http://www.skyve.org/xml/sail\" ";
		assertAll(() -> assertSchemaRejects(HEAD + "<setUp " + XSI + tns + "xsi:type=\"tns:ordinarySuiteMember\" file=\"a.xml\" " +
												"lockTestStrategy=\"false\"/><member file=\"b.xml\"/>" + TAIL),
					() -> assertSchemaRejects(HEAD + "<member file=\"b.xml\"/><restore " + XSI + tns +
												"xsi:type=\"tns:ordinarySuiteMember\" file=\"r.xml\"/>" + TAIL),
					() -> assertSchemaRejects(HEAD + "<member " + XSI + tns + "xsi:type=\"tns:suiteMember\" file=\"b.xml\"/>" + TAIL));
	}

	@Test
	void aSuiteIsNotAWorkflowOrAnAutomation() {
		assertAll(() -> assertThrows(MetaDataException.class, () -> XMLMetaData.unmarshalSAILWorkflowString(SUITE)),
					() -> assertThrows(MetaDataException.class, () -> XMLMetaData.unmarshalSAILString(SUITE)),
					() -> assertSchemaRejects(WORKFLOW));
	}

	@Test
	void aValidationErrorNamesTheSuiteUnwrapped() {
		String xml = HEAD + "<member file=\"a.xml\" requires=\"b.xml\"/>" + TAIL;
		MetaDataException e = assertThrows(MetaDataException.class, () -> XMLMetaData.unmarshalSAILSuiteString(xml, NAME));
		assertAll(() -> assertEquals("SAIL suite sail/admin/suite.xml: <member> a.xml requires b.xml, which is not a member of the suite",
										e.getMessage()),
					() -> assertNull(e.getCause()));
	}

	@Test
	void aValidationErrorFromAFileNamesTheFile(@TempDir Path dir) throws IOException {
		Path path = dir.resolve("suite.xml");
		Files.writeString(path, HEAD + "<member file=\"a.xml\"/><member file=\"a.xml\"/>" + TAIL, StandardCharsets.UTF_8);
		String file = path.toString();
		MetaDataException e = assertThrows(MetaDataException.class, () -> XMLMetaData.unmarshalSAILSuiteFile(file));
		assertEquals("SAIL suite " + file + ": a.xml is listed more than once", e.getMessage());
	}

	@Test
	void eachValidationErrorIsReportedFromXml() {
		assertAll(() -> assertInvalid("a <member> has no file", HEAD + "<member file=\" \"/>" + TAIL),
					() -> assertInvalid("<member> file pf/../01-session.xml has an empty, . or .. segment; it must be a normalised path " +
											"within the suite's folder", HEAD + "<member file=\"pf/../01-session.xml\"/>" + TAIL),
					() -> assertInvalid("<member> file pf/01 session.xml has white space; requires separates files with spaces",
											HEAD + "<member file=\"pf/01 session.xml\"/>" + TAIL),
					() -> assertInvalid("<setUp> a.xml requires itself", HEAD + "<setUp file=\"a.xml\" requires=\"a.xml\"/><member file=\"b.xml\"/>" + TAIL),
					() -> assertInvalid("<member> sc/b.xml requires pf/a.xml more than once",
											HEAD + "<member file=\"pf/a.xml\"/><member file=\"sc/b.xml\" requires=\"pf/a.xml pf/a.xml\"/>" + TAIL),
					() -> assertInvalid("<restore> r.xml requires b.xml, which is not a set-up member; a restore can require only set-up members",
											HEAD + "<member file=\"b.xml\"/><restore file=\"r.xml\" requires=\"b.xml\"/>" + TAIL),
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
	void tellsTheSailRootsApart() {
		assertAll(() -> assertEquals(SAILRoot.SUITE, XMLMetaData.sailRootOfString(SUITE)),
					() -> assertEquals(SAILRoot.WORKFLOW, XMLMetaData.sailRootOfString(WORKFLOW)),
					() -> assertEquals(SAILRoot.AUTOMATION, XMLMetaData.sailRootOfString(AUTOMATION)),
					() -> assertEquals(SAILRoot.SUITE, XMLMetaData.sailRootOfString("<s:suite xmlns:s=\"http://www.skyve.org/xml/sail\"/>")));
	}

	@Test
	void tellsTheRootOfAFile(@TempDir Path dir) throws IOException {
		Path path = dir.resolve("workflow.xml");
		Files.writeString(path, WORKFLOW, StandardCharsets.UTF_8);
		assertEquals(SAILRoot.WORKFLOW, XMLMetaData.sailRootOfFile(path.toString()));
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
					() -> assertEquals(List.of("pf/01-session.xml", "sc/01-session.xml", "pf/03-security-admin.xml", "sc/04-devops.xml"),
										members.stream().map(SuiteMember::getFile).toList()),
					() -> assertTrue(members.get(0).getRequires().isEmpty()),
					() -> assertFalse(suite.isTestStrategyLocked(members.get(0))),
					() -> assertEquals(Boolean.FALSE, members.get(2).getLockTestStrategy()),
					() -> assertFalse(suite.isTestStrategyLocked(members.get(2))),
					() -> assertEquals(List.of("pf/03-security-admin.xml"), members.get(3).getRequires()),
					() -> assertTrue(suite.isTestStrategyLocked(members.get(3))),
					() -> assertEquals(List.of("00-precondition.xml", "00-backup.xml", "pf/03-security-admin.xml"),
										suite.requirementsOf(members.get(3)).stream().map(SuiteMember::getFile).toList()),
					() -> assertEquals("99-restore.xml", (restore == null) ? null : restore.getFile()),
					() -> assertEquals(List.of("00-precondition.xml", "00-backup.xml"), (restore == null) ? null : restore.getRequires()),
					() -> assertTrue((restore != null) && suite.isTestStrategyLocked(restore)),
					() -> assertTrue(suite.isTestStrategyLocked(suite.getSetUps().get(0))));
	}

	private static void assertSchemaRejects(String xml) {
		MetaDataException e = assertThrows(MetaDataException.class, () -> XMLMetaData.unmarshalSAILSuiteString(xml, NAME), xml);
		assertTrue(e.getMessage().startsWith("Could not unmarshal SAIL suite " + NAME), e.getMessage());
	}

	private static void assertInvalid(String expected, String xml) {
		MetaDataException e = assertThrows(MetaDataException.class, () -> XMLMetaData.unmarshalSAILSuiteString(xml, NAME), xml);
		assertEquals("SAIL suite " + NAME + ": " + expected, e.getMessage());
	}

	private static void assertRootMessage(String expected, String xml) {
		MetaDataException e = assertThrows(MetaDataException.class, () -> XMLMetaData.sailRootOfString(xml), xml);
		assertEquals(expected, e.getMessage());
	}
}
