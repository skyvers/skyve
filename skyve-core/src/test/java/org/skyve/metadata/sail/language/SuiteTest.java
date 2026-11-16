package org.skyve.metadata.sail.language;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.Test;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.sail.language.Automation.TestStrategy;

/**
 * Tests the {@link Suite} model and {@link Suite#validate(String)} on suites built in code. The XML forms are in
 * {@link SuiteXmlTest}.
 */
@SuppressWarnings("static-method")
class SuiteTest {
	@Test
	void allMembersAreInRunOrder() {
		Suite suite = suite("00-a.xml 00-b.xml", "01-c.xml 02-d.xml", "99-e.xml");
		assertEquals(List.of("00-a.xml", "00-b.xml", "01-c.xml", "02-d.xml", "99-e.xml"), files(suite.getAllMembers()));
	}

	@Test
	void allMembersWithoutARestoreEndWithTheLastMember() {
		Suite suite = suite("", "01-c.xml", null);
		assertAll(() -> assertEquals(List.of("01-c.xml"), files(suite.getAllMembers())),
					() -> assertThrows(UnsupportedOperationException.class, () -> suite.getAllMembers().clear()));
	}

	@Test
	void getMemberFindsEachKindByFile() {
		Suite suite = suite("00-a.xml", "01-c.xml", "99-e.xml");
		assertAll(() -> assertSame(suite.getSetUps().get(0), suite.getMember("00-a.xml")),
					() -> assertSame(suite.getMembers().get(0), suite.getMember("01-c.xml")),
					() -> assertSame(suite.getRestore(), suite.getMember("99-e.xml")),
					() -> assertNull(suite.getMember("nope.xml")));
	}

	@Test
	void isSetUpIsTrueOnlyForSetUpMembers() {
		Suite suite = suite("00-a.xml", "01-c.xml", "99-e.xml");
		SuiteMember restore = restore(suite);
		assertAll(() -> assertTrue(suite.isSetUp(suite.getSetUps().get(0))),
					() -> assertFalse(suite.isSetUp(suite.getMembers().get(0))),
					() -> assertFalse(suite.isSetUp(restore)));
	}

	@Test
	void anOrdinaryMemberRequiresEverySetUpAndWhatItNamesInRunOrderOnce() {
		Suite suite = suite("00-a.xml 00-b.xml", "01-c.xml 02-d.xml", "99-e.xml");
		OrdinarySuiteMember d = suite.getMembers().get(1);
		d.getRequires().addAll(List.of("01-c.xml", "00-a.xml"));
		assertEquals(List.of("00-a.xml", "00-b.xml", "01-c.xml"), files(suite.requirementsOf(d)));
	}

	@Test
	void anOrdinaryMemberWithNoRequiresRequiresOnlyTheSetUps() {
		Suite suite = suite("00-a.xml", "01-c.xml", "99-e.xml");
		assertEquals(List.of("00-a.xml"), files(suite.requirementsOf(suite.getMembers().get(0))));
	}

	@Test
	void setUpAndRestoreRequireOnlyWhatTheyName() {
		Suite suite = suite("00-a.xml 00-b.xml", "01-c.xml", "99-e.xml");
		SuiteMember b = suite.getSetUps().get(1);
		SuiteMember restore = restore(suite);
		assertAll(() -> assertTrue(suite.requirementsOf(suite.getSetUps().get(0)).isEmpty()),
					() -> assertTrue(suite.requirementsOf(b).isEmpty()),
					() -> assertTrue(suite.requirementsOf(restore).isEmpty()));
		b.getRequires().add("00-a.xml");
		restore.getRequires().add("00-b.xml");
		assertAll(() -> assertEquals(List.of("00-a.xml"), files(suite.requirementsOf(b))),
					() -> assertEquals(List.of("00-b.xml"), files(suite.requirementsOf(restore))));
	}

	@Test
	void setUpAndRestoreAreAlwaysLockedAndAnOrdinaryMemberOnlyWhenItSaysSo() {
		OrdinarySuiteMember ordinary = new OrdinarySuiteMember();
		assertAll(() -> assertTrue(new SuiteMember().isTestStrategyLocked()),
					() -> assertNull(ordinary.getLockTestStrategy()),
					() -> assertFalse(ordinary.isTestStrategyLocked()));
		ordinary.setLockTestStrategy(Boolean.FALSE);
		assertFalse(ordinary.isTestStrategyLocked());
		ordinary.setLockTestStrategy(Boolean.TRUE);
		assertAll(() -> assertEquals(Boolean.TRUE, ordinary.getLockTestStrategy()),
					() -> assertTrue(ordinary.isTestStrategyLocked()));
	}

	@Test
	void theRequiresAttributeIsAbsentWhenEmptyAndReplacesTheRequirements() {
		SuiteMember member = new SuiteMember();
		assertNull(member.getRequiresAttribute());
		member.setRequiresAttribute(new ArrayList<>(List.of("a.xml", "b.xml")));
		assertAll(() -> assertEquals(List.of("a.xml", "b.xml"), member.getRequires()),
					() -> assertEquals(List.of("a.xml", "b.xml"), member.getRequiresAttribute()));
		member.setRequiresAttribute(null);
		assertAll(() -> assertTrue(member.getRequires().isEmpty()),
					() -> assertNull(member.getRequiresAttribute()));
	}

	@Test
	void theAutomationsStrategyWinsThenTheWorkflowsThenTheSuites() {
		Suite suite = new Suite();
		Workflow workflow = new Workflow();
		Automation automation = new Automation();
		assertNull(suite.getEffectiveTestStrategy(workflow, automation));
		suite.setTestStrategy(TestStrategy.None);
		assertEquals(TestStrategy.None, suite.getEffectiveTestStrategy(workflow, automation));
		workflow.setTestStrategy(TestStrategy.Verify);
		assertEquals(TestStrategy.Verify, suite.getEffectiveTestStrategy(workflow, automation));
		automation.setTestStrategy(TestStrategy.Assert);
		assertEquals(TestStrategy.Assert, suite.getEffectiveTestStrategy(workflow, automation));
	}

	@Test
	void settersAndDefaults() {
		Suite suite = new Suite();
		assertAll(() -> assertNull(suite.getTestStrategy()),
					() -> assertNull(suite.getEvidence()),
					() -> assertNull(suite.getRestore()),
					() -> assertTrue(suite.getViewRoles().isEmpty()),
					() -> assertTrue(suite.getSetUps().isEmpty()),
					() -> assertTrue(suite.getMembers().isEmpty()));
		suite.setTestStrategy(TestStrategy.Verify);
		suite.setEvidence(Suite.Evidence.snapshots);
		SuiteMember restore = member(new SuiteMember(), " 99.xml ");
		suite.setRestore(restore);
		ViewRoles viewRoles = new ViewRoles();
		viewRoles.setUser(" jane{run} ");
		assertAll(() -> assertEquals(TestStrategy.Verify, suite.getTestStrategy()),
					() -> assertEquals(Suite.Evidence.snapshots, suite.getEvidence()),
					() -> assertSame(restore, suite.getRestore()),
					() -> assertEquals("99.xml", restore.getFile()),
					() -> assertEquals("jane{run}", viewRoles.getUser()),
					() -> assertTrue(viewRoles.getRoles().isEmpty()));
	}

	@Test
	void evidenceValuesAreTheXmlValuesInIncreasingOrder() {
		assertEquals(List.of("none", "failures", "snapshots", "video", "all"),
						List.of(Suite.Evidence.values()).stream().map(Enum::name).toList());
	}

	@Test
	void aValidSuitePasses() {
		Suite suite = suite("00-a.xml 00-b.xml", "01-c.xml 02-d.xml", "99-e.xml");
		suite.getSetUps().get(1).getRequires().add("00-a.xml");
		suite.getMembers().get(1).getRequires().addAll(List.of("01-c.xml", "00-a.xml"));
		restore(suite).getRequires().addAll(List.of("00-a.xml", "00-b.xml"));
		suite.getViewRoles().add(viewRoles("jane{run}", "admin.BasicUser"));
		suite.getViewRoles().add(viewRoles("tim", "admin.BasicUser", "admin.ContactViewer"));
		assertDoesNotThrow(() -> suite.validate("s.xml"));
	}

	@Test
	void aSuiteWithoutSetUpsOrRestorePasses() {
		assertDoesNotThrow(() -> suite("", "01-c.xml", null).validate("s.xml"));
	}

	@Test
	void aSuiteWithNoOrdinaryMemberIsRejected() {
		assertMessage("SAIL suite s.xml: there is no <member>", suite("00-a.xml", "", "99-e.xml"));
	}

	@Test
	void aMemberWithoutAFileIsRejected() {
		Suite suite = suite("00-a.xml", "01-c.xml", null);
		suite.getMembers().add(member(new OrdinarySuiteMember(), " "));
		assertMessage("SAIL suite s.xml: a <member> has no file", suite);
	}

	@Test
	void aSetUpOrRestoreWithoutAFileIsRejected() {
		Suite setUp = suite("", "01-c.xml", null);
		setUp.getSetUps().add(new SuiteMember());
		Suite restore = suite("", "01-c.xml", null);
		restore.setRestore(new SuiteMember());
		assertAll(() -> assertMessage("SAIL suite s.xml: a <setUp> has no file", setUp),
					() -> assertMessage("SAIL suite s.xml: a <restore> has no file", restore));
	}

	@Test
	void aFileListedTwiceIsRejected() {
		assertAll(() -> assertMessage("SAIL suite s.xml: 01-c.xml is listed more than once", suite("", "01-c.xml 01-c.xml", null)),
					() -> assertMessage("SAIL suite s.xml: 00-a.xml is listed more than once", suite("00-a.xml", "01-c.xml", "00-a.xml")));
	}

	@Test
	void anUnknownRequirementIsRejected() {
		Suite suite = suite("00-a.xml", "01-c.xml 02-d.xml", null);
		suite.getMembers().get(1).getRequires().add("03-x.xml");
		assertMessage("SAIL suite s.xml: <member> 02-d.xml requires 03-x.xml, which is not a member of the suite", suite);
	}

	@Test
	void aMemberRequiringItselfIsRejected() {
		Suite suite = suite("00-a.xml", "01-c.xml", null);
		suite.getSetUps().get(0).getRequires().add("00-a.xml");
		assertMessage("SAIL suite s.xml: <setUp> 00-a.xml requires itself", suite);
	}

	@Test
	void aRestoreRequiringANonSetUpMemberIsRejected() {
		Suite suite = suite("00-a.xml", "01-c.xml", "99-e.xml");
		restore(suite).getRequires().addAll(List.of("00-a.xml", "01-c.xml"));
		assertMessage("SAIL suite s.xml: <restore> 99-e.xml requires 01-c.xml, which is not a set-up member; " +
						"a restore can require only set-up members", suite);
	}

	@Test
	void aCycleOfTwoIsRejectedNamingIt() {
		Suite suite = suite("", "01-c.xml 02-d.xml", null);
		suite.getMembers().get(0).getRequires().add("02-d.xml");
		suite.getMembers().get(1).getRequires().add("01-c.xml");
		assertMessage("SAIL suite s.xml: <member> 02-d.xml is in a cycle of requires: 01-c.xml -> 02-d.xml -> 01-c.xml", suite);
	}

	@Test
	void aLongerCycleAfterAnAcyclicPathIsRejectedNamingOnlyTheCycle() {
		Suite suite = suite("00-a.xml", "01-c.xml 02-d.xml 03-e.xml", null);
		// 00-a.xml is searched, and done, before 01-c.xml leads into the cycle
		suite.getMembers().get(0).getRequires().addAll(List.of("00-a.xml", "02-d.xml"));
		suite.getMembers().get(1).getRequires().add("03-e.xml");
		suite.getMembers().get(2).getRequires().add("02-d.xml");
		assertMessage("SAIL suite s.xml: <member> 03-e.xml is in a cycle of requires: 02-d.xml -> 03-e.xml -> 02-d.xml", suite);
	}

	@Test
	void aRequirementThatRunsLaterIsRejected() {
		Suite member = suite("", "01-c.xml 02-d.xml", null);
		member.getMembers().get(0).getRequires().add("02-d.xml");
		Suite setUp = suite("00-a.xml", "01-c.xml", null);
		setUp.getSetUps().get(0).getRequires().add("01-c.xml");
		Suite restore = suite("", "01-c.xml", "99-e.xml");
		restore.getMembers().get(0).getRequires().add("99-e.xml");
		assertAll(() -> assertMessage("SAIL suite s.xml: <member> 01-c.xml requires 02-d.xml, which runs after it (<member> 02-d.xml); " +
										"members run in order", member),
					() -> assertMessage("SAIL suite s.xml: <setUp> 00-a.xml requires 01-c.xml, which runs after it (<member> 01-c.xml); " +
										"members run in order", setUp),
					() -> assertMessage("SAIL suite s.xml: <member> 01-c.xml requires 99-e.xml, which runs after it (<restore> 99-e.xml); " +
										"members run in order", restore));
	}

	@Test
	void viewRolesWithoutAUserAreRejected() {
		Suite suite = suite("", "01-c.xml", null);
		suite.getViewRoles().add(viewRoles(" ", "admin.BasicUser"));
		assertMessage("SAIL suite s.xml: a <viewRoles> has no user", suite);
	}

	@Test
	void viewRolesForAUserGivenTwiceAreRejected() {
		Suite suite = suite("", "01-c.xml", null);
		suite.getViewRoles().add(viewRoles("jane", "admin.BasicUser"));
		suite.getViewRoles().add(viewRoles("jane", "admin.ContactViewer"));
		assertMessage("SAIL suite s.xml: <viewRoles> for user jane is given more than once", suite);
	}

	@Test
	void viewRolesWithoutARoleAreRejected() {
		Suite suite = suite("", "01-c.xml", null);
		suite.getViewRoles().add(viewRoles("jane"));
		assertMessage("SAIL suite s.xml: <viewRoles> for user jane has no <role>", suite);
	}

	@Test
	void viewRolesWithABlankOrNullRoleAreRejected() {
		Suite blank = suite("", "01-c.xml", null);
		blank.getViewRoles().add(viewRoles("jane", "admin.BasicUser", " "));
		Suite nullRole = suite("", "01-c.xml", null);
		ViewRoles roles = viewRoles("tim");
		roles.getRoles().add(null);
		nullRole.getViewRoles().add(roles);
		assertAll(() -> assertMessage("SAIL suite s.xml: <viewRoles> for user jane has a blank <role>", blank),
					() -> assertMessage("SAIL suite s.xml: <viewRoles> for user tim has a blank <role>", nullRole));
	}

	/**
	 * Builds a suite from space-separated set-up and ordinary member files, and an optional restore file.
	 */
	static Suite suite(String setUps, String members, String restore) {
		Suite result = new Suite();
		for (String file : setUps.split(" ")) {
			if (! file.isEmpty()) {
				result.getSetUps().add(member(new SuiteMember(), file));
			}
		}
		for (String file : members.split(" ")) {
			if (! file.isEmpty()) {
				result.getMembers().add(member(new OrdinarySuiteMember(), file));
			}
		}
		if (restore != null) {
			result.setRestore(member(new SuiteMember(), restore));
		}
		return result;
	}

	private static <T extends SuiteMember> T member(T member, String file) {
		member.setFile(file);
		return member;
	}

	/**
	 * Returns a suite's restore, which the test built.
	 */
	private static SuiteMember restore(Suite suite) {
		SuiteMember result = suite.getRestore();
		assertNotNull(result);
		return result;
	}

	private static ViewRoles viewRoles(String user, String... roles) {
		ViewRoles result = new ViewRoles();
		result.setUser(user);
		result.getRoles().addAll(List.of(roles));
		return result;
	}

	private static List<String> files(List<SuiteMember> members) {
		return members.stream().map(SuiteMember::getFile).toList();
	}

	private static void assertMessage(String expected, Suite suite) {
		MetaDataException e = assertThrows(MetaDataException.class, () -> suite.validate("s.xml"));
		assertEquals(expected, e.getMessage());
	}
}
