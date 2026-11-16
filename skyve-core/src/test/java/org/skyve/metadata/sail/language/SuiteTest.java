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

import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

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
		assertEquals(List.of("01-c.xml"), files(suite.getAllMembers()));
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
	void aSetUpRequiresEveryEarlierSetUp() {
		Suite suite = suite("00-a.xml 00-b.xml 00-c.xml", "01-d.xml", null);
		List<SuiteMember> setUps = suite.getSetUps();
		assertAll(() -> assertTrue(suite.requirementsOf(setUps.get(0)).isEmpty()),
					() -> assertEquals(List.of("00-a.xml"), files(suite.requirementsOf(setUps.get(1)))),
					() -> assertEquals(List.of("00-a.xml", "00-b.xml"), files(suite.requirementsOf(setUps.get(2)))));
	}

	@Test
	void theRestoreRequiresOnlyWhatItNames() {
		Suite suite = suite("00-a.xml 00-b.xml", "01-c.xml", "99-e.xml");
		SuiteMember restore = restore(suite);
		assertTrue(suite.requirementsOf(restore).isEmpty());
		restore.getRequires().add("00-b.xml");
		assertEquals(List.of("00-b.xml"), files(suite.requirementsOf(restore)));
	}

	@Test
	void aMemberNotInTheSuiteRequiresOnlyWhatItNames() {
		Suite suite = suite("00-a.xml", "01-c.xml", null);
		OrdinarySuiteMember stranger = member(new OrdinarySuiteMember(), "02-x.xml");
		stranger.getRequires().add("01-c.xml");
		assertEquals(List.of("01-c.xml"), files(suite.requirementsOf(stranger)));
	}

	@Test
	void lockingIsDecidedByThePlaceInTheSuite() {
		Suite suite = suite("00-a.xml", "01-c.xml 02-d.xml 03-e.xml", "99-e.xml");
		List<OrdinarySuiteMember> members = suite.getMembers();
		members.get(1).setLockTestStrategy(Boolean.FALSE);
		members.get(2).setLockTestStrategy(Boolean.TRUE);
		// an ordinary member put in the set-up list is locked as a set-up
		OrdinarySuiteMember unlockedSetUp = member(new OrdinarySuiteMember(), "00-z.xml");
		unlockedSetUp.setLockTestStrategy(Boolean.FALSE);
		suite.getSetUps().add(unlockedSetUp);
		SuiteMember restore = restore(suite);
		assertAll(() -> assertTrue(suite.isTestStrategyLocked(suite.getSetUps().get(0))),
					() -> assertTrue(suite.isTestStrategyLocked(unlockedSetUp)),
					() -> assertTrue(suite.isTestStrategyLocked(restore)),
					() -> assertNull(members.get(0).getLockTestStrategy()),
					() -> assertFalse(suite.isTestStrategyLocked(members.get(0))),
					() -> assertFalse(suite.isTestStrategyLocked(members.get(1))),
					() -> assertTrue(suite.isTestStrategyLocked(members.get(2))),
					() -> assertFalse(suite.isTestStrategyLocked(member(new SuiteMember(), "elsewhere.xml"))));
	}

	@Test
	void theAutomationsStrategyWinsThenTheWorkflowsThenTheSuitesForAnUnlockedMember() {
		Suite suite = suite("", "01-c.xml", null);
		OrdinarySuiteMember member = suite.getMembers().get(0);
		Workflow workflow = new Workflow();
		Automation automation = new Automation();
		assertNull(suite.getEffectiveTestStrategy(member, workflow, automation));
		suite.setTestStrategy(TestStrategy.None);
		assertEquals(TestStrategy.None, suite.getEffectiveTestStrategy(member, workflow, automation));
		workflow.setTestStrategy(TestStrategy.Verify);
		assertEquals(TestStrategy.Verify, suite.getEffectiveTestStrategy(member, workflow, automation));
		automation.setTestStrategy(TestStrategy.Assert);
		assertEquals(TestStrategy.Assert, suite.getEffectiveTestStrategy(member, workflow, automation));
	}

	@Test
	void aLockedMemberIgnoresTheSuitesStrategy() {
		Suite suite = suite("00-a.xml", "01-c.xml", "99-e.xml");
		suite.setTestStrategy(TestStrategy.None);
		OrdinarySuiteMember locked = suite.getMembers().get(0);
		locked.setLockTestStrategy(Boolean.TRUE);
		SuiteMember setUp = suite.getSetUps().get(0);
		SuiteMember restore = restore(suite);
		Workflow workflow = new Workflow();
		Automation automation = new Automation();
		assertAll(() -> assertNull(suite.getEffectiveTestStrategy(setUp, workflow, automation)),
					() -> assertNull(suite.getEffectiveTestStrategy(locked, workflow, automation)),
					() -> assertNull(suite.getEffectiveTestStrategy(restore, workflow, automation)));
		workflow.setTestStrategy(TestStrategy.Verify);
		assertEquals(TestStrategy.Verify, suite.getEffectiveTestStrategy(setUp, workflow, automation));
		automation.setTestStrategy(TestStrategy.Assert);
		assertEquals(TestStrategy.Assert, suite.getEffectiveTestStrategy(locked, workflow, automation));
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
	void evidenceNamesAreTheXmlValues() {
		assertEquals(Set.of("none", "failures", "snapshots", "video", "all"),
						Stream.of(Suite.Evidence.values()).map(Enum::name).collect(Collectors.toSet()));
	}

	@Test
	void aValidSuitePasses() {
		Suite suite = suite("00-a.xml 00-b.xml", "pf/01-session.xml sc/01-session.xml sc/04-devops.xml", "99-e.xml");
		suite.getSetUps().get(1).getRequires().add("00-a.xml");
		suite.getMembers().get(2).getRequires().addAll(List.of("pf/01-session.xml", "00-a.xml"));
		restore(suite).getRequires().addAll(List.of("00-a.xml", "00-b.xml"));
		suite.getViewRoles().add(viewRoles("jane{run}", "admin.BasicUser"));
		suite.getViewRoles().add(viewRoles("tim", "admin.BasicUser", "admin.ContactViewer"));
		assertDoesNotThrow(() -> suite.validate("s.xml"));
	}

	@Test
	void aSuiteWithoutSetUpsOrRestorePasses() {
		Suite suite = suite("", "01-c.xml", null);
		assertDoesNotThrow(() -> suite.validate("s.xml"));
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
					() -> assertMessage("SAIL suite s.xml: 00-a.xml is listed more than once", suite("00-a.xml", "01-c.xml", "00-a.xml")),
					() -> assertMessage("SAIL suite s.xml: pf/01-c.xml is listed more than once",
											suite("", "pf/01-c.xml sc/01-c.xml pf/01-c.xml", null)));
	}

	@Test
	void theSameFileNameInTwoFoldersIsTwoMembers() {
		Suite suite = suite("", "pf/01-session.xml sc/01-session.xml sc/03-security-admin.xml", null);
		suite.getMembers().get(2).getRequires().add("pf/01-session.xml");
		assertAll(() -> assertDoesNotThrow(() -> suite.validate("s.xml")),
					() -> assertEquals(List.of("pf/01-session.xml"), files(suite.requirementsOf(suite.getMembers().get(2)))));
	}

	@Test
	void aFileThatIsNotANormalisedRelativePathIsRejected() {
		String normalised = "has an empty, . or .. segment; it must be a normalised path within the suite's folder";
		String absolute = "is absolute; it must be relative to the suite's folder";
		assertAll(() -> assertFileRejected("/sail/01-c.xml", absolute),
					() -> assertFileRejected("C:/sail/01-c.xml", absolute),
					() -> assertFileRejected("c:01-c.xml", absolute),
					() -> assertFileRejected("pf\\01-c.xml", "has a \\; separate folders with /"),
					() -> assertFileRejected("\\\\host\\01-c.xml", "has a \\; separate folders with /"),
					() -> assertFileRejected("pf//01-c.xml", normalised),
					() -> assertFileRejected("pf/", normalised),
					() -> assertFileRejected("./01-c.xml", normalised),
					() -> assertFileRejected("pf/./01-c.xml", normalised),
					() -> assertFileRejected("../01-c.xml", normalised),
					() -> assertFileRejected("pf/../sc/01-c.xml", normalised));
	}

	@Test
	void aSetUpOrRestoreFileIsCheckedToo() {
		Suite setUp = suite("../00-a.xml", "01-c.xml", null);
		Suite restore = suite("", "01-c.xml", "/99-e.xml");
		assertAll(() -> assertMessage("SAIL suite s.xml: <setUp> file ../00-a.xml has an empty, . or .. segment; " +
										"it must be a normalised path within the suite's folder", setUp),
					() -> assertMessage("SAIL suite s.xml: <restore> file /99-e.xml is absolute; it must be relative to the suite's folder",
										restore));
	}

	@Test
	void namesWithDotsThatAreNotSegmentsArePaths() {
		Suite suite = suite("", "..a.xml pf/.hidden.xml a..b/c.xml", null);
		assertDoesNotThrow(() -> suite.validate("s.xml"));
	}

	@Test
	void anUnknownRequirementIsRejected() {
		Suite suite = suite("00-a.xml", "01-c.xml 02-d.xml", null);
		suite.getMembers().get(1).getRequires().add("03-x.xml");
		assertMessage("SAIL suite s.xml: <member> 02-d.xml requires 03-x.xml, which is not a member of the suite", suite);
	}

	@Test
	void aRequirementNamedTwiceIsRejected() {
		Suite suite = suite("00-a.xml", "pf/01-c.xml sc/02-d.xml", null);
		suite.getMembers().get(1).getRequires().addAll(List.of("pf/01-c.xml", "00-a.xml", "pf/01-c.xml"));
		assertMessage("SAIL suite s.xml: <member> sc/02-d.xml requires pf/01-c.xml more than once", suite);
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
	void aCycleIsRejectedByTheRunOrder() {
		Suite suite = suite("", "01-c.xml 02-d.xml", null);
		suite.getMembers().get(0).getRequires().add("02-d.xml");
		suite.getMembers().get(1).getRequires().add("01-c.xml");
		assertMessage("SAIL suite s.xml: <member> 01-c.xml requires 02-d.xml, which runs after it (<member> 02-d.xml); " +
						"members run in order", suite);
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
	void viewRolesWithAnEmptyOrNullRoleAreRejected() {
		Suite empty = suite("", "01-c.xml", null);
		empty.getViewRoles().add(viewRoles("jane", "admin.BasicUser", ""));
		Suite nullRole = suite("", "01-c.xml", null);
		ViewRoles roles = viewRoles("tim");
		roles.getRoles().add(null);
		nullRole.getViewRoles().add(roles);
		assertAll(() -> assertMessage("SAIL suite s.xml: <viewRoles> for user jane has an empty <role>", empty),
					() -> assertMessage("SAIL suite s.xml: <viewRoles> for user tim has an empty <role>", nullRole));
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

	private static void assertFileRejected(String file, String problem) {
		assertMessage("SAIL suite s.xml: <member> file " + file + " " + problem, suite("", file, null));
	}

	private static void assertMessage(String expected, Suite suite) {
		MetaDataException e = assertThrows(MetaDataException.class, () -> suite.validate("s.xml"));
		assertEquals(expected, e.getMessage());
	}
}
