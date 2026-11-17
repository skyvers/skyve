package util.sail;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.junit.jupiter.api.Test;
import org.opentest4j.MultipleFailuresError;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.sail.language.Automation;
import org.skyve.metadata.sail.language.Automation.TestStrategy;
import org.skyve.metadata.sail.language.Suite;
import org.skyve.metadata.sail.language.Suite.Evidence;
import org.skyve.metadata.sail.language.Workflow;

import util.sail.SailSuite.Outcome;
import util.sail.SailSuite.Summary;

/**
 * Tests the suite engine without a browser: each member is run by a recording runner that fails the members a test
 * names. Settings resolve from given sources only.
 */
@SuppressWarnings({"static-method", "java:S5960"}) // test-only assertions
class SailSuiteTest {
	/** A suite like the admin suite: a bracket, renderer folders, a genuine requirement, a locked member. */
	private static final String SUITE = """
			<suite xmlns="http://www.skyve.org/xml/sail" testStrategy="Verify" evidence="failures">
				<viewRoles user="sail{run}">
					<role>admin.BasicUser</role>
				</viewRoles>
				<setUp file="00-precondition.xml"/>
				<setUp file="00-backup.xml"/>
				<member file="pf/01-session.xml"/>
				<member file="pf/02-users.xml"/>
				<member file="pf/03-uses-users.xml" requires="pf/02-users.xml"/>
				<member file="pf/04-after-03.xml" requires="pf/03-uses-users.xml"/>
				<member file="sc/01-session.xml" lockTestStrategy="true"/>
				<member file="sc/03-security.xml"/>
				<restore file="99-restore.xml" requires="00-precondition.xml 00-backup.xml"/>
			</suite>
			""";

	private final List<String> ran = new ArrayList<>();
	private final List<String> locks = new ArrayList<>();

	/** Returns where a test strategy came from, or "default". */
	private static String strategySource(SailSettings settings) {
		SailSettings.Setting setting = settings.setting(SailSettings.TEST_STRATEGY);
		return (setting == null) ? "default" : setting.source().toString();
	}

	private Summary run(Map<String, String> systemProperties, String... failing) {
		Suite suite = XMLMetaData.unmarshalSAILSuiteString(SUITE, "s.suite.xml");
		Set<String> fail = Set.of(failing);
		return SailSuite.of(suite, "s.suite.xml", member -> {
			ran.add(member.path());
			locks.add(member.member().getFile() + '=' + member.lock());
			if (fail.contains(member.path())) {
				throw new AssertionError(member.path() + " failed its step");
			}
		})
		.environment(new SailSettings.Environment(systemProperties, Map.of(), null, Path.of("").toAbsolutePath()))
		.execute();
	}

	@Test
	void everyMemberRunsInOrderWhenAllPass() {
		Summary summary = run(Map.of());
		assertAll(() -> assertEquals(List.of("00-precondition.xml", "00-backup.xml", "pf/01-session.xml", "pf/02-users.xml",
												"pf/03-uses-users.xml", "pf/04-after-03.xml", "sc/01-session.xml",
												"sc/03-security.xml", "99-restore.xml"), ran),
					() -> assertEquals(9, summary.members(Outcome.PASSED).size()),
					() -> assertTrue(summary.describe("s").startsWith("SAIL suite s: 9 passed, 0 failed, 0 skipped\n"), summary.describe("s")));
	}

	/** A failed ordinary member does not stop the others; those that require it, directly or not, are skipped. */
	@Test
	void aFailedMemberSkipsOnlyTheMembersThatRequireIt() {
		Summary summary = run(Map.of(), "pf/02-users.xml");
		assertAll(() -> assertEquals(List.of("00-precondition.xml", "00-backup.xml", "pf/01-session.xml", "pf/02-users.xml",
												"sc/01-session.xml", "sc/03-security.xml", "99-restore.xml"), ran),
					() -> assertEquals(List.of("pf/02-users.xml"), summary.members(Outcome.FAILED)),
					() -> assertEquals(List.of("pf/03-uses-users.xml", "pf/04-after-03.xml"), summary.members(Outcome.SKIPPED)),
					() -> assertEquals("requires pf/02-users.xml, which failed", summary.skips().get("pf/03-uses-users.xml")),
					() -> assertEquals("requires pf/03-uses-users.xml, which was skipped", summary.skips().get("pf/04-after-03.xml")),
					() -> assertTrue(summary.describe("s").contains("\n  skipped: pf/04-after-03.xml: requires pf/03-uses-users.xml, which was skipped"),
										summary.describe("s")));
	}

	/**
	 * A failed set-up skips the later set-ups and every ordinary member, and the restore that requires it: there is
	 * nothing of this run's to restore.
	 */
	@Test
	void aFailedSetUpSkipsTheRestOfTheBracketAndTheRestore() {
		Summary summary = run(Map.of(), "00-precondition.xml");
		assertAll(() -> assertEquals(List.of("00-precondition.xml"), ran),
					() -> assertEquals("requires 00-precondition.xml, which failed", summary.skips().get("00-backup.xml")),
					() -> assertEquals("requires 00-precondition.xml, which failed", summary.skips().get("pf/01-session.xml")),
					() -> assertEquals("requires 00-precondition.xml, which failed", summary.skips().get("99-restore.xml")));
	}

	/** The restore runs after failed ordinary members, as long as the bracket passed. */
	@Test
	void theRestoreRunsAfterFailedMembers() {
		Summary summary = run(Map.of(), "pf/01-session.xml", "sc/03-security.xml");
		assertAll(() -> assertEquals("99-restore.xml", ran.get(ran.size() - 1)),
					() -> assertEquals(Outcome.PASSED, summary.outcomes().get("99-restore.xml")),
					() -> assertEquals(List.of("pf/01-session.xml", "sc/03-security.xml"), summary.members(Outcome.FAILED)));
	}

	/** run() fails with every failed member, not only the first. */
	@Test
	void theTestFailsWithEveryFailedMember() {
		Suite suite = XMLMetaData.unmarshalSAILSuiteString(SUITE, "s.suite.xml");
		SailSuite sailSuite = SailSuite.of(suite, "s.suite.xml", member -> {
			if (member.path().endsWith("01-session.xml")) {
				throw new IllegalArgumentException("not a valid script");
			}
		}).environment(new SailSettings.Environment(Map.of(), Map.of(), null, Path.of("").toAbsolutePath()));
		MultipleFailuresError e = assertThrows(MultipleFailuresError.class, sailSuite::run);
		assertAll(() -> assertEquals(2, e.getFailures().size()),
					() -> assertEquals("SAIL suite s.suite.xml: pf/01-session.xml failed: not a valid script", e.getFailures().get(0).getMessage()),
					() -> assertEquals("SAIL suite s.suite.xml: sc/01-session.xml failed: not a valid script", e.getFailures().get(1).getMessage()),
					() -> assertTrue(e.getMessage().startsWith("SAIL suite s.suite.xml: 7 passed, 2 failed, 0 skipped"), e.getMessage()));
	}

	/**
	 * A choice by path prefix runs those members and the members they require, transitively; the bracket always runs,
	 * and the rest are reported as not chosen.
	 */
	@Test
	void onlyChoosesByPathAndPullsInRequirements() {
		Summary summary = run(Map.of(SailSettings.SUITE_ONLY, "pf/04"));
		assertAll(() -> assertEquals(List.of("00-precondition.xml", "00-backup.xml", "pf/02-users.xml", "pf/03-uses-users.xml",
												"pf/04-after-03.xml", "99-restore.xml"), ran),
					() -> assertEquals(List.of("pf/01-session.xml", "sc/01-session.xml", "sc/03-security.xml"), summary.members(Outcome.NOT_SELECTED)),
					() -> assertEquals(List.of("only [pf/04] chooses [pf/04-after-03.xml], with the members they require: " +
												"[pf/03-uses-users.xml (required by pf/04-after-03.xml), pf/02-users.xml (required by pf/03-uses-users.xml)]"),
										summary.notes()));
	}

	/** A prefix of a member's name chooses it in any folder; a renderer folder chooses all of its members. */
	@Test
	void onlyChoosesByNameOrFolder() {
		run(Map.of(SailSettings.SUITE_ONLY, "01"));
		List<String> byName = new ArrayList<>(ran);
		ran.clear();
		run(Map.of(SailSettings.SUITE_ONLY, "sc/"));
		assertAll(() -> assertEquals(List.of("00-precondition.xml", "00-backup.xml", "pf/01-session.xml", "sc/01-session.xml", "99-restore.xml"), byName),
					() -> assertEquals(List.of("00-precondition.xml", "00-backup.xml", "sc/01-session.xml", "sc/03-security.xml", "99-restore.xml"), ran));
	}

	@Test
	void aChoiceOfNoMemberFails() {
		Map<String, String> only = Map.of(SailSettings.SUITE_ONLY, "07");
		IllegalArgumentException e = assertThrows(IllegalArgumentException.class, () -> run(only));
		assertEquals("suite.only=07 (-Dsail.suite.only) chooses no member of SAIL suite s.suite.xml: its members are " +
						"[pf/01-session.xml, pf/02-users.xml, pf/03-uses-users.xml, pf/04-after-03.xml, sc/01-session.xml, sc/03-security.xml]",
						e.getMessage());
	}

	/**
	 * A run's test strategy override is not applied to the locked members (the set-ups, the restore and a member with
	 * lockTestStrategy), and the summary says so for each, naming the override's source.
	 */
	@Test
	void lockedMembersIgnoreAnOverrideAndSaySo() {
		Summary summary = run(Map.of(SailSettings.TEST_STRATEGY, "None"));
		assertAll(() -> assertEquals(List.of("override None from -Dsail.testStrategy not applied to 00-precondition.xml: locked (set-up)",
												"override None from -Dsail.testStrategy not applied to 00-backup.xml: locked (set-up)",
												"override None from -Dsail.testStrategy not applied to sc/01-session.xml: locked (lockTestStrategy)",
												"override None from -Dsail.testStrategy not applied to 99-restore.xml: locked (restore)"),
										summary.notes()),
					() -> assertEquals(List.of("00-precondition.xml=set-up", "00-backup.xml=set-up", "pf/01-session.xml=null",
												"pf/02-users.xml=null", "pf/03-uses-users.xml=null", "pf/04-after-03.xml=null",
												"sc/01-session.xml=lockTestStrategy", "sc/03-security.xml=null", "99-restore.xml=restore"),
										locks));
	}

	/**
	 * A leg's settings: an unlocked member takes the run's override; without one, the suite's default; a locked member
	 * keeps its own strategy (else Assert) and notes the override; the suite's evidence request applies to all.
	 */
	@Test
	void aLegsSettingsTakeTheSuiteDefaultTheOverrideAndTheLock() {
		Suite suite = XMLMetaData.unmarshalSAILSuiteString(SUITE, "s.suite.xml");
		Workflow workflow = new Workflow();
		Automation automation = new Automation();
		workflow.getAutomations().add(automation);
		SailSettings.Environment override = new SailSettings.Environment(Map.of(SailSettings.TEST_STRATEGY, "None"), Map.of(), null,
																		Path.of("").toAbsolutePath());
		SailSettings.Environment none = new SailSettings.Environment(Map.of(), Map.of(), null, Path.of("").toAbsolutePath());
		SailWorkflow.Member ordinary = new SailWorkflow.Member(suite, "s.suite.xml", suite.getMembers().get(0));
		SailWorkflow.Member locked = new SailWorkflow.Member(suite, "s.suite.xml", suite.getSetUps().get(0));

		SailSettings suiteDefault = SailWorkflow.legSettings(SailSettings.resolve(none, List.of()), "pf/01-session.xml", workflow, automation, ordinary);
		SailSettings overridden = SailWorkflow.legSettings(SailSettings.resolve(override, List.of()), "pf/01-session.xml", workflow, automation, ordinary);
		SailSettings kept = SailWorkflow.legSettings(SailSettings.resolve(override, List.of()), "00-precondition.xml", workflow, automation, locked);
		workflow.setTestStrategy(TestStrategy.Verify);
		SailSettings workflowDefault = SailWorkflow.legSettings(SailSettings.resolve(override, List.of()), "00-precondition.xml", workflow, automation, locked)
											.with(SailSettings.automation(null, null));
		assertAll(() -> assertEquals(TestStrategy.Verify, suiteDefault.testStrategy()),
					() -> assertEquals("suite s.suite.xml", strategySource(suiteDefault)),
					() -> assertEquals(Evidence.failures, suiteDefault.evidence()),
					() -> assertEquals(TestStrategy.None, overridden.testStrategy()),
					() -> assertEquals(TestStrategy.Assert, kept.testStrategy()),
					() -> assertEquals("suite s.suite.xml (locked: set-up)", strategySource(kept)),
					() -> assertEquals("testStrategy override None from -Dsail.testStrategy not applied: locked (set-up)",
										kept.describe().get(kept.describe().size() - 1)),
					() -> assertEquals(TestStrategy.Verify, workflowDefault.testStrategy(), "the lock survives with(): the workflow's own default"),
					() -> assertNull(SailWorkflow.lockOf(suite, suite.getMembers().get(0))));
	}
}
