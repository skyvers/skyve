package util.sail;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.RETURNS_DEEP_STUBS;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.openqa.selenium.WebDriver;
import org.skyve.impl.sail.execution.RunState;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.sail.language.Suite;
import org.skyve.metadata.sail.language.SuiteMember;
import org.skyve.metadata.sail.language.Workflow;

/**
 * Tests the browser's lifecycle in the workflow engine, without a browser: the browsers are mocks, and each leg only
 * records that it ran. A suite member that passes keeps its browser for the next; one that fails stops it; a workflow
 * on its own stops its browser when it ends; a browser that fails to start is never reused.
 */
@SuppressWarnings("java:S5960") // test-only assertions
class SailWorkflowTest {
	private static final String AUTOMATION = "<automation uxui=\"tablet\" userAgentType=\"tablet\" xmlns=\"http://www.skyve.org/xml/sail\">" +
												"<interaction name=\"i\"><method><comment>a step</comment></method></interaction></automation>";
	private static final String SMART_CLIENT_AUTOMATION = AUTOMATION.replace("uxui=\"tablet\" userAgentType=\"tablet\"",
																				"uxui=\"desktop\" userAgentType=\"desktop\"");
	private static final Suite SUITE = XMLMetaData.unmarshalSAILSuiteString("<suite xmlns=\"http://www.skyve.org/xml/sail\">" +
																				"<member file=\"a.xml\"/><member file=\"b.xml\"/><member file=\"c.xml\"/>" +
																				"</suite>", "s.suite.xml");

	/** What happened, in order: "start 1", "run 1 a.xml", "stop 1". */
	private final List<String> events = new ArrayList<>();
	private final Map<Selenide<?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?>, Integer> ids = new IdentityHashMap<>();
	/** The members whose leg fails. */
	private final Set<String> failing = new HashSet<>();
	/** True while starting a browser fails. */
	private boolean startFails;
	/** The run each leg ran in, in order. */
	private final List<RunState> runs = new ArrayList<>();
	/** The member running. */
	private String current;

	private final SailWorkflow engine = new SailWorkflow(new SailViewRoles(),
															(smartClient, configuration, resolved) -> start(),
															(automation, browser, run, settings) -> {
																String which = ids.containsKey(browser) ? String.valueOf(ids.get(browser)) : browser.getClass().getSimpleName();
																events.add("run " + which + ' ' + current);
																runs.add(run);
																if (failing.contains(current)) {
																	throw new AssertionError(current + " failed its step");
																}
															},
															new SailSettings.Environment(Map.of(SailSettings.BASE_URL, "http://localhost:1/sail-workflow-test/"),
																							Map.of(), null, Path.of("").toAbsolutePath()));
	private Selenide<?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?> start() {
		if (startFails) {
			events.add("start fails");
			throw new IllegalStateException("the browser did not start");
		}
		PrimeFacesSelenide result = mock(PrimeFacesSelenide.class);
		Integer id = Integer.valueOf(ids.size() + 1);
		ids.put(result, id);
		doAnswer(i -> {
			events.add("stop " + id);
			return null;
		}).when(result).stopBrowser();
		events.add("start " + id);
		return result;
	}

	private void member(String file) {
		member(file, AUTOMATION);
	}

	private void member(String file, String automation) {
		current = file;
		Workflow workflow = XMLMetaData.unmarshalSAILWorkflowString(automation);
		SuiteMember member = SUITE.getMember(file);
		assertNotNull(member, file);
		engine.runMember(file, workflow, new SailWorkflow.Member(SUITE, "s.suite.xml", member, null));
	}

	@AfterEach
	@SuppressWarnings("static-method")
	void releaseServer() {
		SailServerLock.releaseAll();
	}

	@Test
	void aPassedMemberKeepsItsBrowserForTheNextUntilTheSuiteCloses() {
		member("a.xml");
		member("b.xml");
		engine.closeBrowser();
		engine.closeBrowser(); // nothing is left to stop
		assertEquals(List.of("start 1", "run 1 a.xml", "run 1 b.xml", "stop 1"), events);
	}

	@Test
	void aFailedMembersBrowserIsStoppedAndTheNextStartsItsOwn() {
		failing.add("a.xml");
		assertThrows(AssertionError.class, () -> member("a.xml"));
		member("b.xml");
		engine.closeBrowser();
		assertEquals(List.of("start 1", "run 1 a.xml", "stop 1", "start 2", "run 2 b.xml", "stop 2"), events);
	}

	@Test
	void aWorkflowOnItsOwnStopsItsBrowserWhenItEnds() {
		current = "w.xml";
		engine.runWorkflow("w.xml", XMLMetaData.unmarshalSAILWorkflowString(AUTOMATION));
		engine.closeBrowser();
		assertEquals(List.of("start 1", "run 1 w.xml", "stop 1"), events);
	}

	/**
	 * When the next member needs another browser and it fails to start, the old one has been stopped and nothing is
	 * kept: the member after that starts a browser of its own.
	 */
	@Test
	void aBrowserThatFailsToStartIsNeverReused() {
		member("a.xml");
		Selenide<?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?> first = ids.keySet().iterator().next();
		when(first.browserChange(any(), any())).thenReturn("headless true -> false");
		startFails = true;
		assertThrows(IllegalStateException.class, () -> member("b.xml"));
		startFails = false;
		member("c.xml");
		engine.closeBrowser();
		assertEquals(List.of("start 1", "run 1 a.xml", "stop 1", "start fails", "start 2", "run 2 c.xml", "stop 2"), events);
	}

	/** When carrying on with the kept browser fails (a new timeout cannot be applied), that browser is stopped. */
	@Test
	void aKeptBrowserThatCannotTakeTheNextMembersSettingsIsStopped() {
		member("a.xml");
		Selenide<?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?> first = ids.keySet().iterator().next();
		doThrow(new IllegalStateException("the browser has gone")).when(first).useSettings(any(), any());
		assertThrows(IllegalStateException.class, () -> member("b.xml"));
		engine.closeBrowser();
		assertEquals(List.of("start 1", "run 1 a.xml", "stop 1"), events);
	}

	/** Each call is a run of its own, so the run token differs between calls, unless the runner shares its run. */
	@Test
	void callsShareARunOnlyWhenTheRunnerIsToldTo() {
		Workflow workflow = XMLMetaData.unmarshalSAILWorkflowString(AUTOMATION);
		current = "w.xml";
		engine.runWorkflow("w.xml", workflow);
		engine.runWorkflow("w.xml", workflow);
		RunState first = runs.get(0);
		RunState second = runs.get(1);
		assertNotSame(first, second, "without sharing, each call has its own run");
		engine.shareRunAcrossCalls();
		engine.runWorkflow("w.xml", workflow);
		engine.runWorkflow("w.xml", workflow);
		member("a.xml");
		assertAll(() -> assertEquals(5, runs.size()),
					() -> assertSame(runs.get(2), runs.get(3), "a shared run is the same in the next call"),
					() -> assertSame(runs.get(2), runs.get(4), "and in a suite member"),
					() -> assertNotSame(first, runs.get(2), "it is not one of the earlier runs"));
		engine.closeBrowser();
	}

	/** A SmartClient member drives the PrimeFaces member's browser through SmartClient's Selenide, and stops it at the end. */
	@Test
	void aSmartClientMemberDrivesTheBrowserThatAPrimeFacesMemberLeft() {
		member("a.xml");
		Selenide<?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?> first = ids.keySet().iterator().next();
		WebDriver driver = mock(WebDriver.class, RETURNS_DEEP_STUBS);
		first.driver = driver;
		member("b.xml", SMART_CLIENT_AUTOMATION);
		engine.closeBrowser();
		assertEquals(List.of("start 1", "run 1 a.xml", "run SmartClientSelenide b.xml"), events);
		verify(driver).quit();
	}

	/** A workflow run after a suite member on the same engine carries on in the kept browser, then stops it. */
	@Test
	void aWorkflowAfterASuiteMemberUsesItsBrowserThenStopsIt() {
		member("a.xml");
		current = "w.xml";
		engine.runWorkflow("w.xml", XMLMetaData.unmarshalSAILWorkflowString(AUTOMATION));
		engine.closeBrowser();
		assertEquals(List.of("start 1", "run 1 a.xml", "run 1 w.xml", "stop 1"), events);
	}

	/** A load run that stops during a workflow: no further leg starts, so no new browser either. */
	@Test
	void aStoppingEngineStartsNoFurtherLeg() {
		current = "w.xml";
		engine.stopWhen(() -> ! events.isEmpty() && events.get(events.size() - 1).startsWith("run")); // stops after a leg ran
		Workflow twoLegs = XMLMetaData.unmarshalSAILWorkflowString("<workflow xmlns=\"http://www.skyve.org/xml/sail\">" +
																		AUTOMATION.replace(" xmlns=\"http://www.skyve.org/xml/sail\"", "").repeat(2) +
																		"</workflow>");

		IllegalStateException e = assertThrows(IllegalStateException.class, () -> engine.runWorkflow("w.xml", twoLegs));

		assertEquals("SAIL workflow: w.xml automation 2 of 2 did not start: the run is stopping", e.getMessage());
		assertEquals(List.of("start 1", "run 1 w.xml", "stop 1"), events);
	}

	/** A load run that stops during a suite: its remaining members are skipped, and start no browser. */
	@Test
	void aStoppingEngineSkipsASuitesRemainingMembers() {
		engine.stopWhen(() -> events.stream().anyMatch(event -> event.startsWith("run")));
		SailSuite suite = SailSuite.resource("sail/kitchenSink/kitchensink.suite.xml", engine)
									.environment(new SailSettings.Environment(Map.of(), Map.of(), null, Path.of("").toAbsolutePath()));

		SailSuite.Summary summary = suite.execute();

		assertEquals(List.of("start 1", "run 1 null", "stop 1"), events, "one member ran");
		assertEquals(4, summary.members(SailSuite.Outcome.SKIPPED).size(), summary.describe("kitchensink"));
		assertEquals(List.of("the run is stopping"), summary.skips().values().stream().distinct().toList());
	}
}
