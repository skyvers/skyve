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
import org.skyve.web.UserAgentType;

/**
 * Tests the browser's lifecycle in the workflow engine, without a browser: the browsers are mocks, and each leg only
 * records that it ran. A suite member, passed or failed, and a workflow on its own stop their browser when they end, so
 * the next starts its own; a browser that fails to start is never reused.
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
	/** True to start fakes that remember their configuration, as a started browser does, instead of mocks. */
	private boolean realSelenides;
	/** What a leg's step does to its browser, e.g. restarts it; or null. */
	private java.util.function.Consumer<Selenide<?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?>> step;
	/** The member running. */
	private String current;

	private final SailWorkflow engine = new SailWorkflow(new SailViewRoles(),
															(smartClient, configuration, resolved) -> start(configuration, resolved),
															(automation, browser, run, settings) -> {
																String which = ids.containsKey(browser) ? String.valueOf(ids.get(browser)) : browser.getClass().getSimpleName();
																events.add("run " + which + ' ' + current);
																runs.add(run);
																if (step != null) {
																	step.accept(browser);
																}
																if (failing.contains(current)) {
																	throw new AssertionError(current + " failed its step");
																}
															},
															new SailSettings.Environment(Map.of(SailSettings.BASE_URL, "http://localhost:1/sail-workflow-test/"),
																							Map.of(), null, Path.of("").toAbsolutePath()));
	private Selenide<?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?> start(BrowserConfiguration configuration, SailSettings resolved) {
		if (startFails) {
			events.add("start fails");
			throw new IllegalStateException("the browser did not start");
		}
		Integer id = Integer.valueOf(ids.size() + 1);
		if (realSelenides) {
			PrimeFacesSelenide fake = new PrimeFacesSelenide() {
				@Override
				public void startBrowser(BrowserConfiguration c, SailSettings r) {
					recordStart(c, r); // what a started browser remembers, without starting one
					events.add("start " + id);
				}

				@Override
				public void stopBrowser() {
					events.add("stop " + id);
				}
			};
			ids.put(fake, id);
			fake.startBrowser(configuration, resolved);
			return fake;
		}
		PrimeFacesSelenide result = mock(PrimeFacesSelenide.class);
		ids.put(result, id);
		doAnswer(i -> {
			events.add("stop " + id);
			return null;
		}).when(result).stopBrowser();
		events.add("start " + id);
		return result;
	}

	/** A workflow of the automations, which are written as files with their own root element. */
	private static Workflow workflow(String... automations) {
		StringBuilder xml = new StringBuilder("<workflow xmlns=\"http://www.skyve.org/xml/sail\">");
		for (String automation : automations) {
			xml.append(automation.replace(" xmlns=\"http://www.skyve.org/xml/sail\"", ""));
		}
		return XMLMetaData.unmarshalSAILWorkflowString(xml.append("</workflow>").toString());
	}

	private void member(String file) {
		current = file;
		Workflow workflow = XMLMetaData.unmarshalSAILWorkflowString(AUTOMATION);
		SuiteMember member = SUITE.getMember(file);
		assertNotNull(member, file);
		engine.runMember(file, workflow, new SailWorkflow.Member(SUITE, "s.suite.xml", member, null));
	}

	@AfterEach
	@SuppressWarnings("static-method")
	void releaseServer() {
		SailServerLock.releaseAll();
	}

	/** Members are independent runs: what a browser holds (cookies, storage, service workers) is not handed on. */
	@Test
	void aPassedMemberStopsItsBrowserSoTheNextMemberStartsItsOwn() {
		member("a.xml");
		member("b.xml");
		assertEquals(List.of("start 1", "run 1 a.xml", "stop 1", "start 2", "run 2 b.xml", "stop 2"), events);
	}

	@Test
	void aFailedMembersBrowserIsStoppedAndTheNextStartsItsOwn() {
		failing.add("a.xml");
		assertThrows(AssertionError.class, () -> member("a.xml"));
		member("b.xml");
		assertEquals(List.of("start 1", "run 1 a.xml", "stop 1", "start 2", "run 2 b.xml", "stop 2"), events);
	}

	@Test
	void aWorkflowOnItsOwnStopsItsBrowserWhenItEnds() {
		current = "w.xml";
		engine.runWorkflow("w.xml", XMLMetaData.unmarshalSAILWorkflowString(AUTOMATION));
		assertEquals(List.of("start 1", "run 1 w.xml", "stop 1"), events);
	}

	/**
	 * When the next leg needs another browser and it fails to start, the old one has been stopped: the run after that
	 * starts a browser of its own.
	 */
	@Test
	void aBrowserThatFailsToStartIsNeverReused() {
		current = "w.xml";
		step = browser -> {
			when(browser.browserChange(any(), any())).thenReturn("headless true -> false");
			startFails = true;
		};
		assertThrows(IllegalStateException.class, () -> engine.runWorkflow("w.xml", workflow(AUTOMATION, AUTOMATION)));
		startFails = false;
		step = null;
		engine.runWorkflow("w.xml", XMLMetaData.unmarshalSAILWorkflowString(AUTOMATION));
		assertEquals(List.of("start 1", "run 1 w.xml", "stop 1", "start fails", "start 2", "run 2 w.xml", "stop 2"), events);
	}

	/** When carrying on with the browser fails (a new timeout cannot be applied), that browser is stopped. */
	@Test
	void aBrowserThatCannotTakeTheNextLegsSettingsIsStopped() {
		current = "w.xml";
		step = browser -> {
			doThrow(new IllegalStateException("the browser has gone")).when(browser).useSettings(any(), any());
			step = null;
		};
		assertThrows(IllegalStateException.class, () -> engine.runWorkflow("w.xml", workflow(AUTOMATION, AUTOMATION)));
		assertEquals(List.of("start 1", "run 1 w.xml", "stop 1"), events);
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
	}

	/**
	 * A step that restarts the leg's browser on another device leaves the Selenide with that device: the next leg that
	 * asks for it reuses the browser, and one that asks for the first device stops it and starts another.
	 */
	@Test
	void aLegAfterAStepThatRestartedTheBrowserComparesWithTheRestartedDevice() {
		realSelenides = true;
		BrowserConfiguration a = new BrowserConfiguration().profile(Path.of("target", "a"));
		BrowserConfiguration b = new BrowserConfiguration().profile(Path.of("target", "b"));
		// the leg wanting a profile is the one that runs: leg 1 wants a, 2 wants b, 3 wants a
		engine.browser(UserAgentType.tablet, () -> (runs.isEmpty() || (runs.size() == 2)) ? a : b);
		step = browser -> {
			if (runs.size() == 1) { // the first leg's step restarts its browser on b
				SailSettings legSettings = java.util.Objects.requireNonNull(browser.getSettings()); // the engine's test environment, which b would not resolve
				browser.stopBrowser();
				browser.startBrowser(b, legSettings);
			}
		};
		current = "w.xml";
		Workflow threeLegs = workflow(AUTOMATION, AUTOMATION, AUTOMATION);

		engine.runWorkflow("w.xml", threeLegs);

		assertEquals(List.of("start 1", "run 1 w.xml", "stop 1", "start 1", "run 1 w.xml",
								"stop 1", "start 2", "run 2 w.xml", "stop 2"),
						events);
	}

	/** A SmartClient leg drives the PrimeFaces leg's browser through SmartClient's Selenide, and stops it at the end. */
	@Test
	void aSmartClientLegDrivesTheBrowserThatAPrimeFacesLegLeft() {
		current = "w.xml";
		WebDriver driver = mock(WebDriver.class, RETURNS_DEEP_STUBS);
		step = browser -> {
			browser.driver = driver;
			step = null;
		};
		engine.runWorkflow("w.xml", workflow(AUTOMATION, SMART_CLIENT_AUTOMATION));
		assertEquals(List.of("start 1", "run 1 w.xml", "run SmartClientSelenide w.xml"), events);
		verify(driver).quit();
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
		SailSuite suite = SailSuite.resource("util/sail/fixtures/plain.suite.xml", engine)
									.environment(new SailSettings.Environment(Map.of(), Map.of(), null, Path.of("").toAbsolutePath()));

		SailSuite.Summary summary = suite.execute();

		assertEquals(List.of("start 1", "run 1 null", "stop 1"), events, "one member ran");
		assertEquals(4, summary.members(SailSuite.Outcome.SKIPPED).size(), summary.describe("kitchensink"));
		assertEquals(List.of("the run is stopping"), summary.skips().values().stream().distinct().toList());
	}
}
