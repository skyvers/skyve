package util.sail;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
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
	/** The member running. */
	private String current;

	private final SailWorkflow engine = new SailWorkflow(new SailViewRoles(),
															(smartClient, configuration, resolved) -> start(),
															(automation, browser, run, settings) -> {
																events.add("run " + ids.get(browser) + ' ' + current);
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
}
