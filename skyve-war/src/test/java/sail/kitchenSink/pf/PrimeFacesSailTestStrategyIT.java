package sail.kitchenSink.pf;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

import util.AbstractPrimeFacesInterpretedSail;
import util.sail.BrowserConfiguration;
import util.sail.BrowserConfiguration.Browsers;
import util.sail.Devices;

/**
 * Checks that SAIL test strategies behave as {@link org.skyve.metadata.sail.language.Automation.TestStrategy} documents,
 * against the PrimeFaces kitchen sink: Assert stops at the first failure, Verify reports every failure at the end,
 * and None makes no assertions.
 * <p>
 * Runs against {@code -Dsail.baseUrl} (default {@value BrowserConfiguration#DEFAULT_BASE_URL}),
 * headless unless {@code -Dsail.headless=false}.
 */
class PrimeFacesSailTestStrategyIT extends AbstractPrimeFacesInterpretedSail {
	private static final String SCRIPT = """
			<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
			<automation uxui="tablet" userAgentType="tablet" testStrategy="%s"
						xmlns="http://www.skyve.org/xml/sail">
				<interaction name="Login">
					<method>
						<login user="admin" password="admin" />
					</method>
				</interaction>
				<interaction name="Wrong values">
					<method>
						<navigateEdit module="kitchensink" document="ActionsFixture" />
						<dataEnter binding="eventText" value="entered" />
						<testValue binding="eventText" value="not entered" />
						<testValue binding="outcome" value="not the outcome" />
						<action name="RunFromToolbar" />
						<testValue binding="outcome" value="Run From Toolbar ran" />
						<testFailure />
					</method>
				</interaction>
				<interaction name="Missing widget">
					<method>
						<navigateEdit module="kitchensink" document="ActionsFixture" />
						<dataEnter binding="noSuchBinding" value="x" />
						<testValue binding="outcome" value="never checked" />
					</method>
				</interaction>
				<interaction name="Still runs">
					<method>
						<navigateEdit module="kitchensink" document="ActionsFixture" />
						<testValue binding="eventText" value="also wrong" />
					</method>
				</interaction>
			</automation>
			""";

	public PrimeFacesSailTestStrategyIT() {
		super(new BrowserConfiguration()
				.browser(Browsers.chrome)
				.userAgentString(Devices.ipad.userAgentString));
	}

	@Test
	void verifyReportsEveryFailureAtTheEnd() {
		AssertionError e = assertThrows(AssertionError.class, () -> sailString(SCRIPT.formatted("Verify")));
		String message = e.getMessage();
		assertAll(() -> assertTrue(message.contains("with 5 failure(s)"), message),
					() -> assertTrue(message.contains("Interaction \"Wrong values\" step 3 <testValue binding=\"eventText\" value=\"not entered\" />: expected [eventText] to be 'not entered' but it was 'entered'"), message),
					() -> assertTrue(message.contains("Interaction \"Wrong values\" step 4"), message),
					() -> assertTrue(message.contains("Interaction \"Wrong values\" step 7 <testFailure />: expected an error message"), message),
					// a step that throws skips the rest of its interaction
					() -> assertTrue(message.contains("Interaction \"Missing widget\" step 2 <dataEnter"), message),
					() -> assertFalse(message.contains("never checked"), message),
					() -> assertTrue(message.contains("Interaction \"Still runs\" step 2"), message));
	}

	@Test
	void assertStopsAtTheFirstFailure() {
		AssertionError e = assertThrows(AssertionError.class, () -> sailString(SCRIPT.formatted("Assert")));
		String message = e.getMessage();
		assertAll(() -> assertTrue(message.startsWith("Interaction \"Wrong values\" step 3 "), message),
					() -> assertFalse(message.contains("step 4"), message));
	}

	@Test
	void noneMakesNoAssertions() {
		// The throwing step still fails the automation, so leave the missing widget out
		String script = SCRIPT.formatted("None").replace("<dataEnter binding=\"noSuchBinding\" value=\"x\" />", "");
		assertDoesNotThrow(() -> sailString(script));
	}
}
