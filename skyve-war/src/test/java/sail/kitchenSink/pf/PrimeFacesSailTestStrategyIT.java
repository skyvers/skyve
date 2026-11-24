package sail.kitchenSink.pf;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import util.AbstractPrimeFacesInterpretedSail;
import util.sail.BrowserConfiguration;
import util.sail.BrowserConfiguration.Browsers;
import util.sail.Devices;
import util.sail.SailSuite;

/**
 * Checks that SAIL test strategies behave as {@link org.skyve.metadata.sail.language.Automation.TestStrategy} documents,
 * against the PrimeFaces kitchen sink: Assert stops at the first failure, Verify reports every failure at the end,
 * and None makes no assertions.
 * <p>
 * Runs against the server named by {@code -Dsail.baseUrl} (required),
 * headless unless {@code -Dsail.headless=false}. {@code -Dsail.testStrategy} is refused, as it would give every test
 * the same strategy.
 */
@Tag("sail")
@Tag("smoke")
class PrimeFacesSailTestStrategyIT extends AbstractPrimeFacesInterpretedSail {
	private static final String SCRIPT = """
			<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
			<automation uxui="tablet" userAgentType="tablet" testStrategy="%s"
						xmlns="http://www.skyve.org/xml/sail">
				<description>Signs in as admin in the PrimeFaces tablet UX/UI and runs steps that go wrong: values that differ from what was entered, a widget that is not there, and a check for an error that does not happen. The test runs it under each test strategy, and expects Assert to stop at the first failure, Verify to run on and report every failure at the end, and None to make no checks. The missing widget is left out for None.</description>
				<interaction name="Login">
					<description>Signs in as admin.</description>
					<method>
						<login user="admin" password="admin" />
					</method>
				</interaction>
				<interaction name="Wrong values">
					<description>Enters a value and then checks for different values, runs an action and checks for an error that it does not give. Each of those checks is expected to fail.</description>
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
					<description>Enters a value into a binding that the view does not have, which is expected to fail the step and so skip the rest of this interaction.</description>
					<method>
						<navigateEdit module="kitchensink" document="ActionsFixture" />
						<dataEnter binding="noSuchBinding" value="x" />
						<testValue binding="outcome" value="never checked" />
					</method>
				</interaction>
				<interaction name="Still runs">
					<description>Checks a value that is wrong, to show that a later interaction still runs after earlier failures when the strategy is Verify.</description>
					<method>
						<navigateEdit module="kitchensink" document="ActionsFixture" />
						<testValue binding="eventText" value="also wrong" />
					</method>
				</interaction>
			</automation>
			""";

	@BeforeEach
	void refuseOverride() {
		SailSuite.refuseTestStrategyOverride("PrimeFacesSailTestStrategyIT", settings().resolve());
	}

	public PrimeFacesSailTestStrategyIT() {
		super(new BrowserConfiguration()
				.browser(Browsers.chrome)
				.userAgentString(Devices.ipad.userAgentString));
	}

	@Test
	void verifyReportsEveryFailureAtTheEnd() {
		String script = SCRIPT.formatted("Verify");
		AssertionError e = assertThrows(AssertionError.class, () -> sailString(script));
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
		String script = SCRIPT.formatted("Assert");
		AssertionError e = assertThrows(AssertionError.class, () -> sailString(script));
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
