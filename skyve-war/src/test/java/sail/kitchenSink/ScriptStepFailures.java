package sail.kitchenSink;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.Arrays;
import java.util.function.Consumer;
import java.util.stream.Collectors;

import jakarta.annotation.Nonnull;

/**
 * The kitchen sinks' inline failure tests of the script and call steps ({@code execute}, {@code executeAsync},
 * {@code waitFor}, {@code testScript} and {@code call}), the same in each renderer: each kitchen sink IT runs them
 * through its own {@code sailString}, after its own sign-in. Each method runs its cases under Verify (and
 * {@link #waitForFailures()} also under None) and checks each failure the semantics table lists, with
 * {@code assertAll} and the case's name in each message.
 */
public final class ScriptStepFailures {
	private final String uxui;
	private final String afterLogin;
	private final Consumer<String> sail;

	/**
	 * Makes the tests for a renderer.
	 *
	 * @param uxui	the automation's {@code uxui} and {@code userAgentType}: {@code tablet} for PF, {@code desktop} for SC
	 * @param afterLogin	the steps after the sign-in, such as {@code <switchMode />} for SC, or empty
	 * @param sail	runs a SAIL automation from its XML, as the IT's {@code sailString} does
	 */
	public ScriptStepFailures(@Nonnull String uxui, @Nonnull String afterLogin, @Nonnull Consumer<String> sail) {
		this.uxui = uxui;
		this.afterLogin = afterLogin;
		this.sail = sail;
	}

	/**
	 * {@code <execute/>} fails when its script throws, with the error's message, and when it keeps a variable but its
	 * script gives no value. Run under Verify: a step that throws ends its interaction, so the step after the throw
	 * does not run.
	 */
	public void executeFailures() {
		String script = signedIn("Verify", """
				<interaction name="A throw">
					<method>
						<navigateEdit module="kitchensink" document="ActionsFixture" />
						<execute><![CDATA[throw new Error(['execute', 'threw', 'this'].join(' '));]]></execute>
						<testScript message="this never runs after the throw">return false;</testScript>
					</method>
				</interaction>
				<interaction name="No value to keep">
					<method>
						<navigateEdit module="kitchensink" document="ActionsFixture" />
						<execute variable="nothing"><![CDATA[window._sailNothing = true;]]></execute>
					</method>
				</interaction>
				""");
		AssertionError e = assertThrows(AssertionError.class, () -> sail.accept(script));
		String message = e.getMessage();
		String thrown = failureOf(message, "A throw");
		String noValue = failureOf(message, "No value to keep");
		assertAll(() -> assertTrue(message.contains("with 2 failure(s)"), "the count: " + message),
					() -> assertTrue(thrown.contains("JavascriptException"), "A throw: " + message),
					() -> assertTrue(thrown.contains("execute threw this"), "A throw: " + message),
					() -> assertFalse(message.contains("this never runs after the throw"), "A throw: " + message),
					() -> assertTrue(noValue.contains("gave no value to keep in {var:nothing}"), "No value to keep: " + message));
	}

	/**
	 * {@code <executeAsync/>} fails when its promise rejects, with the error's message, when it does not settle within
	 * {@code timeoutMillis}, and when it keeps a variable but resolves to no value. Run under Verify.
	 */
	public void executeAsyncFailures() {
		String script = signedIn("Verify", """
				<interaction name="A rejection">
					<method>
						<navigateEdit module="kitchensink" document="ActionsFixture" />
						<executeAsync><![CDATA[await Promise.resolve(); throw new Error(['executeAsync', 'rejected', 'this'].join(' '));]]></executeAsync>
					</method>
				</interaction>
				<interaction name="Not settled in time">
					<method>
						<navigateEdit module="kitchensink" document="ActionsFixture" />
						<executeAsync timeoutMillis="300"><![CDATA[await new Promise(function () { /* never settles */ });]]></executeAsync>
					</method>
				</interaction>
				<interaction name="No value to keep">
					<method>
						<navigateEdit module="kitchensink" document="ActionsFixture" />
						<executeAsync variable="nothing"><![CDATA[await Promise.resolve();]]></executeAsync>
					</method>
				</interaction>
				""");
		AssertionError e = assertThrows(AssertionError.class, () -> sail.accept(script));
		String message = e.getMessage();
		String rejected = failureOf(message, "A rejection");
		assertAll(() -> assertTrue(message.contains("with 3 failure(s)"), "the count: " + message),
					() -> assertTrue(rejected.contains("JavascriptException"), "A rejection: " + message),
					() -> assertTrue(rejected.contains("executeAsync rejected this"), "A rejection: " + message),
					() -> assertTrue(failureOf(message, "Not settled in time").contains("<executeAsync /> did not settle within 300 ms"),
										"Not settled in time: " + message),
					() -> assertTrue(failureOf(message, "No value to keep").contains("gave no value to keep in {var:nothing}"),
										"No value to keep: " + message));
	}

	/**
	 * {@code <waitFor/>} fails when its script has not held within {@code timeoutMillis}, giving the script's first
	 * line and, when a poll threw, the last poll's error. It is a wait, not a test: under Verify the timeout ends its
	 * interaction, and under None it still fails the run while a false {@code <testScript/>} before it is skipped.
	 */
	public void waitForFailures() {
		String verify = signedIn("Verify", """
				<interaction name="A poll that throws">
					<method>
						<navigateEdit module="kitchensink" document="ActionsFixture" />
						<waitFor timeoutMillis="300"><![CDATA[throw new Error(['no', 'poll', 'held'].join(' '));]]></waitFor>
					</method>
				</interaction>
				<interaction name="Never true">
					<method>
						<navigateEdit module="kitchensink" document="ActionsFixture" />
						<waitFor timeoutMillis="300"><![CDATA[return window._sailNeverSet === true;]]></waitFor>
						<testScript message="this never runs after the wait">return false;</testScript>
					</method>
				</interaction>
				""");
		AssertionError e = assertThrows(AssertionError.class, () -> sail.accept(verify));
		String message = e.getMessage();
		String threw = failureOf(message, "A poll that throws");
		String never = failureOf(message, "Never true");

		String none = signedIn("None", """
				<interaction name="A wait under None">
					<method>
						<navigateEdit module="kitchensink" document="ActionsFixture" />
						<testScript message="this is skipped under None">return false;</testScript>
						<waitFor timeoutMillis="300">return false;</waitFor>
					</method>
				</interaction>
				""");
		AssertionError n = assertThrows(AssertionError.class, () -> sail.accept(none));
		String noneMessage = n.getMessage();
		assertAll(() -> assertTrue(message.contains("with 2 failure(s)"), "the count: " + message),
					() -> assertTrue(threw.contains("<waitFor /> did not hold within 300 ms: throw new Error("), "A poll that throws: " + message),
					() -> assertTrue(threw.contains("; the last poll threw: "), "A poll that throws: " + message),
					() -> assertTrue(threw.contains("no poll held"), "A poll that throws: " + message),
					() -> assertTrue(never.contains("<waitFor /> did not hold within 300 ms: return window._sailNeverSet === true;"),
										"Never true: " + message),
					() -> assertFalse(never.contains("the last poll threw"), "Never true: " + message),
					() -> assertFalse(message.contains("this never runs after the wait"), "Never true: " + message),
					() -> assertTrue(noneMessage.contains("<waitFor /> did not hold within 300 ms: return false;"), "under None: " + noneMessage),
					() -> assertFalse(noneMessage.contains("this is skipped under None"), "under None: " + noneMessage));
	}

	/**
	 * {@code <testScript/>} fails as an assertion: with its {@code message}, its variables substituted, when it is
	 * still false at {@code timeoutMillis}; with the script's first line when it has no message; and with what the
	 * last run threw. Run under Verify: a failed assertion is recorded, and the next step runs.
	 */
	public void testScriptFailures() {
		String script = signedIn("Verify", """
				<interaction name="False, with a message">
					<method>
						<navigateEdit module="kitchensink" document="ActionsFixture" />
						<execute variable="kept"><![CDATA[return ['kept', 'value'].join(' ');]]></execute>
						<testScript message="The kept value was {var:kept}" timeoutMillis="300"><![CDATA[return sail.vars.kept === 'another value';]]></testScript>
						<testScript message="this ran after the failed assertion">return false;</testScript>
					</method>
				</interaction>
				<interaction name="False, without a message">
					<method>
						<navigateEdit module="kitchensink" document="ActionsFixture" />
						<testScript><![CDATA[return 1 + 1 === 3;]]></testScript>
					</method>
				</interaction>
				<interaction name="A run that throws">
					<method>
						<navigateEdit module="kitchensink" document="ActionsFixture" />
						<testScript><![CDATA[throw new Error(['the', 'test', 'threw'].join(' '));]]></testScript>
					</method>
				</interaction>
				""");
		AssertionError e = assertThrows(AssertionError.class, () -> sail.accept(script));
		String message = e.getMessage();
		String withMessage = failureOf(message, "False, with a message");
		String withoutMessage = failureOf(message, "False, without a message");
		String threw = failureOf(message, "A run that throws");
		assertAll(() -> assertTrue(message.contains("with 4 failure(s)"), "the count: " + message),
					() -> assertTrue(withMessage.contains("The kept value was kept value"), "False, with a message: " + message),
					() -> assertTrue(withMessage.contains("this ran after the failed assertion"), "False, with a message: " + message),
					() -> assertTrue(withoutMessage.contains("<testScript /> did not hold: return 1 + 1 === 3;"), "False, without a message: " + message),
					() -> assertFalse(withoutMessage.contains("; it threw: "), "False, without a message: " + message),
					() -> assertTrue(threw.contains("<testScript /> did not hold: throw new Error("), "A run that throws: " + message),
					() -> assertTrue(threw.contains("; it threw: "), "A run that throws: " + message),
					() -> assertTrue(threw.contains("the test threw"), "A run that throws: " + message));
	}

	/**
	 * {@code <call/>} fails when its class cannot be loaded, is not a {@code SailCall} or cannot be constructed (the
	 * {@code SailCall} interface itself), when it keeps a variable but the call gives no value, when the call throws,
	 * and when a parameter is named twice. Run under Verify: an {@link AssertionError} from the call is a failed
	 * assertion, so the next step runs; any other exception ends the interaction.
	 */
	public void callFailures() {
		String script = signedIn("Verify", """
				<interaction name="No such class">
					<method>
						<navigateEdit module="kitchensink" document="ActionsFixture" />
						<call className="sail.kitchenSink.NoSuchCall" />
					</method>
				</interaction>
				<interaction name="Not a SailCall">
					<method>
						<navigateEdit module="kitchensink" document="ActionsFixture" />
						<call className="java.lang.String" />
					</method>
				</interaction>
				<interaction name="Cannot be constructed">
					<method>
						<navigateEdit module="kitchensink" document="ActionsFixture" />
						<call className="org.skyve.metadata.sail.execution.SailCall" />
					</method>
				</interaction>
				<interaction name="No value to keep">
					<method>
						<navigateEdit module="kitchensink" document="ActionsFixture" />
						<call className="util.sail.EchoCall" variable="nothing" />
					</method>
				</interaction>
				<interaction name="A failed assertion">
					<method>
						<navigateEdit module="kitchensink" document="ActionsFixture" />
						<call className="util.sail.EchoCall">
							<parameter name="text" value="the assertion's text" />
							<parameter name="fail" value="assert" />
						</call>
						<testScript message="this ran after the call's failed assertion">return false;</testScript>
					</method>
				</interaction>
				<interaction name="An error">
					<method>
						<navigateEdit module="kitchensink" document="ActionsFixture" />
						<call className="util.sail.EchoCall">
							<parameter name="text" value="the error's text" />
							<parameter name="fail" value="error" />
						</call>
						<testScript message="this never runs after the call's error">return false;</testScript>
					</method>
				</interaction>
				<interaction name="A duplicate parameter">
					<method>
						<navigateEdit module="kitchensink" document="ActionsFixture" />
						<call className="util.sail.EchoCall">
							<parameter name="text" value="one" />
							<parameter name="text" value="two" />
						</call>
					</method>
				</interaction>
				""");
		AssertionError e = assertThrows(AssertionError.class, () -> sail.accept(script));
		String message = e.getMessage();
		String assertion = failureOf(message, "A failed assertion");
		String error = failureOf(message, "An error");
		assertAll(() -> assertTrue(message.contains("with 8 failure(s)"), "the count: " + message),
					() -> assertTrue(failureOf(message, "No such class").contains("<call className=\"sail.kitchenSink.NoSuchCall\" /> names a class that cannot be loaded"),
										"No such class: " + message),
					() -> assertTrue(failureOf(message, "Not a SailCall").contains("<call className=\"java.lang.String\" /> names a class that is not a org.skyve.metadata.sail.execution.SailCall"),
										"Not a SailCall: " + message),
					() -> assertTrue(failureOf(message, "Cannot be constructed").contains("<call className=\"org.skyve.metadata.sail.execution.SailCall\" /> names a class that cannot be constructed"),
										"Cannot be constructed: " + message),
					() -> assertTrue(failureOf(message, "No value to keep").contains("gave no value to keep in {var:nothing}"), "No value to keep: " + message),
					() -> assertTrue(assertion.contains("<call className=\"util.sail.EchoCall\" /> failed: EchoCall was asked to fail an assertion on the assertion's text"),
										"A failed assertion: " + message),
					() -> assertTrue(assertion.contains("this ran after the call's failed assertion"), "A failed assertion: " + message),
					() -> assertTrue(error.contains("<call className=\"util.sail.EchoCall\" /> threw "), "An error: " + message),
					() -> assertTrue(error.contains("IllegalStateException: EchoCall was asked to throw on the error's text"), "An error: " + message),
					() -> assertFalse(message.contains("this never runs after the call's error"), "An error: " + message),
					() -> assertTrue(failureOf(message, "A duplicate parameter").contains("has more than one <parameter name=\"text\" />"),
										"A duplicate parameter: " + message));
	}

	/**
	 * Wraps interactions in an automation that signs in as admin before them and signs out after them.
	 *
	 * @param testStrategy	the automation's test strategy
	 * @param interactions	the interactions' XML
	 * @return the automation's XML
	 */
	private String signedIn(String testStrategy, String interactions) {
		return """
				<automation uxui="%s" userAgentType="%s" testStrategy="%s" xmlns="http://www.skyve.org/xml/sail">
					<before>
						<login user="admin" password="admin" />
						%s
					</before>
				%s
					<after>
						<logout />
					</after>
				</automation>
				""".formatted(uxui, uxui, testStrategy, afterLogin, interactions);
	}

	/**
	 * Returns what the named interaction recorded in a Verify run's failure message.
	 *
	 * @param message	the run's failure message, one failure per {@code "\n  - "}
	 * @param interaction	the interaction's name
	 * @return its failures, one per line, or an empty string if it recorded none
	 */
	private static String failureOf(String message, String interaction) {
		String prefix = "Interaction \"" + interaction + "\" step ";
		return Arrays.stream(message.split("\n  - ")).filter(f -> f.startsWith(prefix)).collect(Collectors.joining("\n"));
	}
}
