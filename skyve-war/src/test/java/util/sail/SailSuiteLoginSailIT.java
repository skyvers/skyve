package util.sail;

import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import util.AbstractInterpretedSailWorkflow;

/**
 * Runs a suite of two members against a Skyve server, in a real browser: the first member signs in and out several
 * times, and leaves a local storage entry; the second signs in first, and, with {@code browserPerMember()}, must find
 * none of what the first left. Without it, the default, the browser is handed on. Needs a disposable server ({@code -Dsail.baseUrl}), like the other SAIL ITs.
 */
@Tag("sail")
class SailSuiteLoginSailIT extends AbstractInterpretedSailWorkflow {
	@Test
	@SuppressWarnings("java:S2699") // a failed step or member throws an AssertionError, so the suite run is the assertion
	void theSecondMemberStartsInABrowserOfItsOwnWhenTheSuiteOptsIn() {
		sailSuite("util/sail/fixtures/login.suite.xml", true);
	}
}
