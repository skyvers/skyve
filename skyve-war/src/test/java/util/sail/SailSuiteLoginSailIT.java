package util.sail;

import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import util.AbstractInterpretedSailWorkflow;

/**
 * Runs a suite of two members against a Skyve server, in a real browser: the first member signs in and out several
 * times, and leaves a cookie and a local storage entry; the second signs in first, and must find none of what the first
 * left, as an independent run does. Needs a disposable server ({@code -Dsail.baseUrl}), like the other SAIL ITs.
 */
@Tag("sail")
class SailSuiteLoginSailIT extends AbstractInterpretedSailWorkflow {
	@Test
	void theSecondMemberStartsInABrowserOfItsOwn() {
		sailSuite("util/sail/fixtures/login.suite.xml");
	}
}
