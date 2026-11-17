package sail.load;

import java.time.Duration;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import util.AbstractH2Test;
import util.sail.SailLoad;
import util.sail.SailServerLock;

/**
 * A load run of the kitchen sink conformance suite ({@code sail/kitchenSink/kitchensink.suite.xml}): 5 virtual users,
 * started over 20 seconds, each starting the suite again and again for 2 minutes, each with its own browser and run
 * token. The suite has no bracket, and its members make and delete their own data, so the users can run it at once.
 * <p>
 * It shows that {@link SailLoad} composes with any test: this one extends {@link AbstractH2Test}, as the Biz Hub Apps
 * ITs do, and releases the server lock itself. The settings beat the code as usual, e.g.
 * {@code -Dsail.load.users=20 -Dsail.load.duration=10m -Dsail.load.maxP95=5s}.
 * <p>
 * {@code pf/pf-kitchensink-sail.xml} is left out: it signs in as admin with a wrong password on purpose, and every
 * virtual user signs in as admin, so with several users at once the failed attempts lock admin's account for all of
 * them. Tagged {@code performance}, not {@code smoke}: the {@code sail} profile leaves it out unless
 * {@code -Dgroups=performance} asks for it. Runs against {@code -Dsail.baseUrl} (required), never a shared server.
 */
@Tag("sail")
@Tag("performance")
class KitchenSinkLoadSailIT extends AbstractH2Test {
	@Test
	@SuppressWarnings({"static-method", "java:S2699"}) // the scripts' test steps are the assertions: the load run fails the test when an iteration fails
	void load() {
		// an iteration of the suite takes several minutes, so the ones in flight at the end get 20 minutes to finish
		SailLoad load = SailLoad.users(5).rampUp(Duration.ofSeconds(20)).forDuration(Duration.ofMinutes(2)).timeLimit(Duration.ofMinutes(20));
		// the bootstrap user's password on a test server; never in the scripts
		load.settings().variable("password", "admin")
				.suiteOnly("pf/pf-kitchensink-row-link.xml", "sc/", "workflow/");
		load.run("sail/kitchenSink/kitchensink.suite.xml");
	}

	@AfterAll
	static void releaseServer() {
		SailServerLock.releaseAll();
	}
}
