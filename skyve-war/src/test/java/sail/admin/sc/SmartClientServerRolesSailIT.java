package sail.admin.sc;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import util.AbstractSmartClientInterpretedSail;
import util.sail.BrowserConfiguration;
import util.sail.BrowserConfiguration.Browsers;

/**
 * Builds the views of a less privileged user with the roles the server says the user has, with no
 * {@code viewRoles} registration: the admin suite's security file signs in as a user it makes with a group's roles
 * (completion plan 1.5). {@link SmartClientAdminSuiteSailIT} registers that user's roles; this test does not, so it
 * passes only if the harness asks the server for them.
 * <p>
 * Runs against the server named by {@code -Dsail.baseUrl} (required), whose {@code skyve.json} has
 * {@code sail.recording} on, so that the server answers {@code sailrecording?roles}. The file makes and deletes its own
 * data group, group and user; it leaves sign in history behind, so run it only against a disposable server.
 */
class SmartClientServerRolesSailIT extends AbstractSmartClientInterpretedSail {
	public SmartClientServerRolesSailIT() {
		super(new BrowserConfiguration().browser(Browsers.chrome));
	}

	@Test
	void anUnregisteredUserGetsTheServersRoles() {
		Assertions.assertDoesNotThrow(() -> sailResource("sail/admin/sc/suite/03-security-admin.xml"));
	}
}
