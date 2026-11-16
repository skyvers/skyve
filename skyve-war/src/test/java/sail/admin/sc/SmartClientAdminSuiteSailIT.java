package sail.admin.sc;

import org.junit.jupiter.api.Test;

import util.AbstractSmartClientInterpretedSail;
import util.sail.BrowserConfiguration;
import util.sail.BrowserConfiguration.Browsers;
import util.sail.SailSuite;

/**
 * Runs the SmartClient admin module SAIL suite ({@code sail/admin/sc/suite/*.xml}) in order, in one browser session,
 * with {@link SailSuite}.
 * <p>
 * The suite is bracketed by a backup and a restore, so it leaves the server as it found it and can be rerun:
 * <ol>
 * <li>Set up: {@code 00-precondition.xml} checks that no restore marker is left over from an earlier run;
 * {@code 00-backup.xml} clears the completed jobs list and backs up the data; {@code 00-marker.xml} leaves a marker for
 * the restore to remove.</li>
 * <li>The suite's files, stopping at the first that fails, as later files build on earlier ones. The destructive
 * actions run last ({@code 98-destructive.xml}), then {@code 99-truncate.xml} (SmartClient's Truncate, which first
 * checks that the backup is there).</li>
 * <li>{@code 99-restore.xml} runs last, even after a failure (the truncate's too, so nothing between the truncate and the
 * restore can leave the server without its data), but only if the precondition and the backup passed: otherwise there
 * is nothing of this run's to restore, and the newest backup, which the restore takes, could be another one.</li>
 * </ol>
 * Run it only against a disposable server; the base class lets one SAIL test at a time use a server.
 * <p>
 * Runs against {@code -Dsail.baseUrl}, headless unless {@code -Dsail.headless=false}. {@code -Dsail.suite.only=03,04}
 * runs only the suite files with those prefixes (the set up and the restore still run). {@code -Dsail.testStrategy} is
 * refused.
 */
class SmartClientAdminSuiteSailIT extends AbstractSmartClientInterpretedSail {
	public SmartClientAdminSuiteSailIT() {
		super(new BrowserConfiguration()
				.browser(Browsers.chrome));
	}

	@Test
	@SuppressWarnings("java:S2699") // the SAIL script's test steps are the assertions: the harness fails the test when one fails
	void suite() {
		// 03 makes this user with a group giving these roles, then signs in as them: build their views with their roles
		viewRoles("sail{run}", "admin.BasicUser", "admin.ContactViewer");
		new SailSuite("sail/admin/sc/suite/")
				.setUp("00-precondition.xml", "00-backup.xml", "00-marker.xml")
				.files("01-session.xml",
						"02-contacts-communications.xml",
						"03-security-admin.xml",
						"04-devops.xml",
						"05-reports-misc.xml",
						"98-destructive.xml",
						"99-truncate.xml")
				.restore("99-restore.xml", "00-precondition.xml", "00-backup.xml")
				.run(this::sailResource);
	}
}
