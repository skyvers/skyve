package sail.admin.pf;

import org.junit.jupiter.api.Test;

import util.AbstractPrimeFacesInterpretedSail;
import util.sail.BrowserConfiguration;
import util.sail.BrowserConfiguration.Browsers;
import util.sail.Devices;
import util.sail.SailSuite;

/**
 * Runs the PrimeFaces admin module SAIL suite ({@code sail/admin/pf/suite/*.xml}) in order, in one browser session,
 * with {@link SailSuite}.
 * <p>
 * {@code 00-precondition} checks that the server has no backup and no restore marker; {@code 00-backup} backs up the
 * data; {@code 99-restore} restores that backup, so the suite leaves the server as it found it and can be rerun.
 * 99 restores the first backup listed, which is the oldest (runtime bug), so it runs only when 00-precondition passed:
 * otherwise it could restore a stray backup. It runs even when a later file fails, and the test then reports both outcomes.
 * The destructive actions run in the file before the restore, so nothing depends on what they destroy.
 * Run it only against a disposable server.
 * <p>
 * Runs against {@code -Dsail.baseUrl} (required: there is no default server), headless unless
 * {@code -Dsail.headless=false}. {@code -Dsail.suite.only=03,04} runs only the files with those prefixes, between
 * the backup and the restore. {@code -Dsail.testStrategy} is refused.
 */
class PrimeFacesAdminSuiteSailIT extends AbstractPrimeFacesInterpretedSail {
	public PrimeFacesAdminSuiteSailIT() {
		super(new BrowserConfiguration()
				.browser(Browsers.chrome)
				.userAgentString(Devices.ipad.userAgentString));
	}

	@Test
	void suite() {
		// The user that 03-security-admin makes, in a group with these roles: build that user's views with them
		viewRoles("sail{run}", "admin.BasicUser", "admin.ContactViewer");
		new SailSuite("sail/admin/pf/suite/")
				.setUp("00-precondition.xml", "00-backup.xml")
				.files("01-session.xml",
						"02-contacts-communications.xml",
						"03-security-admin.xml",
						"04-devops.xml",
						"05-reports-misc.xml",
						"98-destructive.xml")
				.restore("99-restore.xml", "00-precondition.xml")
				.run(this::sailResource);
	}
}
