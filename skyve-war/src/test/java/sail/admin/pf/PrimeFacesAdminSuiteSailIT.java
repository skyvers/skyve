package sail.admin.pf;

import static org.junit.jupiter.api.Assertions.assertAll;

import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.function.Executable;

import util.AbstractPrimeFacesInterpretedSail;
import util.sail.BrowserConfiguration;
import util.sail.BrowserConfiguration.Browsers;
import util.sail.Devices;

/**
 * Runs the PrimeFaces admin module SAIL suite ({@code sail/admin/pf/suite/*.xml}) in order, in one browser session.
 * <p>
 * The first file backs up the data and the last file restores that backup, so the suite leaves the server as it found it
 * and can be rerun. The restore runs even when an earlier file fails, and the test then reports both outcomes.
 * The destructive actions run in the file before the restore, so nothing depends on what they destroy.
 * Run it only against a disposable server.
 * <p>
 * 00 first checks that the server has no other backup and no restore marker: 99 restores the oldest backup listed
 * (runtime bug), so when 00 fails before that check has passed the restore is held back, as it could restore a stray
 * backup.
 * <p>
 * Runs against {@code -Dsail.baseUrl} (required: there is no default server),
 * headless unless {@code -Dsail.headless=false}. {@code -Dsail.suite.only=03,04} runs only the files with those
 * prefixes, to work on a part of the suite; the restore runs only when 00 is one of them.
 */
class PrimeFacesAdminSuiteSailIT extends AbstractPrimeFacesInterpretedSail {
	private static final String FOLDER = "sail/admin/pf/suite/";
	private static final String[] FILES = {
		"00-backup.xml",
		"01-session.xml",
		"02-contacts-communications.xml",
		"03-security-admin.xml",
		"04-devops.xml",
		"05-reports-misc.xml",
		"98-destructive.xml"
	};
	private static final String RESTORE = "99-restore.xml";
	// 00's interactions after its first one, which checks that the server has no stray backup or marker
	private static final List<String> AFTER_PRECONDITION = List.of("Interaction \"Clear the completed jobs",
																		"Interaction \"Back up the data",
																		"Interaction \"Leave a marker");

	public PrimeFacesAdminSuiteSailIT() {
		super(new BrowserConfiguration()
				.browser(Browsers.chrome)
				.userAgentString(Devices.ipad.userAgentString));
	}

	@Test
	void suite() {
		String only = System.getProperty("sail.suite.only");
		List<String> prefixes = ((only == null) || only.isBlank()) ? List.of() : List.of(only.split("\\s*,\\s*"));

		List<Executable> outcomes = new ArrayList<>(2);
		boolean backedUp = false;
		for (String file : FILES) {
			if ((! prefixes.isEmpty()) && prefixes.stream().noneMatch(file::startsWith)) {
				continue;
			}
			backedUp = backedUp || file.equals(FILES[0]);
			try {
				sailResource(FOLDER + file);
			}
			catch (AssertionError e) {
				// Stop at the first failing file, as later files build on earlier ones, but still restore,
				// unless 00 failed before its precondition passed: then a stray backup may be listed first, and restoring would restore it
				String message = String.valueOf(e.getMessage());
				if (file.equals(FILES[0]) && AFTER_PRECONDITION.stream().noneMatch(message::contains)) {
					backedUp = false;
				}
				outcomes.add(() -> {
					throw new AssertionError(file + " failed: " + e.getMessage(), e);
				});
				break;
			}
		}

		// Restore only the backup that this run made
		try {
			if (backedUp) {
				sailResource(FOLDER + RESTORE);
			}
		}
		catch (AssertionError e) {
			outcomes.add(() -> {
				throw new AssertionError(RESTORE + " failed: " + e.getMessage(), e);
			});
		}

		assertAll(outcomes);
	}
}
