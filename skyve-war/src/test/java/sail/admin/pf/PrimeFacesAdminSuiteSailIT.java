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
 * Runs against {@code -Dsail.baseUrl} (default {@value BrowserConfiguration#DEFAULT_BASE_URL}),
 * headless unless {@code -Dsail.headless=false}. {@code -Dsail.suite.only=03,04} runs only the files with those
 * prefixes (and the restore), to work on a part of the suite.
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
		"06-phone.xml",
		"98-destructive.xml"
	};
	private static final String RESTORE = "99-restore.xml";

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
		for (String file : FILES) {
			if ((! prefixes.isEmpty()) && prefixes.stream().noneMatch(file::startsWith)) {
				continue;
			}
			try {
				sailResource(FOLDER + file);
			}
			catch (AssertionError e) {
				// Stop at the first failing file, as later files build on earlier ones, but still restore
				outcomes.add(() -> {
					throw new AssertionError(file + " failed: " + e.getMessage(), e);
				});
				break;
			}
		}

		try {
			sailResource(FOLDER + RESTORE);
		}
		catch (AssertionError e) {
			outcomes.add(() -> {
				throw new AssertionError(RESTORE + " failed: " + e.getMessage(), e);
			});
		}

		assertAll(outcomes);
	}
}
