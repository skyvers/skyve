package sail.admin.sc;

import static org.junit.jupiter.api.Assertions.assertAll;

import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.function.Executable;

import jakarta.annotation.Nonnull;

import util.AbstractSmartClientInterpretedSail;
import util.sail.BrowserConfiguration;
import util.sail.BrowserConfiguration.Browsers;

/**
 * Runs the SmartClient admin module SAIL suite ({@code sail/admin/sc/suite/*.xml}) in order, in one browser session.
 * <p>
 * The suite is bracketed by a backup and a restore, so it leaves the server as it found it and can be rerun:
 * <ol>
 * <li>{@code 00-precondition.xml} checks that the server has no backup and no restore marker: the restore takes the
 * oldest backup listed (runtime bug), so it must be this run's. When it fails, nothing else runs.</li>
 * <li>{@code 00-backup.xml} clears the completed jobs list and backs up the data. Only once it has passed is there
 * something to restore, so the restore bracket runs only then.</li>
 * <li>The other files run in order, stopping at the first that fails, as later files build on earlier ones.
 * The destructive actions run last ({@code 98-destructive.xml}), so nothing depends on what they destroy.</li>
 * <li>The restore bracket: {@code 99-truncate.xml} (SmartClient's Truncate, which checks first that the backup is
 * there), then {@code 99-restore.xml}, which runs even when the truncate file failed, so a failure after the truncate
 * cannot leave the server without its data.</li>
 * </ol>
 * Any failure of a file (an assertion, or an exception such as an unreadable script) counts as that file failing;
 * the test then reports every outcome. Run it only against a disposable server, one suite at a time.
 * <p>
 * Runs against {@code -Dsail.baseUrl}, headless unless {@code -Dsail.headless=false}. {@code -Dsail.suite.only=03,04}
 * runs only the files with those prefixes, to work on a part of the suite; the restore bracket runs only when 00 is one
 * of them.
 */
class SmartClientAdminSuiteSailIT extends AbstractSmartClientInterpretedSail {
	private static final String FOLDER = "sail/admin/sc/suite/";
	private static final String PRECONDITION = "00-precondition.xml";
	private static final String BACKUP = "00-backup.xml";
	private static final String[] FILES = {
		PRECONDITION,
		BACKUP,
		"00-marker.xml",
		"01-session.xml",
		"02-contacts-communications.xml",
		"03-security-admin.xml",
		"04-devops.xml",
		"05-reports-misc.xml",
		"98-destructive.xml"
	};
	private static final String[] RESTORE = {
		"99-truncate.xml",
		"99-restore.xml"
	};

	public SmartClientAdminSuiteSailIT() {
		super(new BrowserConfiguration()
				.browser(Browsers.chrome));
	}

	@Test
	void suite() {
		String only = System.getProperty("sail.suite.only");
		List<String> prefixes = ((only == null) || only.isBlank()) ? List.of() : List.of(only.split("\\s*,\\s*"));

		List<Executable> outcomes = new ArrayList<>(3);
		// set only once this run's backup has been made: restoring otherwise could restore a stray backup, or nothing
		boolean backedUp = false;
		for (String file : FILES) {
			if ((! prefixes.isEmpty()) && prefixes.stream().noneMatch(file::startsWith)) {
				continue;
			}
			if (! run(file, outcomes)) {
				break;
			}
			backedUp = backedUp || file.equals(BACKUP);
		}

		if (backedUp) {
			for (String file : RESTORE) {
				run(file, outcomes);
			}
		}

		assertAll(outcomes);
	}

	/**
	 * Runs a suite file, recording its failure.
	 *
	 * @param file		the file, in {@link #FOLDER}
	 * @param outcomes	the failures so far, added to
	 * @return {@code true} if the file passed
	 */
	private boolean run(@Nonnull String file, @Nonnull List<Executable> outcomes) {
		try {
			sailResource(FOLDER + file);
			return true;
		}
		catch (AssertionError | RuntimeException e) {
			outcomes.add(() -> {
				throw new AssertionError(file + " failed: " + e.getMessage(), e);
			});
			return false;
		}
	}
}
