package sail;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assumptions.assumeFalse;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.function.Executable;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.impl.util.XMLMetaData.SAILRoot;

import util.AbstractInterpretedSailWorkflow;
import util.sail.SailSettings;

/**
 * The generic SAIL runner: runs the scripts and suites that {@value SailSettings#RUN} names, so a recorded script or a
 * suite file runs with no Java. Each entry is a classpath resource or a file path (a file that exists wins), and may
 * be a {@code <suite>}, a {@code <workflow>} or an {@code <automation>}, e.g.
 * <pre>
 * mvn -pl skyve-war -am verify -Psail -Dsail.baseUrl=http://localhost:8080/skyve/ \
 * 	-Dsail.run=sail/kitchenSink/kitchensink.suite.xml,/home/me/recorded.xml -Dsail.var.password=admin
 * </pre>
 * Entries run in order, each independent of the others: the test fails with every entry that failed. Skipped (not
 * failed) when {@value SailSettings#RUN} is not set. Runs against {@code -Dsail.baseUrl} (required); the other settings,
 * such as {@code sail.var.<name>} for a script's {@code {var:name}}, apply as usual.
 */
@Tag("sail")
class SailRunIT extends AbstractInterpretedSailWorkflow {
	@Test
	@SuppressWarnings("java:S2699") // the scripts' test steps are the assertions: the harness fails the test when one fails
	void run() {
		List<String> entries = settings().resolve().run();
		assumeFalse(entries.isEmpty(), "Set -D" + SailSettings.RUN + " (or SAIL_RUN) to the scripts and suites to run");
		List<Executable> outcomes = new ArrayList<>(entries.size());
		for (String entry : entries) {
			Throwable failure = runEntry(entry);
			if (failure != null) {
				outcomes.add(() -> {
					throw new AssertionError(entry + " failed: " + failure.getMessage(), failure);
				});
			}
		}
		assertAll("SAIL run of " + entries, outcomes);
	}

	/**
	 * Runs one entry as the kind of SAIL document it is.
	 *
	 * @return its failure, or null if it passed
	 */
	private Throwable runEntry(String entry) {
		System.out.println("SAIL run: " + entry);
		try {
			boolean file = Files.isRegularFile(Path.of(entry));
			SAILRoot root = file ? XMLMetaData.sailRootOfFile(entry) : XMLMetaData.sailRootOfString(resource(entry));
			if (root == SAILRoot.SUITE) {
				if (file) {
					sailSuiteFile(entry);
				}
				else {
					sailSuite(entry);
				}
			}
			else if (file) {
				sailWorkflowFile(entry);
			}
			else {
				sailWorkflow(entry);
			}
			return null;
		}
		catch (AssertionError | RuntimeException e) { // a failed script, or a missing or invalid one
			return e;
		}
	}

	private static String resource(String resourceName) {
		try (InputStream stream = Thread.currentThread().getContextClassLoader().getResourceAsStream(resourceName)) {
			if (stream == null) {
				throw new IllegalArgumentException(resourceName + " is neither a file nor a resource on the test classpath");
			}
			return new String(stream.readAllBytes(), StandardCharsets.UTF_8);
		}
		catch (IOException e) {
			throw new IllegalArgumentException("Could not read resource " + resourceName, e);
		}
	}
}
