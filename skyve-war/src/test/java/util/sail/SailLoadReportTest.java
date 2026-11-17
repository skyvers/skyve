package util.sail;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.time.Duration;
import java.util.List;

import org.junit.jupiter.api.Test;
import org.opentest4j.MultipleFailuresError;

import util.sail.SailLoad.Plan;
import util.sail.SailLoadReport.FailureKind;
import util.sail.SailLoadReport.Iteration;
import util.sail.SailLoadReport.Sample;

/**
 * Tests {@link SailLoadReport}'s statistics, failure kinds, verdict and text from samples and iterations given
 * directly: no run, no clock.
 */
@SuppressWarnings({"static-method", "java:S5960"}) // test-only assertions
class SailLoadReportTest {
	@Test
	void percentilesAreNearestRank() {
		long[] twenty = new long[20];
		for (int i = 0; i < 20; i++) {
			twenty[i] = i + 1L;
		}

		assertAll(() -> assertEquals(10L, SailLoadReport.percentile(twenty, 50)),
					() -> assertEquals(19L, SailLoadReport.percentile(twenty, 95)),
					() -> assertEquals(20L, SailLoadReport.percentile(twenty, 100)),
					() -> assertEquals(7L, SailLoadReport.percentile(new long[] {7L}, 95), "one sample is every percentile"),
					() -> assertEquals(1L, SailLoadReport.percentile(new long[] {1L, 2L}, 50)));
	}

	@Test
	void noIterationFailsTheRun() {
		SailLoadReport report = new SailLoadReport("x", plan(1), List.of(), List.of(), 0L, List.of());

		assertAll(() -> assertEquals("no iteration ran", report.violations().get(0).message()),
					() -> assertEquals(0.0, report.failureRate()),
					() -> assertEquals(0.0, report.iterationsPerMinute()));
	}

	@Test
	void aFailedIterationsMessageIsItsFirstFailure() {
		MultipleFailuresError several = new MultipleFailuresError("2 failed",
																	List.of(new AssertionError("SAIL suite x: a.xml failed: step 1"), new AssertionError("b")));

		assertAll(() -> assertEquals("SAIL suite x: a.xml failed: step 1", SailLoadReport.message(several)),
					() -> assertEquals("java.lang.IllegalStateException", SailLoadReport.message(new IllegalStateException())));
	}

	@Test
	void theFirstFailureOfEachKindCountsEveryOccurrence() {
		Plan plan = plan(2);
		List<Sample> samples = List.of(new Sample("Save", 2, 1, 10L, 20L, true, "Interaction \"Save\" step 2: first"),
										new Sample("Save", 1, 1, 30L, 40L, true, null),
										new Sample("Open", 1, 2, 50L, 60L, false, null));
		AssertionError thrown = new AssertionError("SAIL workflow: x.xml failed: Interaction \"Save\" step 2: second");
		List<Iteration> iterations = List.of(new Iteration(2, 1, 0L, 25L, new AssertionError("verify")),
												new Iteration(1, 1, 0L, 45L, thrown),
												new Iteration(1, 2, 45L, 70L, new IllegalStateException("outside", new IOException("disk"))));

		List<FailureKind> kinds = new SailLoadReport("x", plan, samples, iterations, 70L, List.of()).failureKinds();

		assertAll(() -> assertEquals(2, kinds.size()),
					() -> assertEquals(new FailureKind("interaction \"Save\"", 2, 2, 1, "Interaction \"Save\" step 2: first", iterations.get(0).failure()),
										kinds.get(0)),
					() -> assertEquals("outside an interaction (IOException)", kinds.get(1).kind()),
					() -> assertEquals("outside", kinds.get(1).message()));
	}

	@Test
	void theSummaryDescribesInteractionsUsersAndFailures() {
		List<Sample> samples = List.of(new Sample("A very long interaction name that goes on and on beyond sixty characters wide", 1, 1, 0L, 2_000_000L, false, null),
										new Sample("Save", 1, 1, 2_000_000L, 5_000_000L, true, "Interaction \"Save\": no\n  at   the page"));
		SailLoadReport report = new SailLoadReport("x.xml", plan(2), samples,
													List.of(new Iteration(1, 1, 0L, 5_000_000L, new AssertionError("failed"))),
													5_000_000L, List.of("the run was interrupted"));

		String summary = report.describe();
		assertAll(() -> assertTrue(summary.startsWith("SAIL load x.xml: 2 virtual users, 1 iterations (0 passed, 1 failed, 100.0%)"), summary),
					() -> assertTrue(summary.contains("A very long interaction name that goes on and on beyond s..."), summary),
					() -> assertTrue(summary.contains("  user 01: 0 passed, 1 failed"), summary),
					() -> assertTrue(summary.contains("  user 02: 0 passed, 0 failed"), summary),
					() -> assertTrue(summary.contains("interaction \"Save\" (1 time(s); first in user 1 iteration 1): Interaction \"Save\": no at the page"), summary),
					() -> assertTrue(summary.endsWith("\n  the run was interrupted"), summary));
	}

	@Test
	void aUserThatRanNoIterationFailsTheRun() {
		SailLoadReport report = new SailLoadReport("x", plan(2), List.of(), List.of(new Iteration(1, 1, 0L, 10L, null)), 10L, List.of());

		assertEquals(List.of("user 02 ran no iteration"), report.violations().stream().map(SailLoadReport.Violation::message).toList());
	}

	@Test
	void aCsvCellThatASpreadsheetWouldRunIsDefused() {
		assertAll(() -> assertEquals("\"'=HYPERLINK(\"\"x\"\")\"", SailLoadReport.csv("=HYPERLINK(\"x\")")),
					() -> assertEquals("\"'+1\"", SailLoadReport.csv("+1")),
					() -> assertEquals("\"'-1\"", SailLoadReport.csv("-1")),
					() -> assertEquals("\"'@SUM(A1)\"", SailLoadReport.csv("@SUM(A1)")),
					() -> assertEquals("\"Save\"", SailLoadReport.csv("Save")),
					() -> assertEquals("\"\"", SailLoadReport.csv("")));
	}

	@Test
	void passwordsInFailuresAndProblemsAreHidden() {
		String failure = "Interaction \"Login\" step 1 <login password=\"secret\" user=\"admin\" />: refused";
		SailLoadReport report = new SailLoadReport("x", plan(1),
													List.of(new Sample("Login", 1, 1, 0L, 10L, true, failure)),
													List.of(new Iteration(1, 1, 0L, 10L, new AssertionError(failure))),
													10L, List.of("user 01 stopped: <login password=\"secret\"/>"));

		String summary = report.describe();
		assertAll(() -> assertTrue(summary.contains("<login password=\"********\" user=\"admin\" />"), summary),
					() -> assertTrue(summary.contains("user 01 stopped: <login password=\"********\"/>"), summary),
					() -> assertFalse(summary.contains("secret"), summary));
	}

	private static Plan plan(int users) {
		return new Plan(users, Duration.ZERO, null, 1, null, 0.0, null, Duration.ofSeconds(30));
	}
}
