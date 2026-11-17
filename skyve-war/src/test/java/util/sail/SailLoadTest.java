package util.sail;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.LongSupplier;
import java.util.stream.Stream;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.opentest4j.MultipleFailuresError;

import util.sail.SailLoad.Plan;
import util.sail.SailLoadReport.FailureKind;
import util.sail.SailLoadReport.InteractionStats;
import util.sail.SailLoadReport.Iteration;
import util.sail.SailLoadReport.Sample;
import util.sail.SailLoadReport.UserStats;

/**
 * Tests {@link SailLoad}'s scheduling, settings, summary and verdict with fake virtual users, a fake clock and a fake
 * scheduler: no browser starts and no real time passes.
 */
@SuppressWarnings({"static-method", "java:S5960"}) // test-only assertions
class SailLoadTest {
	@TempDir
	Path folder;

	/** A clock that moves only when a test moves it. */
	static final class FakeClock implements LongSupplier {
		long nanos = 1_000_000_000L;

		@Override
		public long getAsLong() {
			return nanos;
		}

		void advance(Duration duration) {
			nanos += duration.toNanos();
		}
	}

	/**
	 * Runs the scheduled users one after another on the test's thread, each from its own start time. The users are
	 * independent, so each user's timeline is what it would be with them all at once; the clock ends at the latest end.
	 */
	static class FakeScheduler implements SailLoad.Scheduler {
		private final FakeClock clock;
		private final long start;
		final List<Duration> delays = new ArrayList<>();
		private final List<Runnable> users = new ArrayList<>();
		/** False to act as if a user never ends, so that the time limit passes. */
		boolean ends = true;
		/** False to act as if an interrupted user does not end either. */
		boolean stops = true;
		Duration awaited;
		Duration stopLimit;

		FakeScheduler(FakeClock clock) {
			this.clock = clock;
			start = clock.getAsLong();
		}

		@Override
		public void schedule(Runnable user, Duration delay) {
			delays.add(delay);
			users.add(user);
		}

		@Override
		public boolean awaitEnd(Duration limit) {
			awaited = limit;
			long latest = start;
			List<Integer> order = new ArrayList<>();
			for (int i = 0; i < users.size(); i++) {
				order.add(Integer.valueOf(i));
			}
			order.sort(Comparator.comparing(delays::get));
			for (Integer i : order) {
				clock.nanos = start + delays.get(i.intValue()).toNanos();
				users.get(i.intValue()).run();
				latest = Math.max(latest, clock.nanos);
			}
			clock.nanos = latest;
			return ends;
		}

		@Override
		public boolean stop(Duration limit) {
			stopLimit = limit;
			return stops;
		}
	}

	private final FakeClock clock = new FakeClock();
	private FakeScheduler scheduler;

	/** A load run with the fakes, hermetic settings (no -D, environment or properties file) and the report in a temporary folder. */
	private SailLoad fake(SailLoad load) {
		return fake(load, Map.of(), Map.of());
	}

	private SailLoad fake(SailLoad load, Map<String, String> systemProperties, Map<String, String> environment) {
		return load.clock(clock)
					.scheduler(users -> scheduler = new FakeScheduler(clock))
					.environment(new SailSettings.Environment(systemProperties, environment, null, folder))
					.reportDirectory(folder.resolve("reports"))
					.withoutUserPersistence();
	}

	/** A task that takes a while and notes each user's iterations. */
	private SailLoad.VirtualUserTask taking(Duration each, Map<Integer, List<Integer>> iterations) {
		return user -> {
			iterations.computeIfAbsent(Integer.valueOf(user.number()), k -> new ArrayList<>()).add(Integer.valueOf(user.iteration()));
			clock.advance(each);
		};
	}

	@Test
	void theUsersStartSpreadEvenlyOverTheRampUp() {
		Map<Integer, List<Integer>> iterations = new HashMap<>();

		fake(SailLoad.users(4).rampUp(Duration.ofSeconds(40))).run(taking(Duration.ofSeconds(1), iterations));

		assertAll(() -> assertEquals(List.of(Duration.ZERO, Duration.ofSeconds(10), Duration.ofSeconds(20), Duration.ofSeconds(30)),
										scheduler.delays),
					() -> assertEquals(4, iterations.size(), "every user ran"));
	}

	@Test
	void withNoRampUpTheUsersStartTogether() {
		fake(SailLoad.users(3)).run(taking(Duration.ofSeconds(1), new HashMap<>()));

		assertEquals(List.of(Duration.ZERO, Duration.ZERO, Duration.ZERO), scheduler.delays);
	}

	@Test
	void eachUserRunsItsIterations() {
		Map<Integer, List<Integer>> iterations = new HashMap<>();

		SailLoadReport report = fake(SailLoad.users(3).iterations(2)).execute("test", taking(Duration.ofSeconds(5), iterations));

		assertAll(() -> assertEquals(Map.of(Integer.valueOf(1), List.of(Integer.valueOf(1), Integer.valueOf(2)),
											Integer.valueOf(2), List.of(Integer.valueOf(1), Integer.valueOf(2)),
											Integer.valueOf(3), List.of(Integer.valueOf(1), Integer.valueOf(2))),
										iterations),
					() -> assertEquals(6, report.iterationCount()),
					() -> assertNull(scheduler.awaited, "no time limit without a duration"),
					() -> assertTrue(report.violations().isEmpty(), report.violations().toString()));
	}

	@Test
	void withNeitherADurationNorIterationsEachUserRunsOnce() {
		Map<Integer, List<Integer>> iterations = new HashMap<>();

		fake(SailLoad.users(2)).run(taking(Duration.ofSeconds(5), iterations));

		assertEquals(Map.of(Integer.valueOf(1), List.of(Integer.valueOf(1)), Integer.valueOf(2), List.of(Integer.valueOf(1))), iterations);
	}

	@Test
	void theDurationStopsNewIterationsAndLetsTheOnesInFlightFinish() {
		Map<Integer, List<Integer>> iterations = new HashMap<>();

		// user 1 starts at 0s: iterations at 0, 25 and 50s (the last ends at 75s, after the end); user 2 at 10s: 10, 35, 60s is the end
		SailLoadReport report = fake(SailLoad.users(2).rampUp(Duration.ofSeconds(20)).forDuration(Duration.ofSeconds(60)))
									.execute("test", taking(Duration.ofSeconds(25), iterations));

		assertAll(() -> assertEquals(List.of(Integer.valueOf(1), Integer.valueOf(2), Integer.valueOf(3)), iterations.get(Integer.valueOf(1))),
					() -> assertEquals(List.of(Integer.valueOf(1), Integer.valueOf(2)), iterations.get(Integer.valueOf(2))),
					() -> assertEquals(Duration.ofSeconds(60).plus(SailLoad.DEFAULT_FINISH), scheduler.awaited,
										"the default time limit is the duration and 5 minutes"),
					() -> assertEquals(5, report.iterationCount()));
	}

	@Test
	void aDurationAndIterationsStopAtWhicheverComesFirst() {
		Map<Integer, List<Integer>> iterations = new HashMap<>();

		fake(SailLoad.users(1).forDuration(Duration.ofSeconds(60)).iterations(2)).run(taking(Duration.ofSeconds(10), iterations));
		fake(SailLoad.users(1).forDuration(Duration.ofSeconds(30)).iterations(5)).run(taking(Duration.ofSeconds(20), iterations));

		assertEquals(List.of(Integer.valueOf(1), Integer.valueOf(2), Integer.valueOf(1), Integer.valueOf(2)), iterations.get(Integer.valueOf(1)));
	}

	@Test
	void theSummaryHasEachInteractionsCountFailuresAndDurations() {
		// user 1's "Save" takes 1, 2, ... 10 s and user 2's 11 ... 20 s; "Open" always takes 1 s and fails once
		SailLoadReport report = fake(SailLoad.users(2).iterations(10).maxFailureRate(100)).execute("test", user -> {
			int seconds = ((user.number() - 1) * 10) + user.iteration();
			user.measure("Open", () -> {
				clock.advance(Duration.ofSeconds(1));
				if ((user.number() == 2) && (user.iteration() == 3)) {
					throw new IllegalStateException("not found");
				}
			});
			user.measure("Save", () -> clock.advance(Duration.ofSeconds(seconds)));
		});

		List<InteractionStats> interactions = report.interactions();
		assertAll(() -> assertEquals(List.of("Open", "Save"), interactions.stream().map(InteractionStats::name).toList(), "in the order they first ran"),
					() -> assertEquals(new InteractionStats("Open", 20, 1, 1_000L, 1_000L, 1_000L), interactions.get(0)),
					// user 2's iteration 3 stopped at Open, so Save has 19 samples: 1..12, 14..20 s
					() -> assertEquals(new InteractionStats("Save", 19, 0, 10_000L, 20_000L, 20_000L), interactions.get(1)),
					() -> assertEquals(List.of(new UserStats(1, 10, 0), new UserStats(2, 9, 1)), report.users()),
					() -> assertEquals(5.0, report.failureRate(), 1e-9));
	}

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
	void throughputIsOverTheRunsElapsedTime() {
		// 2 users x 3 iterations of 10 s, together: 30 s for 6 iterations with 2 interactions each
		SailLoadReport report = fake(SailLoad.users(2).iterations(3)).execute("test", user -> {
			user.measure("A", () -> clock.advance(Duration.ofSeconds(4)));
			user.measure("B", () -> clock.advance(Duration.ofSeconds(6)));
		});

		assertAll(() -> assertEquals(12.0, report.iterationsPerMinute(), 1e-9),
					() -> assertEquals(0.4, report.interactionsPerSecond(), 1e-9),
					() -> assertTrue(report.headline().contains("2 virtual users, 6 iterations (6 passed, 0 failed, 0.0%) in 0m 30s"), report.headline()));
	}

	@Test
	void theExecutorsInteractionsReachTheSummaryThroughTheProbe() {
		List<String> labels = new ArrayList<>();

		SailLoadReport report = fake(SailLoad.users(1).iterations(2)).execute("test", user -> {
			SailLoadProbe probe = SailLoadProbe.current();
			assertNotNull(probe, "a probe while an iteration runs");
			labels.add(SailLoadProbe.fileLabel());
			// as InterpretedWebDriverExecutor.executeInteraction reports an interaction
			long started = probe.now();
			clock.advance(Duration.ofMillis(1500));
			probe.interaction("Widgets", started, user.iteration() == 2, (user.iteration() == 2) ? "Interaction \"Widgets\" step 1: no" : null);
			if (user.iteration() == 2) {
				throw new AssertionError("SAIL workflow: x.xml failed");
			}
		});

		assertAll(() -> assertEquals(List.of("user01-it001-", "user01-it002-"), labels),
					() -> assertNull(SailLoadProbe.current(), "no probe after the run"),
					() -> assertEquals("", SailLoadProbe.fileLabel(), "no label outside a load run"),
					() -> assertEquals(new InteractionStats("Widgets", 2, 1, 1_500L, 1_500L, 1_500L), report.interactions().get(0)),
					() -> assertEquals(List.of(new FailureKind("interaction \"Widgets\"", 1, 1, 2, "Interaction \"Widgets\" step 1: no",
																report.failureKinds().get(0).cause())),
										report.failureKinds()),
					() -> assertTrue(report.failureKinds().get(0).cause() instanceof AssertionError, "the iteration's failure is the cause"));
	}

	@Test
	void anyFailedIterationFailsTheTestWithTheFirstFailureOfEachKind() {
		SailLoad load = fake(SailLoad.users(2).iterations(2));

		MultipleFailuresError e = assertThrows(MultipleFailuresError.class, () -> load.run(user -> {
			if (user.number() == 2) {
				throw new IllegalStateException("could not start the browser in iteration " + user.iteration());
			}
		}));

		List<String> messages = e.getFailures().stream().map(Throwable::getMessage).toList();
		assertAll(() -> assertTrue(e.getMessage().startsWith("SAIL load task: 2 virtual users, 4 iterations (2 passed, 2 failed, 50.0%)"), e.getMessage()),
					() -> assertEquals(List.of("2 of 4 iterations failed (50.0%), above the sail.load.maxFailureRate of 0%",
												"outside an interaction (IllegalStateException) failed 2 time(s); first in user 2 iteration 1: " +
													"could not start the browser in iteration 1"),
										messages),
					() -> assertTrue(e.getFailures().get(1).getCause() instanceof IllegalStateException, "with the first failure as the cause"));
	}

	@Test
	void aFailureRateWithinTheThresholdPassesAndAboveItFails() {
		SailLoad.VirtualUserTask oneInFourFails = user -> {
			if ((user.number() == 1) && (user.iteration() == 1)) {
				throw new AssertionError("flaky");
			}
		};

		// green: 25% failed, 25% allowed
		fake(SailLoad.users(2).iterations(2).maxFailureRate(25)).run(oneInFourFails);
		// red: 25% failed, 20% allowed
		SailLoad strict = fake(SailLoad.users(2).iterations(2).maxFailureRate(20));
		MultipleFailuresError e = assertThrows(MultipleFailuresError.class, () -> strict.run(oneInFourFails));

		assertEquals("1 of 4 iterations failed (25.0%), above the sail.load.maxFailureRate of 20%", e.getFailures().get(0).getMessage());
	}

	@Test
	void anInteractionsP95AboveTheThresholdFailsTheTest() {
		SailLoad.VirtualUserTask slowSave = user -> {
			user.measure("Open", () -> clock.advance(Duration.ofMillis(200)));
			user.measure("Save", () -> clock.advance(Duration.ofMillis(user.iteration() * 1000L)));
		};

		// green: Save's p95 of 20 iterations (1..20 s) is 19 s
		fake(SailLoad.users(1).iterations(20).maxP95(Duration.ofSeconds(19))).run(slowSave);
		// red: 18 s
		SailLoad strict = fake(SailLoad.users(1).iterations(20).maxP95(Duration.ofSeconds(18)));
		MultipleFailuresError e = assertThrows(MultipleFailuresError.class, () -> strict.run(slowSave));

		assertEquals(List.of("interaction \"Save\": 95th percentile 19000 ms is above the sail.load.maxP95 of 18000 ms"),
						e.getFailures().stream().map(Throwable::getMessage).toList());
	}

	@Test
	void theTimeLimitInterruptsTheUsersAndFailsTheRun() {
		SailLoad load = fake(SailLoad.users(1).forDuration(Duration.ofSeconds(10)).timeLimit(Duration.ofSeconds(20)));
		load.scheduler(users -> {
			scheduler = new FakeScheduler(clock);
			scheduler.ends = false;
			scheduler.stops = false;
			return scheduler;
		});

		MultipleFailuresError e = assertThrows(MultipleFailuresError.class, () -> load.run(user -> clock.advance(Duration.ofSeconds(1))));

		assertAll(() -> assertEquals(Duration.ofSeconds(20), scheduler.awaited),
					() -> assertEquals(Duration.ofSeconds(SailSettings.DEFAULT_TIMEOUT_SECONDS), scheduler.stopLimit, "interrupted users get the settings' timeout to end"),
					() -> assertEquals(List.of("the time limit of PT20S (sail.load.timeLimit) passed: the users still running were interrupted",
												"some users did not stop within PT30S of being interrupted: their browsers may still be open"),
										e.getFailures().stream().map(Throwable::getMessage).toList()));
	}

	@Test
	void anInterruptedIterationFailsAndTheUserStops() {
		Map<Integer, List<Integer>> iterations = new HashMap<>();
		try {
			SailLoadReport report = fake(SailLoad.users(1).iterations(3)).execute("test", user -> {
				iterations.computeIfAbsent(Integer.valueOf(user.number()), k -> new ArrayList<>()).add(Integer.valueOf(user.iteration()));
				throw new InterruptedException("stopped");
			});

			assertAll(() -> assertEquals(Map.of(Integer.valueOf(1), List.of(Integer.valueOf(1))), iterations, "no iteration after the interrupt"),
						() -> assertEquals(1, report.failedIterations()));
		}
		finally {
			assertTrue(Thread.interrupted(), "the interrupt is kept"); // and cleared, as the fake user ran on this thread
		}
	}

	@Test
	void aUserThatStopsOutsideAnIterationIsAProblemOfTheRun() {
		// the users' clock breaks while they run (as a user's persistence or engine could fail to be made)
		boolean[] broken = {false};
		LongSupplier breaking = () -> {
			if (broken[0]) {
				throw new IllegalStateException("broken");
			}
			return clock.getAsLong();
		};
		SailLoad load = fake(SailLoad.users(1).forDuration(Duration.ofSeconds(60))).clock(breaking);
		load.scheduler(users -> new FakeScheduler(clock) {
			@Override
			public boolean awaitEnd(Duration limit) {
				broken[0] = true;
				try {
					return super.awaitEnd(limit);
				}
				finally {
					broken[0] = false;
				}
			}
		});

		MultipleFailuresError e = assertThrows(MultipleFailuresError.class, () -> load.run(user -> clock.advance(Duration.ofSeconds(1))));

		assertEquals(List.of("no iteration ran", "user 01 stopped: java.lang.IllegalStateException: broken"),
						e.getFailures().stream().map(Throwable::getMessage).toList());
	}

	@Test
	void noIterationFailsTheRun() {
		SailLoadReport report = new SailLoadReport("x", plan(1), List.of(), List.of(), 0L, List.of());

		assertAll(() -> assertEquals("no iteration ran", report.violations().get(0).message()),
					() -> assertEquals(0.0, report.failureRate()),
					() -> assertEquals(0.0, report.iterationsPerMinute()));
	}

	@Test
	void eachUserThreadsTraceStartsWithTheUser() {
		PrintStream original = System.out;
		ByteArrayOutputStream captured = new ByteArrayOutputStream();
		try (PrintStream capture = new PrintStream(captured, true, StandardCharsets.UTF_8)) {
			System.setOut(capture);
			fake(SailLoad.users(2)).run(user -> System.out.println("Interaction: Widgets"));
		}
		finally {
			System.setOut(original);
		}

		String trace = captured.toString(StandardCharsets.UTF_8);
		assertAll(() -> assertTrue(trace.contains("[user 01] Interaction: Widgets"), trace),
					() -> assertTrue(trace.contains("[user 02] SAIL load: iteration 1 passed in 0.0 s"), trace),
					() -> assertTrue(trace.contains("\nSAIL load task: 2 virtual users"), "the summary is not prefixed: " + trace),
					() -> assertSame(original, System.out, "standard output is put back"));
	}

	@Test
	void theReportIsWrittenAsHtmlAndCsv() throws IOException {
		fake(SailLoad.users(1).iterations(2)).run(user -> user.measure("Save, with \"quotes\"", () -> clock.advance(Duration.ofSeconds(2))));

		Path report;
		try (Stream<Path> runs = Files.list(folder.resolve("reports"))) {
			report = runs.findFirst().orElseThrow();
		}
		String interactions = Files.readString(report.resolve("interactions.csv"));
		String samples = Files.readString(report.resolve("samples.csv"));
		String iterations = Files.readString(report.resolve("iterations.csv"));
		String html = Files.readString(report.resolve("index.html"));
		assertAll(() -> assertTrue(report.getFileName().toString().endsWith("-task"), report.toString()),
					() -> assertEquals("interaction,count,failed,median_ms,p95_ms,max_ms\n\"Save, with \"\"quotes\"\"\",2,0,2000,2000,2000\n", interactions),
					() -> assertEquals("user,iteration,interaction,start_ms,duration_ms,failed,failure\n" +
										"1,1,\"Save, with \"\"quotes\"\"\",0,2000,false,\"\"\n" +
										"1,2,\"Save, with \"\"quotes\"\"\",2000,2000,false,\"\"\n", samples),
					() -> assertEquals("user,iteration,start_ms,duration_ms,failed,failure\n1,1,0,2000,false,\"\"\n1,2,2000,2000,false,\"\"\n", iterations),
					() -> assertTrue(html.contains("<td>Save, with &quot;quotes&quot;</td>"), html),
					() -> assertTrue(html.contains("Passed."), html));
	}

	@Test
	void theSettingsBeatTheCodeAsEverySailSettingDoes() {
		Map<Integer, List<Integer>> iterations = new HashMap<>();

		fake(SailLoad.users(1).iterations(1), Map.of(SailSettings.LOAD_USERS, "3"), Map.of("SAIL_LOAD_ITERATIONS", "2"))
			.run(taking(Duration.ofSeconds(1), iterations));

		assertAll(() -> assertEquals(3, iterations.size(), "-Dsail.load.users beats users(1)"),
					() -> assertEquals(List.of(Integer.valueOf(1), Integer.valueOf(2)), iterations.get(Integer.valueOf(3)),
										"SAIL_LOAD_ITERATIONS beats iterations(1)"));
	}

	@Test
	void theLoadSettingsResolveWithTheirSourcesAndDurationFormats() {
		SailSettings settings = resolve(Map.of(SailSettings.LOAD_RAMP_UP, "30s", SailSettings.LOAD_DURATION, "5m",
												SailSettings.LOAD_TIME_LIMIT, "PT1H30M", SailSettings.LOAD_MAX_P95, "1500ms",
												SailSettings.LOAD_MAX_FAILURE_RATE, "2.5"),
										Map.of("SAIL_LOAD_USERS", "10", "SAIL_LOAD_ITERATIONS", "4"));

		assertAll(() -> assertEquals(10, settings.loadUsers()),
					() -> assertEquals("load.users=10 (SAIL_LOAD_USERS)", String.valueOf(settings.setting(SailSettings.LOAD_USERS))),
					() -> assertEquals(Duration.ofSeconds(30), settings.loadRampUp()),
					() -> assertEquals(Duration.ofMinutes(5), settings.loadDuration()),
					() -> assertEquals(4, settings.loadIterations()),
					() -> assertEquals(Duration.ofMinutes(90), settings.loadTimeLimit()),
					() -> assertEquals(Duration.ofMillis(1500), settings.loadMaxP95()),
					() -> assertEquals(2.5, settings.loadMaxFailureRate()),
					() -> assertEquals(Duration.ofHours(2), resolve(Map.of(SailSettings.LOAD_DURATION, "2h"), Map.of()).loadDuration()),
					() -> assertEquals(Duration.ofMinutes(2), resolve(Map.of(SailSettings.LOAD_DURATION, "2 M"), Map.of()).loadDuration()),
					() -> assertEquals(Duration.ZERO, resolve(Map.of(SailSettings.LOAD_RAMP_UP, "0s"), Map.of()).loadRampUp()),
					() -> assertEquals("SAIL_LOAD_MAX_FAILURE_RATE", SailSettings.environmentName(SailSettings.LOAD_MAX_FAILURE_RATE)),
					() -> assertEquals("SAIL_LOAD_RAMP_UP", SailSettings.environmentName(SailSettings.LOAD_RAMP_UP)),
					() -> assertEquals("SAIL_LOAD_MAX_P95", SailSettings.environmentName(SailSettings.LOAD_MAX_P95)));
	}

	@Test
	void theLoadSettingsHaveDefaults() {
		SailSettings settings = resolve(Map.of(), Map.of());

		assertAll(() -> assertEquals(0, settings.loadUsers()),
					() -> assertEquals(Duration.ZERO, settings.loadRampUp()),
					() -> assertNull(settings.loadDuration()),
					() -> assertEquals(0, settings.loadIterations()),
					() -> assertNull(settings.loadTimeLimit()),
					() -> assertEquals(0.0, settings.loadMaxFailureRate()),
					() -> assertNull(settings.loadMaxP95()));
	}

	@Test
	void invalidLoadSettingsFailFastNamingEachKeySourceAndTheValidValues() {
		Map<String, String> invalid = Map.of(SailSettings.LOAD_USERS, "0", SailSettings.LOAD_ITERATIONS, "many",
												SailSettings.LOAD_RAMP_UP, "-5s", SailSettings.LOAD_DURATION, "0s",
												SailSettings.LOAD_TIME_LIMIT, "soon", SailSettings.LOAD_MAX_FAILURE_RATE, "101",
												SailSettings.LOAD_MAX_P95, "fast");

		Map<String, String> none = Map.of();
		IllegalArgumentException e = assertThrows(IllegalArgumentException.class, () -> resolve(invalid, none));

		String message = e.getMessage();
		assertAll(() -> assertTrue(message.contains("sail.load.users=0 (-Dsail.load.users): use a whole number above 0"), message),
					() -> assertTrue(message.contains("sail.load.iterations=many (-Dsail.load.iterations): use a whole number above 0"), message),
					() -> assertTrue(message.contains("sail.load.rampUp=-5s (-Dsail.load.rampUp): use a duration of 0 or more such as 30s, 5m, 1h, 500ms or PT1M30S"), message),
					() -> assertTrue(message.contains("sail.load.duration=0s (-Dsail.load.duration): use a duration above 0"), message),
					() -> assertTrue(message.contains("sail.load.timeLimit=soon (-Dsail.load.timeLimit): use a duration above 0"), message),
					() -> assertTrue(message.contains("sail.load.maxFailureRate=101 (-Dsail.load.maxFailureRate): use a percentage from 0 to 100"), message),
					() -> assertTrue(message.contains("sail.load.maxP95=fast (-Dsail.load.maxP95): use a duration above 0"), message));
	}

	@Test
	void anInvalidSettingFailsBeforeAnyUserStarts() {
		SailLoad load = fake(SailLoad.users(2), Map.of(SailSettings.LOAD_DURATION, "forever"), Map.of());

		assertThrows(IllegalArgumentException.class, () -> load.run(user -> clock.advance(Duration.ofSeconds(1))));
		assertNull(scheduler, "no scheduler was made");
	}

	@Test
	void thePlanNeedsUsersARampUpShorterThanTheDurationAndALongerTimeLimit() {
		SailSettings noUsersSet = resolve(Map.of(), Map.of());
		SailSettings longRampUp = resolve(Map.of(SailSettings.LOAD_USERS, "2", SailSettings.LOAD_RAMP_UP, "1m", SailSettings.LOAD_DURATION, "60s"),
											Map.of());
		SailSettings shortLimit = resolve(Map.of(SailSettings.LOAD_USERS, "2", SailSettings.LOAD_DURATION, "2m", SailSettings.LOAD_TIME_LIMIT, "120s"),
											Map.of());

		IllegalArgumentException noUsers = assertThrows(IllegalArgumentException.class, () -> Plan.of(noUsersSet));
		IllegalArgumentException rampUp = assertThrows(IllegalArgumentException.class, () -> Plan.of(longRampUp));
		IllegalArgumentException timeLimit = assertThrows(IllegalArgumentException.class, () -> Plan.of(shortLimit));

		assertAll(() -> assertEquals("SAIL load: set the number of virtual users, with SailLoad.users(n), -Dsail.load.users or SAIL_LOAD_USERS",
										noUsers.getMessage()),
					() -> assertEquals("SAIL load: the ramp-up 1m (-Dsail.load.rampUp) must be shorter than the duration 60s (-Dsail.load.duration), which includes it",
										rampUp.getMessage()),
					() -> assertEquals("SAIL load: the time limit 120s (-Dsail.load.timeLimit) must be longer than the duration 2m (-Dsail.load.duration)",
										timeLimit.getMessage()));
	}

	@Test
	void thePlanDescribesItself() {
		Plan plan = Plan.of(resolve(Map.of(SailSettings.LOAD_USERS, "5", SailSettings.LOAD_RAMP_UP, "20s", SailSettings.LOAD_DURATION, "2m",
											SailSettings.LOAD_MAX_FAILURE_RATE, "1", SailSettings.LOAD_MAX_P95, "3s"), Map.of()));

		assertEquals("5 virtual users, ramp-up PT20S, duration PT2M, time limit PT7M, max failure rate 1.0%, max p95 PT3S", plan.toString());
	}

	@Test
	void aSuiteWithABracketIsRefusedUnlessAllowed() {
		SailLoad load = fake(SailLoad.users(2));

		IllegalArgumentException e = assertThrows(IllegalArgumentException.class, () -> load.run("sail/admin/admin.suite.xml"));

		assertAll(() -> assertTrue(e.getMessage().startsWith("SAIL load: the suite sail/admin/admin.suite.xml has a bracket"), e.getMessage()),
					() -> assertTrue(e.getMessage().contains("allowSuiteBracket()"), e.getMessage()),
					() -> assertNull(scheduler, "refused before any user starts"));
	}

	@Test
	void aSuiteWithoutABracketAWorkflowAndAnAllowedBracketAreAccepted() {
		IllegalStateException started = new IllegalStateException("the users would start now");
		SailLoad.Scheduler refusing = new SailLoad.Scheduler() {
			@Override
			public void schedule(Runnable user, Duration delay) {
				throw started;
			}

			@Override
			public boolean awaitEnd(Duration limit) {
				return true;
			}

			@Override
			public boolean stop(Duration limit) {
				return true;
			}
		};

		SailLoad plain = fake(SailLoad.users(1)).scheduler(users -> refusing);
		SailLoad allowing = fake(SailLoad.users(1)).allowSuiteBracket().scheduler(users -> refusing);

		assertAll(() -> assertSame(started, assertThrows(IllegalStateException.class,
															() -> plain.run("sail/kitchenSink/kitchensink.suite.xml",
																			"sail/kitchenSink/workflow/kitchensink-workflow.xml"))),
					() -> assertSame(started, assertThrows(IllegalStateException.class, () -> allowing.run("sail/admin/admin.suite.xml"))));
	}

	@Test
	void aMissingFileIsRefused() {
		SailLoad load = fake(SailLoad.users(1));

		IllegalArgumentException missing = assertThrows(IllegalArgumentException.class, () -> load.run("sail/no-such.xml"));
		IllegalArgumentException none = assertThrows(IllegalArgumentException.class, () -> load.run());

		assertAll(() -> assertEquals("SAIL load: sail/no-such.xml is neither a file nor a resource on the test classpath", missing.getMessage()),
					() -> assertEquals("SAIL load: name at least one SAIL file to run", none.getMessage()));
	}

	@Test
	void viewRolesAreCheckedAsTheyAreGivenAndATestsSettingsReplaceItsOwn() {
		SailLoad load = SailLoad.users(1);
		SailSettings.Code own = load.settings();
		SailSettings.Code tests = SailSettings.code();

		assertAll(() -> assertThrows(IllegalArgumentException.class, () -> load.viewRoles("jane")),
					() -> assertSame(load, load.viewRoles("jane{run}", "admin.BasicUser")),
					() -> assertSame(tests, load.settings(tests).settings()),
					() -> assertNotSame(own, load.settings()));
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

	private static Plan plan(int users) {
		return new Plan(users, Duration.ZERO, null, 1, null, 0.0, null, Duration.ofSeconds(30));
	}

	private SailSettings resolve(Map<String, String> systemProperties, Map<String, String> environment) {
		return SailSettings.resolve(new SailSettings.Environment(systemProperties, environment, null, folder), List.of());
	}
}
