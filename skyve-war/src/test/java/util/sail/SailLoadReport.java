package util.sail;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.TimeUnit;

import org.opentest4j.MultipleFailuresError;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * What a load run ({@link SailLoad}) did: every interaction and iteration of every virtual user, and the statistics the
 * summary shows. Percentiles are nearest-rank (the smallest duration that at least that share of the samples do not
 * exceed), over every sample of the interaction, failed ones included.
 */
final class SailLoadReport {
	/** How many characters of a failure message the summary shows. */
	private static final int MESSAGE_LENGTH = 400;

	/**
	 * One interaction of one iteration.
	 *
	 * @param interaction	its name
	 * @param user	the virtual user, from 1
	 * @param iteration	the user's iteration, from 1
	 * @param startNanos	when it started, from the run's clock
	 * @param endNanos	when it ended
	 * @param failed	true if a step threw or a check failed
	 * @param failure	what failed, or null if it passed or the reason is not known
	 */
	record Sample(@Nonnull String interaction,
					int user,
					int iteration,
					long startNanos,
					long endNanos,
					boolean failed,
					@Nullable String failure) {
		/** Returns how long it took, in milliseconds. */
		long millis() {
			return TimeUnit.NANOSECONDS.toMillis(endNanos - startNanos);
		}
	}

	/**
	 * One iteration of one virtual user.
	 *
	 * @param user	the virtual user, from 1
	 * @param number	the user's iteration, from 1
	 * @param startNanos	when it started, from the run's clock
	 * @param endNanos	when it ended
	 * @param failure	what it threw, or null if it passed
	 */
	record Iteration(int user, int number, long startNanos, long endNanos, @Nullable Throwable failure) {
		/** Indicates if it failed. */
		boolean failed() {
			return failure != null;
		}
	}

	/**
	 * The statistics of one interaction, by name, across every user and iteration.
	 *
	 * @param name	the interaction's name
	 * @param count	how many times it ran
	 * @param failures	how many of those failed
	 * @param medianMillis	the median duration
	 * @param p95Millis	the 95th percentile duration
	 * @param maxMillis	the longest duration
	 */
	record InteractionStats(@Nonnull String name, int count, int failures, long medianMillis, long p95Millis, long maxMillis) {
		// a value
	}

	/**
	 * What one virtual user's iterations did.
	 *
	 * @param user	the virtual user, from 1
	 * @param passed	its iterations that passed
	 * @param failed	its iterations that failed
	 */
	record UserStats(int user, int passed, int failed) {
		// a value
	}

	/**
	 * A kind of failure, with its first occurrence: a failed interaction (by name), or an iteration that failed outside
	 * any interaction (by the class of what it threw).
	 *
	 * @param kind	e.g. {@code interaction "Save"}
	 * @param count	how many times it happened
	 * @param user	the user of the first occurrence
	 * @param iteration	the iteration of the first occurrence
	 * @param message	what the first occurrence said
	 * @param cause	what the first occurrence's iteration threw, or null if it was not thrown (Verify)
	 */
	record FailureKind(@Nonnull String kind, int count, int user, int iteration, @Nonnull String message, @Nullable Throwable cause) {
		// a value
	}

	/**
	 * Collects the samples and iterations from every virtual user's thread. Thread-safe.
	 */
	static final class Recorder {
		private final List<Sample> samples = new ArrayList<>();
		private final List<Iteration> iterations = new ArrayList<>();
		private final List<String> problems = new ArrayList<>();

		/**
		 * Adds an interaction's sample.
		 *
		 * @param sample	the sample
		 */
		synchronized void sample(@Nonnull Sample sample) {
			samples.add(sample);
		}

		/**
		 * Adds an ended iteration.
		 *
		 * @param iteration	the iteration
		 */
		synchronized void iteration(@Nonnull Iteration iteration) {
			iterations.add(iteration);
		}

		/**
		 * Adds a problem of a virtual user that is not an iteration's failure, such as a user that could not start.
		 *
		 * @param problem	what went wrong, naming the user
		 */
		synchronized void problem(@Nonnull String problem) {
			problems.add(problem);
		}

		/** Returns a copy of the problems so far. */
		synchronized @Nonnull List<String> problems() {
			return new ArrayList<>(problems);
		}

		/** Returns a copy of the samples so far. */
		synchronized @Nonnull List<Sample> samples() {
			return new ArrayList<>(samples);
		}

		/** Returns a copy of the iterations so far. */
		synchronized @Nonnull List<Iteration> iterations() {
			return new ArrayList<>(iterations);
		}
	}

	private final String name;
	private final SailLoad.Plan plan;
	private final List<Sample> samples;
	private final List<Iteration> iterations;
	private final long elapsedNanos;
	private final List<String> problems;
	private final List<InteractionStats> interactions;
	private final List<UserStats> users;
	private final List<FailureKind> failureKinds;
	private final int failedIterations;

	/**
	 * Makes the report of a run.
	 *
	 * @param name	what ran, for the heading
	 * @param plan	the run's plan, with its thresholds
	 * @param samples	every interaction's sample
	 * @param iterations	every ended iteration
	 * @param elapsedNanos	how long the run took, from its start until its users ended
	 * @param problems	what went wrong with the run itself (its time limit, users that would not stop), which fail it
	 */
	SailLoadReport(@Nonnull String name,
					@Nonnull SailLoad.Plan plan,
					@Nonnull List<Sample> samples,
					@Nonnull List<Iteration> iterations,
					long elapsedNanos,
					@Nonnull List<String> problems) {
		this.name = name;
		this.plan = plan;
		this.samples = new ArrayList<>(samples);
		this.samples.sort(Comparator.comparingLong(Sample::startNanos));
		this.iterations = new ArrayList<>(iterations);
		this.iterations.sort(Comparator.comparingLong(Iteration::startNanos));
		this.elapsedNanos = elapsedNanos;
		this.problems = problems.stream().map(SailMedia::hidePasswords).toList();
		interactions = interactionStats(this.samples);
		users = userStats(plan.users(), this.iterations);
		failureKinds = failureKinds(this.samples, this.iterations);
		int failed = 0;
		for (Iteration iteration : this.iterations) {
			if (iteration.failed()) {
				failed++;
			}
		}
		failedIterations = failed;
	}

	private static @Nonnull List<InteractionStats> interactionStats(@Nonnull List<Sample> samples) {
		// in the order the interactions first ran
		Map<String, List<Sample>> byName = new LinkedHashMap<>();
		for (Sample sample : samples) {
			byName.computeIfAbsent(sample.interaction(), k -> new ArrayList<>()).add(sample);
		}
		List<InteractionStats> result = new ArrayList<>(byName.size());
		for (Map.Entry<String, List<Sample>> entry : byName.entrySet()) {
			List<Sample> runs = entry.getValue();
			long[] millis = new long[runs.size()];
			int failures = 0;
			for (int i = 0; i < millis.length; i++) {
				Sample sample = runs.get(i);
				millis[i] = sample.millis();
				if (sample.failed()) {
					failures++;
				}
			}
			Arrays.sort(millis);
			result.add(new InteractionStats(entry.getKey(), millis.length, failures,
											percentile(millis, 50), percentile(millis, 95), millis[millis.length - 1]));
		}
		return result;
	}

	/**
	 * Returns a nearest-rank percentile: the smallest value that at least {@code percent}% of the values do not exceed.
	 *
	 * @param sorted	the values, sorted, at least one
	 * @param percent	from 1 to 100
	 * @return the value
	 */
	static long percentile(@Nonnull long[] sorted, int percent) {
		int rank = (int) Math.ceil((percent / 100.0) * sorted.length);
		return sorted[Math.max(rank, 1) - 1];
	}

	private static @Nonnull List<UserStats> userStats(int userCount, @Nonnull List<Iteration> iterations) {
		int[] passed = new int[userCount + 1];
		int[] failed = new int[userCount + 1];
		for (Iteration iteration : iterations) {
			if (iteration.failed()) {
				failed[iteration.user()]++;
			}
			else {
				passed[iteration.user()]++;
			}
		}
		List<UserStats> result = new ArrayList<>(userCount);
		for (int user = 1; user <= userCount; user++) {
			result.add(new UserStats(user, passed[user], failed[user]));
		}
		return result;
	}

	private static @Nonnull List<FailureKind> failureKinds(@Nonnull List<Sample> samples, @Nonnull List<Iteration> iterations) {
		Map<String, Throwable> thrown = new LinkedHashMap<>();
		for (Iteration iteration : iterations) {
			if (iteration.failure() != null) {
				thrown.put(iteration.user() + "/" + iteration.number(), iteration.failure());
			}
		}
		Map<String, FailureKind> kinds = new LinkedHashMap<>();
		Set<String> withFailedInteraction = new HashSet<>();
		for (Sample sample : samples) {
			if (sample.failed()) {
				String key = sample.user() + "/" + sample.iteration();
				withFailedInteraction.add(key);
				Throwable cause = thrown.get(key);
				String failure = sample.failure();
				String message = (failure != null) ? failure : ((cause == null) ? "(no message)" : message(cause));
				add(kinds, "interaction \"" + sample.interaction() + '"', sample.user(), sample.iteration(), message, cause);
			}
		}
		for (Iteration iteration : iterations) {
			Throwable failure = iteration.failure();
			if ((failure != null) && (! withFailedInteraction.contains(iteration.user() + "/" + iteration.number()))) {
				add(kinds, "outside an interaction (" + rootCause(failure).getClass().getSimpleName() + ')',
						iteration.user(), iteration.number(), message(failure), failure);
			}
		}
		return new ArrayList<>(kinds.values());
	}

	private static void add(@Nonnull Map<String, FailureKind> kinds,
								@Nonnull String kind,
								int user,
								int iteration,
								@Nonnull String message,
								@Nullable Throwable cause) {
		FailureKind first = kinds.get(kind);
		kinds.put(kind, (first == null) ?
							new FailureKind(kind, 1, user, iteration, shorten(message), cause) :
							new FailureKind(kind, first.count() + 1, first.user(), first.iteration(), first.message(), first.cause()));
	}

	/**
	 * Returns what a failed iteration says: the first of several failures (a suite's members, an iteration's entries),
	 * else its own message.
	 */
	static @Nonnull String message(@Nonnull Throwable failure) {
		Throwable first = failure;
		while ((first instanceof MultipleFailuresError multiple) && (! multiple.getFailures().isEmpty())) {
			first = multiple.getFailures().get(0);
		}
		String result = first.getMessage();
		return (result == null) ? first.getClass().getName() : result;
	}

	private static @Nonnull Throwable rootCause(@Nonnull Throwable failure) {
		Throwable result = failure;
		while ((result instanceof MultipleFailuresError multiple) && (! multiple.getFailures().isEmpty())) {
			result = multiple.getFailures().get(0);
		}
		while ((result.getCause() != null) && (result.getCause() != result)) {
			result = result.getCause();
		}
		return result;
	}

	/** Puts a message on one line, at most {@value #MESSAGE_LENGTH} characters, with any login password hidden. */
	private static @Nonnull String shorten(@Nonnull String message) {
		String result = SailMedia.hidePasswords(message).replaceAll("\\s+", " ").trim();
		return (result.length() <= MESSAGE_LENGTH) ? result : result.substring(0, MESSAGE_LENGTH) + "...";
	}

	/** Returns the interactions' statistics, in the order they first ran. */
	@Nonnull List<InteractionStats> interactions() {
		return interactions;
	}

	/** Returns each virtual user's iterations, by user. */
	@Nonnull List<UserStats> users() {
		return users;
	}

	/** Returns each kind of failure, in the order they first happened. */
	@Nonnull List<FailureKind> failureKinds() {
		return failureKinds;
	}

	/** Returns how many iterations ended. */
	int iterationCount() {
		return iterations.size();
	}

	/** Returns how many iterations failed. */
	int failedIterations() {
		return failedIterations;
	}

	/** Returns the percentage of the iterations that failed, or 0 if none ran. */
	double failureRate() {
		return iterations.isEmpty() ? 0.0 : (failedIterations * 100.0) / iterations.size();
	}

	/** Returns the iterations that ended per minute of the run. */
	double iterationsPerMinute() {
		return perSecond(iterations.size()) * 60.0;
	}

	/** Returns the interactions that ended per second of the run. */
	double interactionsPerSecond() {
		return perSecond(samples.size());
	}

	private double perSecond(int count) {
		return (elapsedNanos <= 0L) ? 0.0 : (count * 1_000_000_000.0) / elapsedNanos;
	}

	/**
	 * Returns why the run fails, if it does: no iteration at all, or a user that ran none; the run's own problems (its
	 * time limit, users that would not stop or never started, a user's thread that failed); then
	 * its failed iterations unless the failure rate is within {@value SailSettings#LOAD_MAX_FAILURE_RATE} (each kind of
	 * failure, after the rate), then each interaction whose 95th percentile is above {@value SailSettings#LOAD_MAX_P95}.
	 *
	 * @return the reasons, each with the first failure's cause if there is one; empty if the run passes
	 */
	@Nonnull List<Violation> violations() {
		List<Violation> result = new ArrayList<>();
		if (iterations.isEmpty()) {
			result.add(new Violation("no iteration ran", null));
		}
		else {
			for (UserStats user : users) {
				if ((user.passed() + user.failed()) == 0) {
					result.add(new Violation(String.format("user %02d ran no iteration", Integer.valueOf(user.user())), null));
				}
			}
		}
		for (String problem : problems) {
			result.add(new Violation(problem, null));
		}
		if ((failedIterations > 0) && (failureRate() > plan.maxFailureRate())) {
			result.add(new Violation(String.format(Locale.ROOT, "%d of %d iterations failed (%.1f%%), above the %s of %s%%",
														Integer.valueOf(failedIterations), Integer.valueOf(iterations.size()),
														Double.valueOf(failureRate()), SailSettings.LOAD_MAX_FAILURE_RATE,
														number(plan.maxFailureRate())),
										null));
			for (FailureKind kind : failureKinds) {
				result.add(new Violation(String.format("%s failed %d time(s); first in user %d iteration %d: %s",
															kind.kind(), Integer.valueOf(kind.count()), Integer.valueOf(kind.user()),
															Integer.valueOf(kind.iteration()), kind.message()),
											kind.cause()));
			}
		}
		Duration maxP95 = plan.maxP95();
		if (maxP95 != null) {
			for (InteractionStats interaction : interactions) {
				if (interaction.p95Millis() > maxP95.toMillis()) {
					result.add(new Violation(String.format("interaction \"%s\": 95th percentile %d ms is above the %s of %d ms",
																interaction.name(), Long.valueOf(interaction.p95Millis()),
																SailSettings.LOAD_MAX_P95, Long.valueOf(maxP95.toMillis())),
												null));
				}
			}
		}
		return result;
	}

	/**
	 * A reason a run fails.
	 *
	 * @param message	what is wrong
	 * @param cause	what the first such failure threw, or null
	 */
	record Violation(@Nonnull String message, @Nullable Throwable cause) {
		// a value
	}

	/**
	 * Describes the run for the trace: the totals and throughput, each interaction's statistics, each user's iterations,
	 * the first failure of each kind, and the run's problems.
	 *
	 * @return the summary, over several lines
	 */
	@Nonnull String describe() {
		StringBuilder result = new StringBuilder(2048);
		result.append(headline());
		int width = 11;
		for (InteractionStats interaction : interactions) {
			width = Math.max(width, Math.min(interaction.name().length(), 60));
		}
		String row = "\n  %-" + width + "s %7s %7s %9s %9s %9s";
		result.append(String.format(row, "interaction", "count", "failed", "median ms", "p95 ms", "max ms"));
		for (InteractionStats interaction : interactions) {
			String shown = interaction.name();
			if (shown.length() > width) {
				shown = shown.substring(0, width - 3) + "...";
			}
			result.append(String.format(row, shown, Integer.valueOf(interaction.count()), Integer.valueOf(interaction.failures()),
										Long.valueOf(interaction.medianMillis()), Long.valueOf(interaction.p95Millis()),
										Long.valueOf(interaction.maxMillis())));
		}
		for (UserStats user : users) {
			result.append(String.format("\n  user %02d: %d passed, %d failed", Integer.valueOf(user.user()),
											Integer.valueOf(user.passed()), Integer.valueOf(user.failed())));
		}
		if (! failureKinds.isEmpty()) {
			result.append("\n  first failure of each kind:");
			for (FailureKind kind : failureKinds) {
				result.append(String.format("\n    %s (%d time(s); first in user %d iteration %d): %s", kind.kind(),
												Integer.valueOf(kind.count()), Integer.valueOf(kind.user()),
												Integer.valueOf(kind.iteration()), kind.message()));
			}
		}
		for (String problem : problems) {
			result.append("\n  ").append(problem);
		}
		return result.toString();
	}

	/**
	 * Returns the summary's first line: what ran, how many iterations passed and failed, and the throughput.
	 *
	 * @return the line
	 */
	@Nonnull String headline() {
		return String.format(Locale.ROOT, "SAIL load %s: %d virtual users, %d iterations (%d passed, %d failed, %.1f%%) in %s: " +
												"%.1f iterations/min, %.2f interactions/s",
								name, Integer.valueOf(plan.users()), Integer.valueOf(iterations.size()),
								Integer.valueOf(iterations.size() - failedIterations), Integer.valueOf(failedIterations),
								Double.valueOf(failureRate()), elapsed(), Double.valueOf(iterationsPerMinute()),
								Double.valueOf(interactionsPerSecond()));
	}

	private @Nonnull String elapsed() {
		long seconds = TimeUnit.NANOSECONDS.toSeconds(elapsedNanos);
		return String.format("%dm %02ds", Long.valueOf(seconds / 60L), Long.valueOf(seconds % 60L));
	}

	private static @Nonnull String number(double value) {
		return (value == Math.rint(value)) ? String.valueOf((long) value) : String.valueOf(value);
	}

	/**
	 * Writes the report into a folder: {@code index.html} (the summary), {@code interactions.csv},
	 * {@code iterations.csv} and {@code samples.csv} (every interaction of every iteration).
	 *
	 * <p>Side effects: makes the folder and writes the files, replacing any there.
	 *
	 * @param folder	the folder
	 * @throws IOException	if a file cannot be written
	 */
	void write(@Nonnull Path folder) throws IOException {
		Files.createDirectories(folder);
		StringBuilder csv = new StringBuilder(1024).append("interaction,count,failed,median_ms,p95_ms,max_ms\n");
		for (InteractionStats interaction : interactions) {
			csv.append(csv(interaction.name())).append(',').append(interaction.count()).append(',').append(interaction.failures())
				.append(',').append(interaction.medianMillis()).append(',').append(interaction.p95Millis()).append(',')
				.append(interaction.maxMillis()).append('\n');
		}
		Files.writeString(folder.resolve("interactions.csv"), csv, StandardCharsets.UTF_8);

		long origin = iterations.isEmpty() ? 0L : iterations.get(0).startNanos();
		csv.setLength(0);
		csv.append("user,iteration,start_ms,duration_ms,failed,failure\n");
		for (Iteration iteration : iterations) {
			Throwable failure = iteration.failure();
			csv.append(iteration.user()).append(',').append(iteration.number()).append(',')
				.append(TimeUnit.NANOSECONDS.toMillis(iteration.startNanos() - origin)).append(',')
				.append(TimeUnit.NANOSECONDS.toMillis(iteration.endNanos() - iteration.startNanos())).append(',')
				.append(iteration.failed()).append(',').append(csv((failure == null) ? "" : shorten(message(failure)))).append('\n');
		}
		Files.writeString(folder.resolve("iterations.csv"), csv, StandardCharsets.UTF_8);

		csv.setLength(0);
		csv.append("user,iteration,interaction,start_ms,duration_ms,failed,failure\n");
		for (Sample sample : samples) {
			String failure = sample.failure();
			csv.append(sample.user()).append(',').append(sample.iteration()).append(',').append(csv(sample.interaction())).append(',')
				.append(TimeUnit.NANOSECONDS.toMillis(sample.startNanos() - origin)).append(',').append(sample.millis()).append(',')
				.append(sample.failed()).append(',').append(csv((failure == null) ? "" : shorten(failure))).append('\n');
		}
		Files.writeString(folder.resolve("samples.csv"), csv, StandardCharsets.UTF_8);

		Files.writeString(folder.resolve("index.html"), html(), StandardCharsets.UTF_8);
	}

	/**
	 * Quotes a CSV cell. A cell that a spreadsheet would read as a formula (starting with {@code =}, {@code +}, {@code -}
	 * or {@code @}) starts with {@code '} instead, so that opening the report cannot run one.
	 */
	static @Nonnull String csv(@Nonnull String value) {
		boolean formula = (! value.isEmpty()) && ("=+-@".indexOf(value.charAt(0)) >= 0);
		return '"' + (formula ? "'" : "") + value.replace("\"", "\"\"") + '"';
	}

	private @Nonnull String html() {
		StringBuilder result = new StringBuilder(4096);
		result.append("<!DOCTYPE html>\n<html lang=\"en\"><head><meta charset=\"utf-8\"><title>SAIL load</title>")
				.append("<style>body{font-family:sans-serif;margin:16px}table{border-collapse:collapse;margin:8px 0}")
				.append("td,th{border:1px solid #ccc;padding:2px 8px}td.n{text-align:right}.failed{color:#b00020}</style>")
				.append("</head><body>\n<h1>SAIL load</h1>\n<p>").append(SailMedia.escape(headline())).append("</p>\n");
		List<Violation> violations = violations();
		result.append("<p class=\"").append(violations.isEmpty() ? "" : "failed").append("\">")
				.append(violations.isEmpty() ? "Passed." : "Failed:").append("</p>\n");
		if (! violations.isEmpty()) {
			result.append("<ul>");
			for (Violation violation : violations) {
				result.append("<li>").append(SailMedia.escape(violation.message())).append("</li>");
			}
			result.append("</ul>\n");
		}
		result.append("<h2>Interactions</h2>\n<table><tr><th>interaction</th><th>count</th><th>failed</th><th>median ms</th>")
				.append("<th>p95 ms</th><th>max ms</th></tr>\n");
		for (InteractionStats interaction : interactions) {
			result.append("<tr><td>").append(SailMedia.escape(interaction.name())).append("</td>").append(cell(interaction.count()))
					.append(cell(interaction.failures())).append(cell(interaction.medianMillis())).append(cell(interaction.p95Millis()))
					.append(cell(interaction.maxMillis())).append("</tr>\n");
		}
		result.append("</table>\n<h2>Virtual users</h2>\n<table><tr><th>user</th><th>passed</th><th>failed</th></tr>\n");
		for (UserStats user : users) {
			result.append("<tr>").append(cell(user.user())).append(cell(user.passed())).append(cell(user.failed())).append("</tr>\n");
		}
		result.append("</table>\n");
		if (! failureKinds.isEmpty()) {
			result.append("<h2>First failure of each kind</h2>\n<table><tr><th>kind</th><th>times</th><th>first</th><th>message</th></tr>\n");
			for (FailureKind kind : failureKinds) {
				result.append("<tr><td>").append(SailMedia.escape(kind.kind())).append("</td>").append(cell(kind.count()))
						.append("<td>user ").append(kind.user()).append(" iteration ").append(kind.iteration()).append("</td><td>")
						.append(SailMedia.escape(kind.message())).append("</td></tr>\n");
			}
			result.append("</table>\n");
		}
		result.append("<p>Every sample: <a href=\"samples.csv\">samples.csv</a>; iterations: <a href=\"iterations.csv\">iterations.csv</a>; ")
				.append("interactions: <a href=\"interactions.csv\">interactions.csv</a>.</p>\n</body></html>\n");
		return result.toString();
	}

	private static @Nonnull String cell(long value) {
		return "<td class=\"n\">" + value + "</td>";
	}

}
