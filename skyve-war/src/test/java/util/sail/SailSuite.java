package util.sail;

import static org.junit.jupiter.api.Assertions.assertAll;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Consumer;

import org.junit.jupiter.api.function.Executable;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.sail.language.Automation;
import org.skyve.metadata.sail.language.OrdinarySuiteMember;
import org.skyve.metadata.sail.language.Suite;
import org.skyve.metadata.sail.language.SuiteMember;
import org.skyve.metadata.sail.language.ViewRoles;
import org.skyve.metadata.sail.language.Workflow;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Runs a SAIL suite: an execution plan of independent members, each a workflow file (an {@code <automation>} file is a
 * workflow of one), from a {@code <suite>} file (see {@link Suite}):
 * <ul>
 * <li><b>Members.</b> The set-up members run first, in order, then the ordinary members, in order, then the restore.
 * 		Each member is run by the {@link SailWorkflow} engine: each automation in the renderer its {@code uxui} names and
 * 		the browser its {@code userAgentType} needs; the browser carries on from one member to the next when the
 * 		configuration matches, and each member signs in for itself. Each member is a run of its own: members share no
 * 		{@code {run}} token or variables.</li>
 * <li><b>Requirements.</b> A member runs only if every member it requires passed (see
 * 		{@link Suite#requirementsOf(SuiteMember)}: what its {@code requires} names, and the set-ups). One that does not
 * 		run is reported as skipped, with the requirement that failed or was skipped. A failed ordinary member does not
 * 		stop the others. The restore runs last, even after failures, if its requirements passed.</li>
 * <li><b>Selection.</b> {@value SailSettings#SUITE_ONLY} (e.g. {@code pf/}, {@code sc/03}, {@code 03}) chooses
 * 		ordinary members by a prefix of their file as the suite writes it, or of its name; the members they require are
 * 		pulled in, and printed. Set-up members and the restore always run.</li>
 * <li><b>Test strategies.</b> The suite's {@code testStrategy} is a default for unlocked members (see
 * 		{@link Suite#getEffectiveTestStrategy(SuiteMember, Workflow, Automation)}). A run's override
 * 		({@value SailSettings#TEST_STRATEGY} from {@code -D}, the environment, code or the properties file) applies to
 * 		unlocked members; a locked member (a set-up, the restore, or one with {@code lockTestStrategy="true"}) ignores
 * 		it, and the trace and the summary say so.</li>
 * <li><b>Evidence and view roles.</b> The suite's {@code evidence} is a request that the run's settings can override;
 * 		its {@code viewRoles} register the roles that views are built with for the users the members sign in as.</li>
 * </ul>
 * When the suite ends, it prints a summary of the passed, failed, skipped and unselected members, and the test fails
 * with every failed member. The older builder ({@link #SailSuite(String)}) is kept for existing tests: it runs on the
 * same engine as it always ran: its files in a chain (a file runs only after the earlier files passed); a restore whose
 * {@code requires} names a file that is not in the suite is held back; a {@value SailSettings#SUITE_ONLY} that matches
 * no file runs the set-ups and the restore; and it refuses a test strategy override.
 * <p>
 * Not thread-safe.
 */
public final class SailSuite {
	/**
	 * System property naming the suite files (by prefix, comma separated) to run, e.g. {@code 03,04}.
	 * @deprecated	use {@link SailSettings#SUITE_ONLY}
	 */
	@Deprecated(since = "10")
	public static final String ONLY_PROPERTY = SailSettings.SUITE_ONLY;

	/**
	 * Runs one member of a suite: by default the {@link SailWorkflow} engine, in a browser; a unit test gives its own, to
	 * test the engine's order, requirements, selection and locks without a browser.
	 */
	@FunctionalInterface
	interface MemberRunner {
		/**
		 * Runs a member.
		 *
		 * @param run	the member, where its file is, and its lock
		 * @throws AssertionError	if the member fails
		 * @throws RuntimeException	if the member's file is missing or invalid
		 */
		void run(@Nonnull MemberRun run);
	}

	/**
	 * A member to run.
	 *
	 * @param member	the suite's member
	 * @param path	where the member's file is: a classpath resource, or a file path
	 * @param lock	why its test strategy is locked ({@code set-up}, {@code restore}, {@code lockTestStrategy}), or null
	 */
	record MemberRun(@Nonnull SuiteMember member, @Nonnull String path, @Nullable String lock) {
		// a value
	}

	/** How a member ended. */
	enum Outcome {
		/** It ran and passed. */
		PASSED,
		/** It ran and failed. */
		FAILED,
		/** It did not run, as a member it requires failed or was skipped. */
		SKIPPED,
		/** It was not chosen by {@value SailSettings#SUITE_ONLY}. */
		NOT_SELECTED
	}

	/**
	 * What a suite run did.
	 *
	 * @param outcomes	each member's outcome, by file, in run order
	 * @param failures	what each failed member threw, by file, in run order
	 * @param skips	why each skipped member did not run, by file
	 * @param notes	the notes the summary ends with: the selection, overrides not applied to locked members
	 */
	record Summary(@Nonnull Map<String, Outcome> outcomes,
					@Nonnull Map<String, Throwable> failures,
					@Nonnull Map<String, String> skips,
					@Nonnull List<String> notes) {
		/**
		 * Lists the members with an outcome.
		 *
		 * @param outcome	the outcome to list
		 * @return their files, in run order
		 */
		@Nonnull List<String> members(@Nonnull Outcome outcome) {
			List<String> result = new ArrayList<>();
			for (Map.Entry<String, Outcome> entry : outcomes.entrySet()) {
				if (entry.getValue() == outcome) {
					result.add(entry.getKey());
				}
			}
			return result;
		}

		/**
		 * Describes the run: the passed, failed, skipped (with why) and unselected members, then the notes.
		 *
		 * @param name	the suite, for the heading
		 * @return the summary, over several lines
		 */
		@Nonnull String describe(@Nonnull String name) {
			List<String> passed = members(Outcome.PASSED);
			List<String> failed = members(Outcome.FAILED);
			List<String> skipped = members(Outcome.SKIPPED);
			List<String> notSelected = members(Outcome.NOT_SELECTED);
			StringBuilder result = new StringBuilder(512);
			result.append("SAIL suite ").append(name).append(": ").append(passed.size()).append(" passed, ")
					.append(failed.size()).append(" failed, ").append(skipped.size()).append(" skipped");
			if (! notSelected.isEmpty()) {
				result.append(", ").append(notSelected.size()).append(" not selected");
			}
			result.append("\n  passed: ").append(passed);
			for (String file : failed) {
				result.append("\n  failed: ").append(file).append(": ").append(failures.get(file).getMessage());
			}
			for (String file : skipped) {
				result.append("\n  skipped: ").append(file).append(": ").append(skips.get(file));
			}
			if (! notSelected.isEmpty()) {
				result.append("\n  not selected: ").append(notSelected);
			}
			for (String note : notes) {
				result.append("\n  ").append(note);
			}
			return result.toString();
		}
	}

	private final Suite suite;
	/** The suite's file or resource, for messages. */
	private final String name;
	/** The folder the members' files are in: a classpath folder (ending with {@code /}, or empty) or a directory. */
	private final String folder;
	/** True if the members' files are classpath resources, false for files. */
	private final boolean resources;
	/** True for the older builder: its ordinary files are a chain, and an override is refused. */
	private final boolean legacy;
	/** Runs each member. */
	private MemberRunner runner;
	/** The engine that runs the members, or null when a test or the older builder gives the runner. */
	private SailWorkflow engine;
	private SailSettings.Code settings = SailSettings.code();
	/** The sources the settings resolve from: this JVM's, unless a test gives others. */
	private SailSettings.Environment environment;

	private SailSuite(@Nonnull Suite suite, @Nonnull String name, @Nonnull String folder, boolean resources, boolean legacy) {
		this.suite = suite;
		this.name = name;
		this.folder = folder;
		this.resources = resources;
		this.legacy = legacy;
	}

	/**
	 * Loads a suite from a test classpath resource, to run with a workflow engine (such as a test base's).
	 *
	 * <p>Side effects: registers the suite's view roles with the engine.
	 *
	 * @param resourceName	the suite file, e.g. {@code sail/admin/admin.suite.xml}; members are relative to its folder
	 * @param engine	runs the members, with its settings, view roles and browsers
	 * @return the suite, ready to {@link #run()}
	 * @throws IllegalArgumentException	if the resource, or a member's file, is missing or unreadable
	 * @throws org.skyve.metadata.MetaDataException	if the suite is invalid
	 */
	public static @Nonnull SailSuite resource(@Nonnull String resourceName, @Nonnull SailWorkflow engine) {
		return resource(resourceName, SailWorkflow.resource(resourceName), engine);
	}

	private static @Nonnull SailSuite resource(@Nonnull String resourceName, @Nonnull String xml, @Nonnull SailWorkflow engine) {
		Suite suite = XMLMetaData.unmarshalSAILSuiteString(xml, resourceName);
		int slash = resourceName.lastIndexOf('/');
		return new SailSuite(suite, resourceName, (slash < 0) ? "" : resourceName.substring(0, slash + 1), true, false)
					.checkMembersExist().engine(engine);
	}

	/**
	 * Loads a suite from a file, to run with a workflow engine (such as a test base's).
	 *
	 * <p>Side effects: registers the suite's view roles with the engine.
	 *
	 * @param file	the suite file, absolute or relative to the working directory; members are relative to its folder
	 * @param engine	runs the members, with its settings, view roles and browsers
	 * @return the suite, ready to {@link #run()}
	 * @throws org.skyve.metadata.MetaDataException	if the file is missing, unreadable or an invalid suite
	 * @throws IllegalArgumentException	if a member's file is missing
	 */
	public static @Nonnull SailSuite file(@Nonnull String file, @Nonnull SailWorkflow engine) {
		Suite suite = XMLMetaData.unmarshalSAILSuiteFile(file);
		Path parent = Path.of(file).toAbsolutePath().getParent();
		return new SailSuite(suite, file, (parent == null) ? "" : parent.toString(), false, false).checkMembersExist().engine(engine);
	}

	/**
	 * Checks that every member's file exists, before anything runs.
	 *
	 * @throws IllegalArgumentException	naming the suite and each missing member file
	 */
	private @Nonnull SailSuite checkMembersExist() {
		List<String> missing = new ArrayList<>();
		ClassLoader loader = Thread.currentThread().getContextClassLoader();
		for (SuiteMember member : suite.getAllMembers()) {
			String path = path(member);
			if (resources ? (loader.getResource(path) == null) : (! Files.isRegularFile(Path.of(path)))) {
				missing.add(member.getFile());
			}
		}
		if (! missing.isEmpty()) {
			throw new IllegalArgumentException("SAIL suite " + name + " names member files that do not exist: " + missing);
		}
		return this;
	}

	/** Returns where a member's file is: a classpath resource or a file path. */
	private @Nonnull String path(@Nonnull SuiteMember member) {
		String file = member.getFile();
		return resources ? folder + file : Path.of(folder).resolve(file).toString();
	}

	/**
	 * Runs one SAIL entry as the kind of document it is: a {@code <suite>} as a suite, a {@code <workflow>} or an
	 * {@code <automation>} as a workflow. The entry is a file path if such a file exists, else a test classpath resource;
	 * a resource is read once.
	 *
	 * @param entry	a file path or a classpath resource
	 * @param engine	runs the automations, with its settings, view roles and browsers
	 * @throws AssertionError	if the suite or workflow fails
	 * @throws IllegalArgumentException	if the entry is neither a file nor a resource, or a suite's member is missing
	 */
	public static void runEntry(@Nonnull String entry, @Nonnull SailWorkflow engine) {
		if (Files.isRegularFile(Path.of(entry))) {
			if (XMLMetaData.sailRootOfFile(entry) == XMLMetaData.SAILRoot.SUITE) {
				file(entry, engine).run();
			}
			else {
				engine.runFiles(entry);
			}
			return;
		}
		String xml = SailWorkflow.resource(entry);
		if (XMLMetaData.sailRootOfString(xml) == XMLMetaData.SAILRoot.SUITE) {
			resource(entry, xml, engine).run();
		}
		else {
			engine.runWorkflow(entry, XMLMetaData.unmarshalSAILWorkflowString(xml));
		}
	}

	/**
	 * Makes a suite of a model already loaded, for tests that run its members with a runner of their own.
	 *
	 * @param suite	a valid suite
	 * @param name	its name, for messages
	 * @param memberRunner	runs each member
	 * @return the suite, ready to {@link #run()}
	 */
	static @Nonnull SailSuite of(@Nonnull Suite suite, @Nonnull String name, @Nonnull MemberRunner memberRunner) {
		SailSuite result = new SailSuite(suite, name, "", true, false);
		result.runner = memberRunner;
		return result;
	}

	private @Nonnull SailSuite engine(@Nonnull SailWorkflow workflow) {
		engine = workflow;
		settings = workflow.settings();
		SailViewRoles roles = workflow.viewRoles();
		for (ViewRoles user : suite.getViewRoles()) {
			roles.register(user.getUser(), user.getRoles().toArray(String[]::new));
		}
		runner = this::runWithEngine;
		return this;
	}

	/**
	 * Returns the suite's code-level settings, to change in a test, e.g. {@code settings().suiteOnly("pf/")}. For a
	 * suite loaded with a workflow engine, they are the engine's.
	 *
	 * @return the settings builder
	 */
	public @Nonnull SailSettings.Code settings() {
		return settings;
	}

	/**
	 * Uses a test's code-level settings, such as a test base's {@code settings()}.
	 *
	 * <p>Side effects: replaces the settings the suite had, and its engine's, which run the members.
	 *
	 * @param code	the test's settings; shared, so later changes to them apply
	 * @return this suite
	 */
	public @Nonnull SailSuite settings(@Nonnull SailSettings.Code code) {
		settings = code;
		if (engine != null) {
			engine.settings(code);
		}
		return this;
	}

	/** Resolves the settings from the given sources instead of this JVM's, for tests. */
	@Nonnull SailSuite environment(@Nonnull SailSettings.Environment sources) {
		environment = sources;
		return this;
	}

	/**
	 * Runs the suite, prints its summary, and fails with every failed member.
	 *
	 * @throws AssertionError	with each failed member's failure, headed by the summary
	 * @throws IllegalArgumentException	if a setting is invalid, or {@value SailSettings#SUITE_ONLY} chooses no member
	 */
	public void run() {
		Summary summary = execute();
		List<Executable> outcomes = new ArrayList<>();
		for (Map.Entry<String, Throwable> failure : summary.failures().entrySet()) {
			String file = failure.getKey();
			Throwable cause = failure.getValue();
			outcomes.add(() -> {
				throw new AssertionError("SAIL suite " + name + ": " + file + " failed: " + cause.getMessage(), cause);
			});
		}
		assertAll(summary.describe(name), outcomes);
	}

	/**
	 * Runs the suite and prints its summary.
	 *
	 * <p>Side effects: runs the members (in a browser, with the engine); stops the browser when the suite ends.
	 *
	 * @return what each member did
	 * @throws IllegalArgumentException	if a setting is invalid, or {@value SailSettings#SUITE_ONLY} chooses no member
	 * @throws IllegalStateException	for the older builder, if the test strategy is overridden
	 */
	@Nonnull Summary execute() {
		SailSettings resolved = SailSettings.resolve((environment == null) ? SailSettings.Environment.current() : environment,
														List.of(settings.layer()));
		List<String> notes = new ArrayList<>();
		if (legacy) {
			refuseTestStrategyOverride("A SAIL admin suite", resolved);
		}
		Set<SuiteMember> selected = select(resolved, notes);
		if (! legacy) {
			noteLockedMembers(resolved, selected, notes);
		}

		Map<String, Outcome> outcomes = new LinkedHashMap<>();
		Map<String, Throwable> failures = new LinkedHashMap<>();
		Map<String, String> skips = new HashMap<>();
		Map<SuiteMember, Outcome> byMember = new HashMap<>();
		SuiteMember previousOrdinary = null;
		try {
			for (SuiteMember member : suite.getAllMembers()) {
				String file = member.getFile();
				Outcome outcome = Outcome.NOT_SELECTED;
				if (selected.contains(member)) {
					String skip = unmetRequirement(member, byMember, previousOrdinary);
					if ((skip == null) && (engine != null) && engine.stopping()) {
						skip = "the run is stopping";
					}
					if (skip == null) {
						Throwable failure = runMember(member);
						outcome = (failure == null) ? Outcome.PASSED : Outcome.FAILED;
						if (failure != null) {
							failures.put(file, failure);
						}
					}
					else {
						outcome = Outcome.SKIPPED;
						skips.put(file, skip);
						System.out.println("SAIL suite " + name + ": " + file + " skipped: " + skip);
					}
					if (legacy && (member instanceof OrdinarySuiteMember)) {
						previousOrdinary = member;
					}
				}
				byMember.put(member, outcome);
				outcomes.put(file, outcome);
			}
		}
		finally {
			if (engine != null) {
				engine.closeBrowser();
			}
		}
		Summary result = new Summary(outcomes, failures, skips, notes);
		System.out.println(result.describe(name));
		return result;
	}

	/**
	 * Notes, in the trace and the summary, each selected locked member that a run's test strategy override does not
	 * apply to.
	 */
	private void noteLockedMembers(@Nonnull SailSettings resolved, @Nonnull Set<SuiteMember> selected, @Nonnull List<String> notes) {
		SailSettings.Setting override = resolved.runOverride(SailSettings.TEST_STRATEGY);
		if (override == null) {
			return;
		}
		for (SuiteMember member : selected) {
			String lock = SailWorkflow.lockOf(suite, member);
			if (lock != null) {
				String note = String.format("override %s from %s not applied to %s: locked (%s)",
												override.value(), override.source(), member.getFile(), lock);
				System.out.println("SAIL suite " + name + ": " + note);
				notes.add(note);
			}
		}
	}

	/**
	 * Returns the members to run: every set-up member and the restore, and the ordinary members that
	 * {@value SailSettings#SUITE_ONLY} chooses, with the members they require, transitively (all, without a choice).
	 *
	 * @throws IllegalArgumentException	if a choice chooses no ordinary member
	 */
	private @Nonnull Set<SuiteMember> select(@Nonnull SailSettings resolved, @Nonnull List<String> notes) {
		List<SuiteMember> all = suite.getAllMembers();
		List<String> prefixes = resolved.suiteOnly();
		if (prefixes.isEmpty()) {
			return new LinkedHashSet<>(all);
		}
		List<OrdinarySuiteMember> chosen = new ArrayList<>();
		for (OrdinarySuiteMember member : suite.getMembers()) {
			if (matches(member.getFile(), prefixes)) {
				chosen.add(member);
			}
		}
		if (chosen.isEmpty() && legacy) {
			// the older builder runs its set-ups and restore, as it always did
			String note = "only " + prefixes + " chooses no file: the set-ups and the restore run";
			System.out.println("SAIL suite " + name + ": " + note);
			notes.add(note);
		}
		else if (chosen.isEmpty()) {
			throw new IllegalArgumentException(String.format("%s chooses no member of SAIL suite %s: its members are %s",
																resolved.setting(SailSettings.SUITE_ONLY), name, files(suite.getMembers())));
		}
		Set<SuiteMember> needed = new LinkedHashSet<>(chosen);
		List<SuiteMember> pending = new ArrayList<>(chosen);
		List<String> pulled = new ArrayList<>();
		while (! pending.isEmpty()) {
			SuiteMember member = pending.remove(0);
			for (SuiteMember required : suite.requirementsOf(member)) {
				if ((required instanceof OrdinarySuiteMember) && needed.add(required)) {
					pending.add(required);
					pulled.add(required.getFile() + " (required by " + member.getFile() + ')');
				}
			}
		}
		Set<SuiteMember> result = new LinkedHashSet<>();
		for (SuiteMember member : all) {
			if ((! (member instanceof OrdinarySuiteMember)) || needed.contains(member)) {
				result.add(member);
			}
		}
		String note = "only " + prefixes + " chooses " + files(chosen) + (pulled.isEmpty() ? "" : ", with the members they require: " + pulled);
		System.out.println("SAIL suite " + name + ": " + note);
		notes.add(note);
		return result;
	}

	/**
	 * Indicates if a member's file starts with a prefix, or its name does (so that {@code 03} chooses {@code pf/03-x.xml}).
	 */
	private static boolean matches(@Nonnull String file, @Nonnull List<String> prefixes) {
		String fileName = file.substring(file.lastIndexOf('/') + 1);
		for (String prefix : prefixes) {
			if (file.startsWith(prefix) || fileName.startsWith(prefix)) {
				return true;
			}
		}
		return false;
	}

	private static @Nonnull List<String> files(@Nonnull List<? extends SuiteMember> members) {
		List<String> result = new ArrayList<>(members.size());
		for (SuiteMember member : members) {
			result.add(member.getFile());
		}
		return result;
	}

	/**
	 * Returns why a member cannot run: a requirement that did not pass, a {@code requires} name that is no member (only
	 * the older builder's, which is not validated), or (for the older builder) an earlier file that did not pass; or
	 * null if it can run.
	 */
	private @Nullable String unmetRequirement(@Nonnull SuiteMember member,
												@Nonnull Map<SuiteMember, Outcome> byMember,
												@Nullable SuiteMember previousOrdinary) {
		for (String required : member.getRequires()) {
			if (suite.getMember(required) == null) {
				return "requires " + required + ", which is not a file of this suite";
			}
		}
		List<SuiteMember> requirements = suite.requirementsOf(member); // a new list
		if (legacy && (member instanceof OrdinarySuiteMember) && (previousOrdinary != null)) {
			requirements.add(previousOrdinary);
		}
		for (SuiteMember required : requirements) {
			Outcome outcome = byMember.get(required);
			if (outcome != Outcome.PASSED) {
				return "requires " + required.getFile() + ", which " + ((outcome == Outcome.FAILED) ? "failed" : "was skipped");
			}
		}
		return null;
	}

	/**
	 * Runs one member, naming its media folders by its file.
	 *
	 * @return its failure, or null if it passed
	 */
	private @Nullable Throwable runMember(@Nonnull SuiteMember member) {
		String file = member.getFile();
		String path = path(member);
		System.out.println("SAIL suite " + name + ": " + file);
		SailMedia.label(file); // names the member's snapshot and video folders, when they are on
		try {
			runner.run(new MemberRun(member, path, SailWorkflow.lockOf(suite, member)));
			return null;
		}
		catch (AssertionError | RuntimeException e) { // a step failure, or a missing or invalid script
			return e;
		}
		finally {
			SailMedia.label(null);
		}
	}

	/** Runs a member's workflow file with the engine, which keeps the browser for the next member if it passes. */
	private void runWithEngine(@Nonnull MemberRun run) {
		String path = run.path();
		Workflow workflow = resources ? XMLMetaData.unmarshalSAILWorkflowString(SailWorkflow.resource(path)) : XMLMetaData.unmarshalSAILWorkflowFile(path);
		engine.runMember(run.member().getFile(), workflow, new SailWorkflow.Member(suite, name, run.member(), run.lock()));
	}

	// The older builder, kept for existing tests

	private final List<String> legacySetUps = new ArrayList<>();
	private final List<String> legacyFiles = new ArrayList<>();
	private String legacyRestore;
	private final List<String> legacyRestoreRequires = new ArrayList<>();

	/**
	 * Creates a suite of the scripts in a test classpath folder, with the older builder: {@link #setUp(String...)},
	 * {@link #files(String...)}, {@link #restore(String, String...)}, then {@link #run(Consumer)}. Its files form a
	 * chain: a file runs only after the earlier ones passed.
	 *
	 * @param folder	the folder, ending with {@code /}, e.g. {@code sail/admin/pf/}
	 * @deprecated	write a {@code <suite>} file and run it with {@link #resource(String, SailWorkflow)}
	 */
	@Deprecated(since = "10")
	public SailSuite(@Nonnull String folder) {
		this(new Suite(), folder, folder, true, true);
	}

	/**
	 * Sets the files that always run first, in order.
	 *
	 * @param names	the file names in the folder
	 * @return this suite
	 * @deprecated	see {@link #SailSuite(String)}
	 */
	@Deprecated(since = "10")
	public @Nonnull SailSuite setUp(@Nonnull String... names) {
		legacySetUps.addAll(Arrays.asList(names));
		return this;
	}

	/**
	 * Sets the suite's files, in order.
	 *
	 * @param names	the file names in the folder
	 * @return this suite
	 * @deprecated	see {@link #SailSuite(String)}
	 */
	@Deprecated(since = "10")
	public @Nonnull SailSuite files(@Nonnull String... names) {
		legacyFiles.addAll(Arrays.asList(names));
		return this;
	}

	/**
	 * Sets the file that runs last, even after a failure, provided each of the required files has passed.
	 *
	 * @param fileName	the restore file's name in the folder
	 * @param requires	the set up files that must have passed for the restore to run
	 * @return this suite
	 * @deprecated	see {@link #SailSuite(String)}
	 */
	@Deprecated(since = "10")
	public @Nonnull SailSuite restore(@Nonnull String fileName, @Nonnull String... requires) {
		legacyRestore = fileName;
		legacyRestoreRequires.addAll(Arrays.asList(requires));
		return this;
	}

	/**
	 * Runs the older builder's suite on the suite engine, each file with the given runner.
	 *
	 * @param sailResource	runs one script, given its test classpath resource name
	 * @throws AssertionError	with each failed file's failure
	 * @throws IllegalStateException	if a test strategy override is set
	 * @deprecated	see {@link #SailSuite(String)}
	 */
	@Deprecated(since = "10")
	public void run(@Nonnull Consumer<String> sailResource) {
		suite.getSetUps().clear();
		suite.getMembers().clear();
		for (String file : legacySetUps) {
			suite.getSetUps().add(member(new SuiteMember(), file));
		}
		for (String file : legacyFiles) {
			suite.getMembers().add(member(new OrdinarySuiteMember(), file));
		}
		if (legacyRestore != null) {
			SuiteMember restore = member(new SuiteMember(), legacyRestore);
			restore.getRequires().addAll(legacyRestoreRequires);
			suite.setRestore(restore);
		}
		runner = run -> sailResource.accept(run.path());
		run();
	}

	private static @Nonnull <M extends SuiteMember> M member(@Nonnull M member, @Nonnull String file) {
		member.setFile(file);
		return member;
	}

	/**
	 * Fails fast if the settings outside code ({@code -D}, the environment, the properties file) override the test
	 * strategy, for tests that it would weaken or break.
	 *
	 * @param what	what refuses it, for the message
	 * @throws IllegalStateException	if the override is set
	 * @throws IllegalArgumentException	if a setting is invalid
	 */
	public static void refuseTestStrategyOverride(@Nonnull String what) {
		refuseTestStrategyOverride(what, SailSettings.resolve());
	}

	/**
	 * Fails fast if the settings override the test strategy ({@value SailSettings#TEST_STRATEGY} from {@code -D}, the
	 * environment, code or the properties file), for tests that any override would invalidate (a suite's locked
	 * members ignore an override instead).
	 *
	 * @param what	what refuses it, for the message
	 * @param settings	the test's settings, e.g. {@code settings().resolve()} in a test base
	 * @throws IllegalStateException	if the override is set, naming its source
	 */
	public static void refuseTestStrategyOverride(@Nonnull String what, @Nonnull SailSettings settings) {
		SailSettings.Setting override = settings.runOverride(SailSettings.TEST_STRATEGY);
		if (override != null) {
			throw new IllegalStateException(String.format("%s runs with its scripts' own test strategies: unset %s (it is %s, from %s)",
															what, SailSettings.TEST_STRATEGY, override.value(), override.source()));
		}
	}
}
