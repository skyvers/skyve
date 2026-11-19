package util.sail;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.EnumMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.function.BooleanSupplier;
import java.util.function.Supplier;

import org.openqa.selenium.Dimension;
import org.opentest4j.TestAbortedException;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.sail.execution.RunState;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.impl.web.faces.FacesUtil;
import org.skyve.metadata.sail.language.Automation;
import org.skyve.metadata.sail.language.Automation.TestStrategy;
import org.skyve.metadata.sail.language.Suite;
import org.skyve.metadata.sail.language.SuiteMember;
import org.skyve.metadata.sail.language.Workflow;
import org.skyve.metadata.user.User;
import org.skyve.web.UserAgentType;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import util.sail.BrowserConfiguration.Browsers;

/**
 * Runs a SAIL workflow: automations, from several script files or from {@code <workflow>} files, in order, as one run
 * that can move between renderers.
 * <ul>
 * <li>Each automation runs in the renderer its {@code uxui} names: {@code desktop} in SmartClient
 * 		({@link SmartClientInterpretedWebDriverExecutor}), any other UX/UI in PrimeFaces
 * 		({@link PrimeFacesInterpretedWebDriverExecutor}).</li>
 * <li>The browser is the one for the automation's {@code userAgentType} (see {@link #browser(UserAgentType, Supplier)}).
 * 		A new browser starts only when an automation needs another user agent or window size than the browser the
 * 		previous automation used; otherwise the next automation, even in the other renderer, drives the same browser,
 * 		and so the same session. Each automation signs in for itself, as usual.</li>
 * <li>A step may restart the leg's browser, for example on another configuration with another
 * 		{@linkplain BrowserConfiguration#profile(java.nio.file.Path) profile}, with
 * 		{@code stopBrowser(); startBrowser(configuration)} on the leg's Selenide: the Selenide holds what its browser
 * 		started with, so the next leg compares against, reuses or stops that browser. A step that stops the browser
 * 		without starting it again is not supported. Two configurations are the same device only if their browser,
 * 		headless mode, server, user agent, window size, profile and extra arguments are all the same.</li>
 * <li>The automations share one run: {@code {run}} is the same token in all of them, and a variable that one captures
 * 		can be used as {@code {var:name}} by the ones after it.</li>
 * <li>Views are built for the signed-in user's roles registered in the {@link SailViewRoles}, as the test bases do,
 * 		including users named with {@code {run}} (the shared token).</li>
 * <li>The server lock ({@link SailServerLock}) is taken once, before the first browser starts. The caller releases it
 * 		when the test class has finished (as the test bases do with {@link SailServerLock#releaseAll()}).</li>
 * <li>The run stops at the first automation that fails, as later ones build on earlier ones. The browser is stopped
 * 		when the workflow ends (unless a suite keeps it for its next member).</li>
 * <li>When a run ends, after its browsers have quit, its device profiles end ({@link SailProfiles}): deleted if it
 * 		passed, kept if it failed. A run of its own ends when its call returns (or, for a suite member that keeps its
 * 		browser for the next, when that browser stops); a run shared across calls ends when the workflow is
 * 		{@linkplain #close() closed}.</li>
 * <li>A workflow's {@code testStrategy} is the default for its automations that state none; in a suite, so is the
 * 		suite's, unless the member is locked (see {@link SailSuite}).</li>
 * <li>The settings ({@link SailSettings}) are resolved, and checked, before the first browser starts, with this
 * 		workflow's code-level {@link #settings()}, which beat each browser configuration's own values. Their variables
 * 		({@value SailSettings#VAR_PREFIX}{@code name}) start the run; a value captured later replaces them.</li>
 * </ul>
 * Not thread-safe.
 */
public final class SailWorkflow {
	private final SailViewRoles viewRoles;
	private final Map<UserAgentType, Supplier<BrowserConfiguration>> browsers = new EnumMap<>(UserAgentType.class);
	private SailSettings.Code settings = SailSettings.code();
	/** The browser a suite member that passed left open for the next member, or null; see {@link #closeBrowser()}. */
	private Selenide<?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?> kept;
	/** Starts a new browser for a leg. */
	private final BrowserStarter starter;
	/** Runs a leg's automation in its browser. */
	private final LegRunner legRunner;
	/** The sources the legs' settings resolve from, or null for this JVM's. */
	private final SailSettings.Environment environment;
	/** True when no further leg or suite member may start (a load run is stopping); see {@link #stopWhen}. */
	private BooleanSupplier stopping = () -> false;
	/** The run that all of this runner's calls share, or null if each call has its own; see {@link #shareRunAcrossCalls()}. */
	private RunState sharedRun;
	/** True once a call in the shared run has thrown: the run failed, so {@link #close()} keeps its device profiles. */
	private boolean sharedRunFailed;
	/** The runs that passed whose device profiles end when the {@link #kept} browser stops, by run token. */
	private final List<String> keptRuns = new ArrayList<>();

	/**
	 * Starts a new browser: a {@link SmartClientSelenide} or {@link PrimeFacesSelenide}, started with the leg's settings.
	 */
	@FunctionalInterface
	interface BrowserStarter {
		/**
		 * Starts a browser.
		 *
		 * @param smartClient	true for SmartClient's Selenide, false for PrimeFaces'
		 * @param configuration	the leg's browser configuration
		 * @param resolved	the leg's settings
		 * @return the started browser
		 */
		@Nonnull Selenide<?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?> start(boolean smartClient, @Nonnull BrowserConfiguration configuration, @Nonnull SailSettings resolved);
	}

	/**
	 * Runs a leg's automation in a browser, as one of the workflow's runs.
	 */
	@FunctionalInterface
	interface LegRunner {
		/**
		 * Runs an automation.
		 *
		 * @param automation	the leg's automation
		 * @param browser	the browser it runs in
		 * @param run	the workflow's run, which the legs share
		 * @param settings	the leg's settings
		 * @throws AssertionError	if the automation fails
		 */
		void run(@Nonnull Automation automation,
					@Nonnull Selenide<?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?> browser,
					@Nonnull RunState run,
					@Nonnull SailSettings settings);
	}

	/**
	 * Creates a runner that builds views with the given registered roles.
	 *
	 * @param viewRoles	the roles of the users the scripts sign in as (see {@link SailViewRoles})
	 */
	public SailWorkflow(@Nonnull SailViewRoles viewRoles) {
		this.viewRoles = viewRoles;
		this.starter = SailWorkflow::startBrowser;
		this.legRunner = this::execute;
		this.environment = null;
	}

	/**
	 * Creates a runner that starts browsers and runs legs with the given functions, and resolves its settings from the
	 * given sources, for tests of the browser's lifecycle.
	 *
	 * @param viewRoles	the roles of the users the scripts sign in as
	 * @param starter	starts a new browser for a leg
	 * @param legRunner	runs a leg's automation in its browser
	 * @param environment	the sources the legs' settings resolve from, instead of this JVM's
	 */
	SailWorkflow(@Nonnull SailViewRoles viewRoles,
					@Nonnull BrowserStarter starter,
					@Nonnull LegRunner legRunner,
					@Nonnull SailSettings.Environment environment) {
		this.viewRoles = viewRoles;
		this.starter = starter;
		this.legRunner = legRunner;
		this.environment = environment;
	}

	/**
	 * Sets the browser for the automations of a user agent type, replacing the default one
	 * (see {@link #defaultBrowser(UserAgentType)}).
	 *
	 * @param userAgentType	the automations' user agent type
	 * @param configuration	makes the browser's configuration
	 * @return this runner
	 */
	public @Nonnull SailWorkflow browser(@Nonnull UserAgentType userAgentType, @Nonnull Supplier<BrowserConfiguration> configuration) {
		browsers.put(userAgentType, configuration);
		return this;
	}

	/**
	 * Makes this runner's calls share one run, instead of each call (and each suite member) starting its own: the
	 * {@code {run}} token and the variables that automations capture carry from one {@code runResources},
	 * {@code runFiles} or {@code runStrings} call to the next. Without it, every call is a run of its own.
	 *
	 * <p>Side effects: the run is never reset, so it holds the variables of every earlier call, including a failed one.
	 * Like the runner, it is for one thread: do not share such a runner between threads. The run's device profiles end
	 * only when the runner is {@linkplain #close() closed}, so close it when its calls are done (a test base does). A
	 * second call ends the run shared so far, as {@link #close()} would, and starts a new one.
	 *
	 * @return this runner
	 */
	public @Nonnull SailWorkflow shareRunAcrossCalls() {
		endSharedRun();
		sharedRun = new RunState();
		return this;
	}

	/**
	 * Returns the workflow's code-level settings, to change in a test, e.g. {@code settings().headless(false)}.
	 *
	 * @return the settings: a new builder, unless {@link #settings(SailSettings.Code)} gave the test's
	 */
	public @Nonnull SailSettings.Code settings() {
		return settings;
	}

	/**
	 * Uses a test's code-level settings, so that the workflow shares them with the rest of the test.
	 *
	 * <p>Side effects: replaces the settings the workflow had.
	 *
	 * @param code	the settings; shared, so later changes to them apply
	 * @return this runner
	 */
	public @Nonnull SailWorkflow settings(@Nonnull SailSettings.Code code) {
		settings = code;
		return this;
	}

	/**
	 * Returns the default browser for a user agent type: Chrome, with an iPad's user agent for {@code tablet} (as the
	 * PrimeFaces ITs use) and an iPhone's user agent and window size for {@code phone}; a desktop Chrome otherwise.
	 *
	 * @param userAgentType	the automation's user agent type
	 * @return the configuration
	 */
	public static @Nonnull BrowserConfiguration defaultBrowser(@Nonnull UserAgentType userAgentType) {
		BrowserConfiguration result = new BrowserConfiguration().browser(Browsers.chrome);
		if (userAgentType == UserAgentType.tablet) {
			result.userAgentString(Devices.ipad.userAgentString);
		}
		else if (userAgentType == UserAgentType.phone) {
			Dimension size = Devices.iphone.resolution;
			result.userAgentString(Devices.iphone.userAgentString).resolution(size.getWidth(), size.getHeight());
		}
		return result;
	}

	/**
	 * Runs the automations of SAIL scripts on the test classpath, in order. Each script is a {@code <workflow>} of
	 * automations or a single {@code <automation>}.
	 *
	 * @param resourceNames	the scripts' resource paths, e.g. {@code sail/kitchenSink/workflow/workflow-1-pf.xml}
	 * @throws IllegalArgumentException	if a resource is missing or unreadable
	 * @throws AssertionError	naming the automation that failed, with its failure as the cause
	 * @throws TestAbortedException	naming the automation that a step skipped (such as a call that needs what the browser
	 * 									can't do), with the skip as the cause; no later step or automation runs
	 */
	public void runResources(@Nonnull String... resourceNames) {
		List<Leg> legs = new ArrayList<>();
		for (String resourceName : resourceNames) {
			legs.addAll(legs(resourceName, XMLMetaData.unmarshalSAILWorkflowString(resource(resourceName))));
		}
		run(legs, null, false, false);
	}

	/**
	 * Runs the automations of SAIL script files, in order. Each script is a {@code <workflow>} of automations or a single
	 * {@code <automation>}.
	 *
	 * @param files	the scripts' paths, absolute or relative to the working directory
	 * @throws IllegalArgumentException	if a file is missing, unreadable or not a SAIL workflow or automation
	 * @throws AssertionError	naming the automation that failed, with its failure as the cause
	 * @throws TestAbortedException	naming the automation that a step skipped (such as a call that needs what the browser
	 * 									can't do), with the skip as the cause; no later step or automation runs
	 */
	public void runFiles(@Nonnull String... files) {
		List<Leg> legs = new ArrayList<>();
		for (String file : files) {
			legs.addAll(legs(file, XMLMetaData.unmarshalSAILWorkflowFile(file)));
		}
		run(legs, null, false, false);
	}

	/**
	 * Runs a workflow already loaded, as one run.
	 *
	 * @param name	where it came from, for messages
	 * @param workflow	the workflow (a single automation's is a workflow of one)
	 * @throws AssertionError	naming the automation that failed, with its failure as the cause
	 * @throws TestAbortedException	naming the automation that a step skipped (such as a call that needs what the browser
	 * 									can't do), with the skip as the cause; no later step or automation runs
	 */
	void runWorkflow(@Nonnull String name, @Nonnull Workflow workflow) {
		run(legs(name, workflow), null, false, false);
	}

	/**
	 * Runs the automations of SAIL scripts given as XML, in order. Each script is a {@code <workflow>} of automations or
	 * a single {@code <automation>}.
	 *
	 * @param scripts	the scripts
	 * @throws AssertionError	naming the automation that failed, with its failure as the cause
	 * @throws TestAbortedException	naming the automation that a step skipped (such as a call that needs what the browser
	 * 									can't do), with the skip as the cause; no later step or automation runs
	 */
	public void runStrings(@Nonnull String... scripts) {
		List<Leg> legs = new ArrayList<>();
		for (int i = 0; i < scripts.length; i++) {
			legs.addAll(legs("script " + (i + 1), XMLMetaData.unmarshalSAILWorkflowString(scripts[i])));
		}
		run(legs, null, false, false);
	}

	/**
	 * A suite member that a workflow runs as: its suite's default test strategy, lock and evidence request apply.
	 *
	 * @param suite	the suite
	 * @param suiteName	the suite's file or resource, for messages
	 * @param member	the member, in the suite
	 * @param lock	why the member's test strategy is locked (see {@link SailWorkflow#lockOf(Suite, SuiteMember)}), or null
	 */
	record Member(@Nonnull Suite suite, @Nonnull String suiteName, @Nonnull SuiteMember member, @Nullable String lock) {
		// a value
	}

	/**
	 * Returns why a suite member's test strategy is locked, by its place in the suite
	 * ({@link Suite#isTestStrategyLocked(SuiteMember)}).
	 *
	 * @param suite	the suite
	 * @param member	one of its members
	 * @return {@code set-up}, {@code restore} or {@code lockTestStrategy}; or null for an unlocked member
	 */
	static @Nullable String lockOf(@Nonnull Suite suite, @Nonnull SuiteMember member) {
		if (! suite.isTestStrategyLocked(member)) {
			return null;
		}
		if (suite.isSetUp(member)) {
			return "set-up";
		}
		return (member == suite.getRestore()) ? "restore" : "lockTestStrategy";
	}

	/**
	 * Returns the roles views are built with, to register more users' roles (a suite's {@code viewRoles}).
	 *
	 * @return the registered roles, which this engine uses
	 */
	@Nonnull SailViewRoles viewRoles() {
		return viewRoles;
	}

	/**
	 * Runs a suite member's workflow as one run of its own, in the browser that the member before it left open if the
	 * configuration matches. When the member passes, its browser is kept open for the next member (the suite stops it
	 * with {@link #closeBrowser()} when it ends); when it fails, its browser is stopped, so the next member starts clean.
	 *
	 * @param name	the member's file, for messages
	 * @param workflow	the member's workflow (a file's automation is a workflow of one)
	 * @param member	the member and its suite
	 * @throws AssertionError	naming the automation that failed, with its failure as the cause
	 * @throws TestAbortedException	naming the automation that a step skipped (such as a call that needs what the browser
	 * 									can't do), with the skip as the cause; no later step or automation runs
	 */
	void runMember(@Nonnull String name, @Nonnull Workflow workflow, @Nonnull Member member) {
		runMember(name, workflow, member, false);
	}

	/**
	 * Runs a suite member's workflow as one run of its own; see {@link #runMember(String, Workflow, Member)}.
	 *
	 * @param name	the member's file, for messages
	 * @param workflow	the member's workflow (a file's automation is a workflow of one)
	 * @param member	the member and its suite
	 * @param ownBrowser	true to start the member in a browser of its own and stop it when the member ends, whether it
	 * 						passed or failed, so the next member finds no cookies, storage, service workers or caches
	 * 						from this one; false to carry the browser on from member to member
	 * @throws AssertionError	naming the automation that failed, with its failure as the cause
	 * @throws TestAbortedException	naming the automation that a step skipped (such as a call that needs what the browser
	 * 									can't do), with the skip as the cause; no later step or automation runs
	 */
	void runMember(@Nonnull String name, @Nonnull Workflow workflow, @Nonnull Member member, boolean ownBrowser) {
		run(legs(name, workflow), member, ! ownBrowser, ownBrowser);
	}

	/**
	 * Stops the browser that a suite member left open for the next, if any, then ends the device profiles of the passed
	 * runs that waited for it. A suite calls it when it ends, in a {@code finally}, so no browser outlives its suite.
	 */
	void closeBrowser() {
		Selenide<?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?> browser = kept;
		kept = null;
		try {
			if (browser != null) {
				browser.stopBrowser();
			}
		}
		finally {
			endKeptRuns();
		}
	}

	/**
	 * Ends the workflow: stops the browser a suite member left open, if any, then ends the device profiles of the run
	 * shared across calls ({@link #shareRunAcrossCalls()}), if there is one: the run failed if any of its calls threw
	 * (see {@link SailProfiles}). A run of its own has ended already, when its call returned. A test that shares a run
	 * closes its runner when its calls are done, in a {@code finally}; a test base closes its own. Afterwards no run is
	 * shared: a later call is a run of its own.
	 */
	public void close() {
		try {
			closeBrowser();
		}
		finally {
			endSharedRun();
		}
	}

	/** Ends the device profiles of the run shared across calls, if there is one, and shares none. */
	private void endSharedRun() {
		RunState run = sharedRun;
		sharedRun = null;
		if (run != null) {
			SailProfiles.end(run.getRunToken(), ! sharedRunFailed);
		}
		sharedRunFailed = false;
	}

	/** Ends the device profiles of the passed runs that waited for the kept browser, which has stopped. */
	private void endKeptRuns() {
		for (String run : keptRuns) {
			SailProfiles.end(run, true);
		}
		keptRuns.clear();
	}

	/**
	 * An automation and where it came from, for messages.
	 *
	 * @param name	the script and the automation's position in it
	 * @param workflow	the workflow it is in (a single automation's is a workflow of one), whose default applies
	 * @param automation	the automation to run
	 */
	private record Leg(@Nonnull String name, @Nonnull Workflow workflow, @Nonnull Automation automation) {
	}

	private static @Nonnull List<Leg> legs(@Nonnull String source, @Nonnull Workflow workflow) {
		List<Automation> automations = workflow.getAutomations();
		List<Leg> result = new ArrayList<>(automations.size());
		for (int i = 0, l = automations.size(); i < l; i++) {
			result.add(new Leg((l == 1) ? source : String.format("%s automation %d of %d", source, Integer.valueOf(i + 1), Integer.valueOf(l)),
								workflow,
								automations.get(i)));
		}
		return result;
	}

	/**
	 * Reads a SAIL file from the test classpath.
	 *
	 * @param resourceName	the resource's path, e.g. {@code sail/admin/admin.suite.xml}
	 * @return the file's text
	 * @throws IllegalArgumentException	if the resource is missing or unreadable
	 */
	static @Nonnull String resource(@Nonnull String resourceName) {
		try (InputStream stream = Thread.currentThread().getContextClassLoader().getResourceAsStream(resourceName)) {
			if (stream == null) {
				throw new IllegalArgumentException("SAIL resource " + resourceName + " is not on the test classpath");
			}
			return new String(stream.readAllBytes(), StandardCharsets.UTF_8);
		}
		catch (IOException e) {
			throw new IllegalArgumentException("Could not read resource " + resourceName, e);
		}
	}

	/**
	 * Stops the engine starting another leg, or (in a suite) another member, once a condition holds: a load run's
	 * stopping flag ({@link SailLoad}).
	 *
	 * @param condition	true when nothing more may start
	 * @return this runner
	 */
	@Nonnull SailWorkflow stopWhen(@Nonnull BooleanSupplier condition) {
		stopping = condition;
		return this;
	}

	/**
	 * Indicates if nothing more may start; see {@link #stopWhen(BooleanSupplier)}.
	 *
	 * @return true once the condition holds
	 */
	boolean stopping() {
		return stopping.getAsBoolean();
	}

	/**
	 * Runs the legs in order with one shared run.
	 *
	 * @param member	the suite member the legs are, or null for a workflow on its own
	 * @param keepBrowser	true to keep the browser open for the next run if this one passes; false to stop it
	 * @param ownBrowser	true to start in a browser of its own, leaving a kept one alone; false to carry on in the kept one
	 */
	private void run(@Nonnull List<Leg> legs, @Nullable Member member, boolean keepBrowser, boolean ownBrowser) {
		if (legs.isEmpty()) {
			throw new IllegalArgumentException("A SAIL workflow needs at least one automation");
		}
		RunState run = (sharedRun == null) ? new RunState() : sharedRun;
		Selenide<?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?> browser = null;
		// the passed runs whose device profiles wait for the browser this run carries on in to stop
		List<String> waiting = new ArrayList<>();
		if (! ownBrowser) {
			browser = kept;
			kept = null;
			waiting.addAll(keptRuns);
			keptRuns.clear();
		}
		boolean passed = false;
		SailMedia.beginUnit(); // with keepMedia=failed, the legs' media is deleted only if the whole workflow passed
		try {
			for (Leg leg : legs) {
				if (stopping()) {
					throw new IllegalStateException("SAIL workflow: " + leg.name() + " did not start: the run is stopping");
				}
				Automation automation = leg.automation();
				boolean smartClient = "desktop".equals(automation.getUxui());
				BrowserConfiguration configuration = configuration(automation.getUserAgentType()).settings(settings);
				// an invalid setting fails here, before this leg's browser starts
				SailSettings resolved = legSettings((environment == null) ? configuration.resolve() : configuration.resolve(environment),
													leg.name(), leg.workflow(), automation, member);
				SailServerLock.acquire(resolved.requireBaseUrl()); // once: this JVM holds it after the first time
				Selenide<?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?> current = browser;
				browser = null; // until the leg has its browser: one that fails to start is never kept or reused
				browser = browserFor(current, configuration, resolved, smartClient);
				System.out.println(String.format("SAIL workflow: %s in %s (run %s)", leg.name(), smartClient ? "SmartClient" : "PrimeFaces", run.getRunToken()));
				try {
					legRunner.run(automation, browser, run, resolved);
				}
				catch (TestAbortedException e) { // a call the browser can't run: skipped, not failed
					throw new TestAbortedException("SAIL workflow: " + leg.name() + " skipped: " + e.getMessage(), e);
				}
				catch (AssertionError | RuntimeException e) { // a step failure, or an invalid script
					throw new AssertionError("SAIL workflow: " + leg.name() + " failed: " + e.getMessage(), e);
				}
			}
			passed = true;
		}
		finally {
			SailMedia.endUnit(passed);
			boolean keeping = (browser != null) && keepBrowser && passed;
			try {
				if (keeping) {
					kept = browser;
				}
				else if (browser != null) {
					browser.stopBrowser();
				}
			}
			finally {
				endProfiles(run, passed, waiting, keeping);
			}
		}
	}

	/**
	 * Ends a run's device profiles once its browser has quit, with those of the passed runs that waited for that
	 * browser; or, while the browser is kept for the next run, leaves them to end when it stops. A run shared across
	 * calls ends only when the workflow is closed: this notes whether it failed.
	 *
	 * @param run	the run that ended
	 * @param passed	true if it passed
	 * @param waiting	the passed runs whose profiles waited for the browser the run carried on in
	 * @param keeping	true if the run's browser is kept open for the next run
	 */
	private void endProfiles(@Nonnull RunState run, boolean passed, @Nonnull List<String> waiting, boolean keeping) {
		boolean own = (run != sharedRun);
		if ((! own) && (! passed)) {
			sharedRunFailed = true;
		}
		if (keeping) { // so the run passed
			keptRuns.addAll(waiting);
			if (own) {
				keptRuns.add(run.getRunToken());
			}
			return;
		}
		for (String token : waiting) {
			SailProfiles.end(token, true);
		}
		if (own) {
			SailProfiles.end(run.getRunToken(), passed);
		}
	}

	/**
	 * Adds a leg's script levels to its settings: its workflow's default test strategy and, for a suite member, its
	 * suite's (see {@link Suite#getEffectiveTestStrategy(SuiteMember, Workflow, Automation)}) and evidence request, and
	 * the member's lock. The executor adds the automation's own strategy.
	 *
	 * @param resolved	the leg's settings from the run and its browser configuration
	 * @param name	the leg's script, for the sources
	 * @param workflow	the leg's workflow
	 * @param automation	the leg's automation, in the workflow
	 * @param member	the suite member the workflow is, or null
	 * @return the settings
	 */
	static @Nonnull SailSettings legSettings(@Nonnull SailSettings resolved,
												@Nonnull String name,
												@Nonnull Workflow workflow,
												@Nonnull Automation automation,
												@Nullable Member member) {
		SailSettings.Layer workflowLayer = SailSettings.workflow(name, workflow.getTestStrategy());
		if (member == null) {
			return resolved.with(workflowLayer);
		}
		Suite suite = member.suite();
		String lock = member.lock();
		// the suite's level gives the strategy only when neither the automation nor its workflow states one
		TestStrategy suiteStrategy = (workflow.getEffectiveTestStrategy(automation) == null) ?
										suite.getEffectiveTestStrategy(member.member(), workflow, automation) :
										null;
		// the lock is named on the strategy's source only: the suite's other values are not locked
		SailSettings result = resolved.with(workflowLayer,
											SailSettings.suite(member.suiteName() + ((lock == null) ? "" : " (locked: " + lock + ')'),
																suiteStrategy,
																null),
											SailSettings.suite(member.suiteName(), null, suite.getEvidence()));
		return (lock == null) ? result : result.lockTestStrategy(lock);
	}

	private @Nonnull BrowserConfiguration configuration(@Nonnull UserAgentType userAgentType) {
		Supplier<BrowserConfiguration> supplier = browsers.get(userAgentType);
		return (supplier == null) ? defaultBrowser(userAgentType) : Objects.requireNonNull(supplier.get(), "browser configuration");
	}

	/**
	 * Returns the browser for the next leg: the current one if the leg's settings and configuration ask for the same
	 * browser (driven by the leg's renderer's Selenide, with the leg's timeout), or else a new one, after the current
	 * one is stopped. If carrying on with the current one fails, it is stopped.
	 */
	private @Nonnull Selenide<?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?> browserFor(@Nullable Selenide<?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?> current,
																				@Nonnull BrowserConfiguration configuration,
																				@Nonnull SailSettings resolved,
																				boolean smartClient) {
		if ((current != null) && (current.browserChange(resolved, configuration) == null)) {
			try {
				Selenide<?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?> result = current;
				if (smartClient != (current instanceof SmartClientSelenide)) {
					result = smartClient ? new SmartClientSelenide() : new PrimeFacesSelenide();
					result.useBrowserOf(current);
				}
				result.useSettings(resolved, configuration); // the same browser: applies the leg's timeout
				return result;
			}
			catch (RuntimeException e) {
				current.stopBrowser();
				throw e;
			}
		}
		if (current != null) {
			System.out.println("SAIL workflow: a new browser for another device");
			current.stopBrowser();
		}
		return starter.start(smartClient, configuration, resolved);
	}

	private static @Nonnull Selenide<?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?> startBrowser(boolean smartClient,
																						@Nonnull BrowserConfiguration configuration,
																						@Nonnull SailSettings resolved) {
		Selenide<?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?> result = smartClient ? new SmartClientSelenide() : new PrimeFacesSelenide();
		result.startBrowser(configuration, resolved);
		return result;
	}

	/**
	 * Runs one automation as the test bases do: as a user with every role (or the signed-in user's registered roles),
	 * sharing the workflow's run, with the leg's settings.
	 */
	private void execute(@Nonnull Automation automation,
							@Nonnull Selenide<?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?> browser,
							@Nonnull RunState run,
							@Nonnull SailSettings legSettings) {
		AbstractPersistence persistence = AbstractPersistence.get();
		User testUser = persistence.getUser();
		persistence.setUser(SailUsers.bootstrapLikeUser(testUser));
		try (FacesUtil.SailFacesContextScope ignored = FacesUtil.withSailFacesContextIfNeeded()) {
			InterpretedWebDriverExecutor<?, ?> executor = (browser instanceof SmartClientSelenide sc) ?
															new SmartClientInterpretedWebDriverExecutor(sc) :
															new PrimeFacesInterpretedWebDriverExecutor((PrimeFacesSelenide) browser);
			executor.useViewRoles(viewRoles);
			executor.shareRun(run);
			executor.useSettings(legSettings);
			automation.execute(executor);
		}
		finally {
			persistence.setUser(testUser);
		}
	}
}
