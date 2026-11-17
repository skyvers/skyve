package util.sail;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.EnumMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.function.Supplier;

import org.openqa.selenium.Dimension;
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
 * <li>The automations share one run: {@code {run}} is the same token in all of them, and a variable that one captures
 * 		can be used as {@code {var:name}} by the ones after it.</li>
 * <li>Views are built for the signed-in user's roles registered in the {@link SailViewRoles}, as the test bases do,
 * 		including users named with {@code {run}} (the shared token).</li>
 * <li>The server lock ({@link SailServerLock}) is taken once, before the first browser starts. The caller releases it
 * 		when the test class has finished (as the test bases do with {@link SailServerLock#releaseAll()}).</li>
 * <li>The run stops at the first automation that fails, as later ones build on earlier ones. The browser is stopped
 * 		when the workflow ends (unless a suite keeps it for its next member).</li>
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
	/** True to keep the browser open when a run ends; see {@link #keepBrowser()}. */
	private boolean keepBrowser;
	/** The browser a run left open for the next, or null. */
	private Selenide<?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?> kept;

	/**
	 * Creates a runner that builds views with the given registered roles.
	 *
	 * @param viewRoles	the roles of the users the scripts sign in as (see {@link SailViewRoles})
	 */
	public SailWorkflow(@Nonnull SailViewRoles viewRoles) {
		this.viewRoles = viewRoles;
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
	 */
	public void runResources(@Nonnull String... resourceNames) {
		List<Leg> legs = new ArrayList<>();
		for (String resourceName : resourceNames) {
			legs.addAll(legs(resourceName, XMLMetaData.unmarshalSAILWorkflowString(resource(resourceName))));
		}
		run(legs, null);
	}

	/**
	 * Runs the automations of SAIL script files, in order. Each script is a {@code <workflow>} of automations or a single
	 * {@code <automation>}.
	 *
	 * @param files	the scripts' paths, absolute or relative to the working directory
	 * @throws IllegalArgumentException	if a file is missing, unreadable or not a SAIL workflow or automation
	 * @throws AssertionError	naming the automation that failed, with its failure as the cause
	 */
	public void runFiles(@Nonnull String... files) {
		List<Leg> legs = new ArrayList<>();
		for (String file : files) {
			legs.addAll(legs(file, XMLMetaData.unmarshalSAILWorkflowFile(file)));
		}
		run(legs, null);
	}

	/**
	 * Runs the automations of SAIL scripts given as XML, in order. Each script is a {@code <workflow>} of automations or
	 * a single {@code <automation>}.
	 *
	 * @param scripts	the scripts
	 * @throws AssertionError	naming the automation that failed, with its failure as the cause
	 */
	public void runStrings(@Nonnull String... scripts) {
		List<Leg> legs = new ArrayList<>();
		for (int i = 0; i < scripts.length; i++) {
			legs.addAll(legs("script " + (i + 1), XMLMetaData.unmarshalSAILWorkflowString(scripts[i])));
		}
		run(legs, null);
	}

	/**
	 * A suite member that a workflow runs as: its suite's default test strategy, lock and evidence request apply.
	 *
	 * @param suite	the suite
	 * @param suiteName	the suite's file or resource, for messages
	 * @param member	the member, in the suite
	 */
	record Member(@Nonnull Suite suite, @Nonnull String suiteName, @Nonnull SuiteMember member) {
		/** Returns why the member's test strategy is locked; see {@link SailWorkflow#lockOf(Suite, SuiteMember)}. */
		@Nullable String lock() {
			return lockOf(suite, member);
		}
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
	 * configuration matches (see {@link #keepBrowser()}).
	 *
	 * @param name	the member's file, for messages
	 * @param workflow	the member's workflow (a file's automation is a workflow of one)
	 * @param member	the member and its suite
	 * @throws AssertionError	naming the automation that failed, with its failure as the cause
	 */
	void runMember(@Nonnull String name, @Nonnull Workflow workflow, @Nonnull Member member) {
		run(legs(name, workflow), member);
	}

	/**
	 * Keeps the browser open when a run ends, for the next run (a suite's next member) to use if its configuration
	 * matches; {@link #closeBrowser()} stops it.
	 */
	void keepBrowser() {
		keepBrowser = true;
	}

	/**
	 * Stops the browser that {@link #keepBrowser()} kept open, if any.
	 */
	void closeBrowser() {
		Selenide<?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?> browser = kept;
		kept = null;
		if (browser != null) {
			browser.stopBrowser();
		}
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

	private static @Nonnull String resource(@Nonnull String resourceName) {
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
	 * Runs the legs in order with one shared run.
	 *
	 * @param member	the suite member the legs are, or null for a workflow on its own
	 */
	private void run(@Nonnull List<Leg> legs, @Nullable Member member) {
		if (legs.isEmpty()) {
			throw new IllegalArgumentException("A SAIL workflow needs at least one automation");
		}
		RunState run = new RunState();
		Selenide<?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?> browser = kept;
		kept = null;
		boolean passed = false;
		SailMedia.beginUnit(); // with keepMedia=failed, the legs' media is deleted only if the whole workflow passed
		try {
			for (Leg leg : legs) {
				Automation automation = leg.automation();
				boolean smartClient = "desktop".equals(automation.getUxui());
				BrowserConfiguration configuration = configuration(automation.getUserAgentType()).settings(settings);
				// an invalid setting fails here, before this leg's browser starts
				SailSettings resolved = legSettings(configuration.resolve(), leg.name(), leg.workflow(), automation, member);
				SailServerLock.acquire(resolved.requireBaseUrl()); // once: this JVM holds it after the first time
				browser = browserFor(browser, configuration, resolved, smartClient);
				System.out.println(String.format("SAIL workflow: %s in %s (run %s)", leg.name(), smartClient ? "SmartClient" : "PrimeFaces", run.getRunToken()));
				try {
					execute(automation, browser, run, resolved);
				}
				catch (AssertionError | RuntimeException e) { // a step failure, or an invalid script
					throw new AssertionError("SAIL workflow: " + leg.name() + " failed: " + e.getMessage(), e);
				}
			}
			passed = true;
		}
		finally {
			SailMedia.endUnit(passed);
			if (browser != null) {
				if (keepBrowser) {
					kept = browser;
				}
				else {
					browser.stopBrowser();
				}
			}
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
		SailSettings result = resolved.with(workflowLayer,
											SailSettings.suite(member.suiteName() + ((lock == null) ? "" : " (locked: " + lock + ')'),
																suiteStrategy,
																suite.getEvidence()));
		return (lock == null) ? result : result.lockTestStrategy(lock);
	}

	private @Nonnull BrowserConfiguration configuration(@Nonnull UserAgentType userAgentType) {
		Supplier<BrowserConfiguration> supplier = browsers.get(userAgentType);
		return (supplier == null) ? defaultBrowser(userAgentType) : Objects.requireNonNull(supplier.get(), "browser configuration");
	}

	/**
	 * Returns the browser for the next leg: the current one if the leg's settings and configuration ask for the same
	 * browser (driven by the leg's renderer's Selenide, with the leg's timeout), or else a new one, after the current
	 * one is stopped.
	 */
	private static @Nonnull Selenide<?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?> browserFor(@Nullable Selenide<?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?> current,
																						@Nonnull BrowserConfiguration configuration,
																						@Nonnull SailSettings resolved,
																						boolean smartClient) {
		if ((current != null) && (current.browserChange(resolved, configuration) == null)) {
			Selenide<?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?> result = current;
			if (smartClient != (current instanceof SmartClientSelenide)) {
				result = smartClient ? new SmartClientSelenide() : new PrimeFacesSelenide();
				result.useBrowserOf(current);
			}
			result.useSettings(resolved, configuration); // the same browser: applies the leg's timeout
			return result;
		}
		if (current != null) {
			System.out.println("SAIL workflow: a new browser for another device");
			current.stopBrowser();
		}
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
