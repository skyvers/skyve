package util;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.skyve.impl.metadata.user.UserImpl;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.impl.web.faces.FacesUtil;
import org.skyve.metadata.sail.language.Automation;
import org.skyve.metadata.user.User;

import jakarta.annotation.Nonnull;
import util.sail.BrowserConfiguration;
import util.sail.SailServerLock;
import util.sail.SailSettings;
import util.sail.SailViewRoles;
import util.sail.PrimeFacesInterpretedWebDriverExecutor;
import util.sail.PrimeFacesSelenide;
import util.sail.SailUsers;

/**
 * Base test class for running SAIL scripts against a PrimeFaces UI in an interpreted browser session.
 * <p>
 * Each test class holds {@link SailServerLock} for its server ({@code -Dsail.baseUrl}) from before its first browser
 * starts until the class has finished, so SAIL tests against one server run one at a time, even in forked JVMs
 * (surefire's {@code forkCount}). A test whose server is in use waits for the other to finish: the one necessary wait.
 * <p>
 * {@link #settings()} is the test's code level of {@link SailSettings}: every {@code -D} option can be set there, e.g.
 * {@code settings().headless(false).timeoutSeconds(60)}, and beats the configuration's own values and the properties
 * file, though not a {@code -D} or environment variable. They apply to each script as it starts: a browser-level change
 * (browser, headless mode, base URL) made in a test method restarts the browser for the next script.
 */
public abstract class AbstractPrimeFacesInterpretedSail extends AbstractH2Test {

	private BrowserConfiguration configuration;
	@Nonnull protected PrimeFacesSelenide selenium = new PrimeFacesSelenide();
	
	/** The test's code-level settings; see {@link #settings()}. */
	private final SailSettings.Code settings = SailSettings.code();

	protected AbstractPrimeFacesInterpretedSail(@Nonnull BrowserConfiguration configuration) {
		this.configuration = configuration.settings(settings);
	}

	/**
	 * Returns the test's code-level settings, to change in a test or its constructor, e.g.
	 * {@code settings().testStrategy(TestStrategy.Verify).evidence(SailSettings.Evidence.ALL)}.
	 *
	 * @return the settings builder, shared by every script this test runs
	 */
	protected final @Nonnull SailSettings.Code settings() {
		return settings;
	}
	
	/** The roles the views are built with for the users the scripts sign in as. */
	private final SailViewRoles viewRoles = new SailViewRoles();

	/**
	 * Builds the views for a user that the scripts sign in as with the user's roles, instead of every role.
	 * Call it before running the scripts; see {@link SailViewRoles}.
	 *
	 * @param userName	the user name as a script signs in with it; may contain {@code {run}}
	 * @param roleNames	the user's roles: {@code module.Role}, or a customer role's name
	 */
	protected final void viewRoles(@Nonnull String userName, @Nonnull String... roleNames) {
		viewRoles.register(userName, roleNames);
	}

	/**
	 * Defines a variable that every script this test runs starts with, for {@code {var:name}}. A recorded script signs
	 * in with {@code password="{var:password}"}: define {@code password} to run it unedited. It is a code-level setting
	 * ({@code settings().variable(name, value)}), so {@code -Dsail.var.name} beats it.
	 *
	 * @param name	the variable name
	 * @param value	the value
	 */
	protected final void variable(@Nonnull String name, @Nonnull String value) {
		settings.variable(name, value);
	}

	@BeforeEach
	void setupBrowser() {
		// One SAIL test at a time per server, even across forked JVMs (see SailServerLock): may wait for another to finish
		SailSettings resolved = configuration.resolve();
		SailServerLock.acquire(resolved.requireBaseUrl());
		selenium.startBrowser(configuration, resolved);
	}

	@AfterAll
	static void releaseServer() {
		SailServerLock.releaseAll();
	}
	
	@AfterEach
	void tearDownBrowser() {
		selenium.stopBrowser();
	}
	
	protected void sailFile(String filePath) {
		Automation automation = XMLMetaData.unmarshalSAILFile(filePath);
		sail(automation);
	}
	
	/**
	 * Runs the SAIL script at the given test classpath resource.
	 *
	 * @param resourceName	the resource path, e.g. {@code sail/admin/pf/pf-admin-sail.xml}
	 * @throws IllegalArgumentException if the resource is missing or unreadable
	 */
	protected void sailResource(@Nonnull String resourceName) {
		try (InputStream stream = Thread.currentThread().getContextClassLoader().getResourceAsStream(resourceName)) {
			if (stream == null) {
				throw new IllegalArgumentException("SAIL resource " + resourceName + " is not on the test classpath");
			}
			String sail = new String(stream.readAllBytes(), StandardCharsets.UTF_8);
			Automation automation = XMLMetaData.unmarshalSAILString(sail);
			sail(automation);
		} catch (IOException e) {
			throw new IllegalArgumentException("Could not read resource " + resourceName, e);
		}
	}
	
	protected void sailString(String sail) {
		Automation automation = XMLMetaData.unmarshalSAILString(sail);
		sail(automation);
	}
	
	/**
	 * Runs the automation. The PrimeFaces executor rebuilds each view's component tree in this JVM to find the
	 * client ids of the widgets on the page, so it runs as a user with the same permissions as the bootstrap user
	 * that the browser signs in as (every role of every module), not as the test's super user. Otherwise actions that
	 * the browser's user cannot see (such as Save on a document they cannot create) would be in the tree, shift the
	 * generated ids, and the executor would find the wrong elements. A login as a user registered with
	 * {@link #viewRoles(String, String...)} builds the views with that user's roles instead.
	 */
	private void sail(Automation automation) {
		// a browser-level change made in the test since the browser started restarts it; the timeout applies at once
		SailSettings resolved = configuration.resolve();
		selenium.useSettings(resolved, configuration);
		AbstractPersistence persistence = AbstractPersistence.get();
		User testUser = persistence.getUser();
		persistence.setUser(bootstrapLikeUser(testUser));
		try (FacesUtil.SailFacesContextScope ignored = FacesUtil.withSailFacesContextIfNeeded()) {
			PrimeFacesInterpretedWebDriverExecutor executor = new PrimeFacesInterpretedWebDriverExecutor(selenium);
			executor.useViewRoles(viewRoles);
			executor.useSettings(resolved);
			automation.execute(executor);
		}
		finally {
			persistence.setUser(testUser);
		}
	}

	/**
	 * Creates a user of the test user's customer with every role of every module, as Skyve's bootstrap user has.
	 *
	 * @param testUser	the test's user, whose customer, name and id are copied
	 * @return the user
	 * @see SailUsers#bootstrapLikeUser(User)
	 */
	protected static @Nonnull UserImpl bootstrapLikeUser(@Nonnull User testUser) {
		return SailUsers.bootstrapLikeUser(testUser);
	}
}
