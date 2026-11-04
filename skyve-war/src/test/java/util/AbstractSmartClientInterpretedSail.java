package util;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.impl.web.faces.FacesUtil;
import org.skyve.metadata.sail.language.Automation;
import org.skyve.metadata.user.User;
import org.slf4j.Logger;
import org.skyve.util.logging.SkyveLoggerFactory;

import jakarta.annotation.Nonnull;
import util.sail.BrowserConfiguration;
import util.sail.SmartClientInterpretedWebDriverExecutor;
import util.sail.SmartClientSelenide;

/**
 * Base test class for running SAIL scripts against a SmartClient UI in an interpreted browser session.
 */
public abstract class AbstractSmartClientInterpretedSail extends AbstractH2Test {

	private static final Logger LOGGER = SkyveLoggerFactory.getLogger(AbstractSmartClientInterpretedSail.class);

	private BrowserConfiguration configuration;
	@Nonnull protected SmartClientSelenide selenium = new SmartClientSelenide();
	
	protected AbstractSmartClientInterpretedSail(@Nonnull BrowserConfiguration configuration) {
		this.configuration = configuration;
	}
	
	@BeforeEach
	void setupBrowser() {
		selenium.startBrowser(configuration);
	}
	
	@AfterEach
	void tearDownBrowser() {
		selenium.stopBrowser();
	}
	
	protected void sailFile(String filePath) {
		Automation automation = XMLMetaData.unmarshalSAILFile(filePath);
		LOGGER.info("Automation: {}", automation);
		sail(automation);
	}
	
	/**
	 * Runs the SAIL script at the given test classpath resource.
	 *
	 * @param resourceName	the resource path, e.g. {@code sail/admin/sc/sc-admin-sail.xml}
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
	 * Runs the automation as a user with every role, like the bootstrap user the scripts sign in as,
	 * so that the test JVM sees the same views, actions and permissions as the browser.
	 *
	 * @param automation	the automation
	 */
	private void sail(@Nonnull Automation automation) {
		AbstractPersistence persistence = AbstractPersistence.get();
		User testUser = persistence.getUser();
		persistence.setUser(util.sail.SailUsers.bootstrapLikeUser(testUser));
		try (FacesUtil.SailFacesContextScope ignored = FacesUtil.withSailFacesContextIfNeeded()) {
			automation.execute(new SmartClientInterpretedWebDriverExecutor(selenium));
		}
		finally {
			persistence.setUser(testUser);
		}
	}
}
