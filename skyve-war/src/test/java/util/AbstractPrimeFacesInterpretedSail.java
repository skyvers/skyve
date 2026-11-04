package util;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.impl.web.faces.FacesUtil;
import org.skyve.metadata.sail.language.Automation;

import jakarta.annotation.Nonnull;
import util.sail.BrowserConfiguration;
import util.sail.PrimeFacesInterpretedWebDriverExecutor;
import util.sail.PrimeFacesSelenide;

/**
 * Base test class for running SAIL scripts against a PrimeFaces UI in an interpreted browser session.
 */
public abstract class AbstractPrimeFacesInterpretedSail extends AbstractH2Test {

	private BrowserConfiguration configuration;
	@Nonnull protected PrimeFacesSelenide selenium = new PrimeFacesSelenide();
	
	protected AbstractPrimeFacesInterpretedSail(@Nonnull BrowserConfiguration configuration) {
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
	
	private void sail(Automation automation) {
		try (FacesUtil.SailFacesContextScope ignored = FacesUtil.withSailFacesContextIfNeeded()) {
			automation.execute(new PrimeFacesInterpretedWebDriverExecutor(selenium));
		}
	}
}
