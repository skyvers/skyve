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
	
	public AbstractPrimeFacesInterpretedSail(@Nonnull BrowserConfiguration configuration) {
		this.configuration = configuration;
	}
	
	@BeforeEach
	public void setupBrowser() {
		selenium.startBrowser(configuration);
	}
	
	@AfterEach
	public void tearDownBrowser() {
		selenium.stopBrowser();
	}
	
	protected void sailFile(String filePath) {
		Automation automation = XMLMetaData.unmarshalSAILFile(filePath);
		sail(automation);
	}
	
	protected void sailResource(String resourceName) {
		try (InputStream stream = Thread.currentThread().getContextClassLoader().getResourceAsStream(resourceName)) {
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
		FacesUtil.setSailFacesContextIfNeeded();
		try {
			automation.execute(new PrimeFacesInterpretedWebDriverExecutor(selenium));
		} finally {
			FacesUtil.resetSailFacesContextIfNeeded();
		}
	}
}
