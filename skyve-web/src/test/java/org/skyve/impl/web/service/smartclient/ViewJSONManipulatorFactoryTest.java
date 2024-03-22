package org.skyve.impl.web.service.smartclient;

import static org.mockito.Mockito.when;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import jakarta.servlet.ServletConfig;
import jakarta.servlet.ServletException;

/**
 * Test that we can instantiate a ViewJSONManipulator from SmartClientEditServlet from an init parameter.
 */
public class ViewJSONManipulatorFactoryTest {
	@Mock
	private ServletConfig config;
	
	private AutoCloseable auto;
	
	@BeforeEach
	public void setup() {
		auto = MockitoAnnotations.openMocks(this);
	}

	@AfterEach
	public void tearDown() throws Exception {
		if (auto != null) {
			auto.close();
		}
	}
	
	@Test
	public void testSCESInitWithManipulator() throws ServletException {
		SmartClientEditServlet sces = new SmartClientEditServlet();
		when(config.getInitParameter("manipulator")).thenReturn(TestExtendingViewJSONManipulator.class.getCanonicalName());
		sces.init(config);
	}
}
