package modules.admin.ControlPanel.actions;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.domain.messages.ValidationException;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import modules.admin.ControlPanel.ControlPanelExtension;
import modules.admin.UserProxy.UserProxyExtension;
import modules.admin.domain.ControlPanel;
import modules.admin.domain.ControlPanel.SailExecutor;
import modules.admin.domain.UserProxy;
import util.AbstractH2Test;

/**
 * Tests the ExecuteSAIL action's validation, and that a failed execution goes into the results.
 * {@link DownloadSAILH2Test} executes real SAIL with each executor.
 */
@SuppressWarnings("java:S120") // Module test packages mirror the generated document package.
class ExecuteSAILTest extends AbstractH2Test {
	private static final String BASE_URL = "http://localhost:8080";
	private static final String COMPONENT_BUILDER = "org.skyve.impl.web.faces.pipeline.component.SkyveComponentBuilderChain";
	private static final String LAYOUT_BUILDER = "org.skyve.impl.web.faces.pipeline.layout.ResponsiveLayoutBuilder";
	private static final String SAIL = "<automation/>";

	private DataBuilder db;
	private ControlPanelExtension controlPanel;

	@BeforeEach
	void setup() {
		db = new DataBuilder().fixture(FixtureType.crud);
		controlPanel = db.build(ControlPanel.MODULE_NAME, ControlPanel.DOCUMENT_NAME);
	}

	@Test
	void testExecuteSAILWithNullUserThrowsValidationException() {
		// setup the test data - all fields except user
		controlPanel.setSailUser(null);
		controlPanel.setSailBaseUrl(BASE_URL);
		controlPanel.setSailExecutor(SailExecutor.primeFacesInlineWebDriver);
		controlPanel.setSailComponentBuilder(COMPONENT_BUILDER);
		controlPanel.setSailLayoutBuilder(LAYOUT_BUILDER);
		controlPanel.setSail(SAIL);

		// call the method under test and expect exception
		ValidationException e = assertThrows(ValidationException.class, () -> ExecuteSAIL.executeSAIL(controlPanel));

		// verify the exception contains the sailUser binding
		assertEquals(1, e.getMessages().size());
		assertTrue(hasBinding(e.getMessages().get(0).getBindings(), ControlPanel.sailUserPropertyName));
	}

	@Test
	void executePropagatesValidationExceptionFromActionEntryPoint() {
		controlPanel.setSailUser(null);
		controlPanel.setSailBaseUrl(BASE_URL);
		controlPanel.setSailExecutor(SailExecutor.primeFacesInlineWebDriver);
		controlPanel.setSailComponentBuilder(COMPONENT_BUILDER);
		controlPanel.setSailLayoutBuilder(LAYOUT_BUILDER);
		controlPanel.setSail(SAIL);

		ExecuteSAIL action = new ExecuteSAIL();
		ValidationException e = assertThrows(ValidationException.class,
				() -> action.execute(controlPanel, null));

		assertEquals(1, e.getMessages().size());
		assertTrue(hasBinding(e.getMessages().get(0).getBindings(), ControlPanel.sailUserPropertyName));
	}

	@Test
	void testExecuteSAILWithNullBaseUrlThrowsValidationException() {
		// setup the test data - all fields except baseUrl
		UserProxyExtension user = db.build(UserProxy.MODULE_NAME, UserProxy.DOCUMENT_NAME);
		controlPanel.setSailUser(user);
		controlPanel.setSailBaseUrl(null);
		controlPanel.setSailExecutor(SailExecutor.primeFacesInlineWebDriver);
		controlPanel.setSailComponentBuilder(COMPONENT_BUILDER);
		controlPanel.setSailLayoutBuilder(LAYOUT_BUILDER);
		controlPanel.setSail(SAIL);

		// call the method under test and expect exception
		ValidationException e = assertThrows(ValidationException.class, () -> ExecuteSAIL.executeSAIL(controlPanel));

		// verify the exception contains the sailBaseUrl binding
		assertEquals(1, e.getMessages().size());
		assertTrue(hasBinding(e.getMessages().get(0).getBindings(), ControlPanel.sailBaseUrlPropertyName));
	}

	@Test
	void testExecuteSAILWithNullExecutorThrowsValidationException() {
		// setup the test data - all fields except executor
		UserProxyExtension user = db.build(UserProxy.MODULE_NAME, UserProxy.DOCUMENT_NAME);
		controlPanel.setSailUser(user);
		controlPanel.setSailBaseUrl(BASE_URL);
		controlPanel.setSailExecutor(null);
		controlPanel.setSailComponentBuilder(COMPONENT_BUILDER);
		controlPanel.setSailLayoutBuilder(LAYOUT_BUILDER);
		controlPanel.setSail(SAIL);

		// call the method under test and expect exception
		ValidationException e = assertThrows(ValidationException.class, () -> ExecuteSAIL.executeSAIL(controlPanel));

		// verify the exception contains the sailExecutor binding
		assertEquals(1, e.getMessages().size());
		assertTrue(hasBinding(e.getMessages().get(0).getBindings(), ControlPanel.sailExecutorPropertyName));
	}

	@Test
	void testExecuteSAILWithNullComponentBuilderThrowsValidationException() {
		// setup the test data - all fields except componentBuilder
		UserProxyExtension user = db.build(UserProxy.MODULE_NAME, UserProxy.DOCUMENT_NAME);
		controlPanel.setSailUser(user);
		controlPanel.setSailBaseUrl(BASE_URL);
		controlPanel.setSailExecutor(SailExecutor.primeFacesInlineWebDriver);
		controlPanel.setSailComponentBuilder(null);
		controlPanel.setSailLayoutBuilder(LAYOUT_BUILDER);
		controlPanel.setSail(SAIL);

		// call the method under test and expect exception
		ValidationException e = assertThrows(ValidationException.class, () -> ExecuteSAIL.executeSAIL(controlPanel));

		// verify the exception contains the sailComponentBuilder binding
		assertEquals(1, e.getMessages().size());
		assertTrue(hasBinding(e.getMessages().get(0).getBindings(), ControlPanel.sailComponentBuilderPropertyName));
	}

	@Test
	void testExecuteSAILWithNullLayoutBuilderThrowsValidationException() {
		// setup the test data - all fields except layoutBuilder
		UserProxyExtension user = db.build(UserProxy.MODULE_NAME, UserProxy.DOCUMENT_NAME);
		controlPanel.setSailUser(user);
		controlPanel.setSailBaseUrl(BASE_URL);
		controlPanel.setSailExecutor(SailExecutor.primeFacesInlineWebDriver);
		controlPanel.setSailComponentBuilder(COMPONENT_BUILDER);
		controlPanel.setSailLayoutBuilder(null);
		controlPanel.setSail(SAIL);

		// call the method under test and expect exception
		ValidationException e = assertThrows(ValidationException.class, () -> ExecuteSAIL.executeSAIL(controlPanel));

		// verify the exception contains the sailLayoutBuilder binding
		assertEquals(1, e.getMessages().size());
		assertTrue(hasBinding(e.getMessages().get(0).getBindings(), ControlPanel.sailLayoutBuilderPropertyName));
	}

	@Test
	void testExecuteSAILWithNullSailThrowsValidationException() {
		// setup the test data - all fields except sail
		UserProxyExtension user = db.build(UserProxy.MODULE_NAME, UserProxy.DOCUMENT_NAME);
		controlPanel.setSailUser(user);
		controlPanel.setSailBaseUrl(BASE_URL);
		controlPanel.setSailExecutor(SailExecutor.primeFacesInlineWebDriver);
		controlPanel.setSailComponentBuilder(COMPONENT_BUILDER);
		controlPanel.setSailLayoutBuilder(LAYOUT_BUILDER);
		controlPanel.setSail(null);

		// call the method under test and expect exception
		ValidationException e = assertThrows(ValidationException.class, () -> ExecuteSAIL.executeSAIL(controlPanel));

		// verify the exception contains the sail binding
		assertEquals(1, e.getMessages().size());
		assertTrue(hasBinding(e.getMessages().get(0).getBindings(), ControlPanel.sailPropertyName));
	}

	@Test
	void testExecuteSAILWithMultipleNullFieldsThrowsValidationExceptionWithAllBindings() {
		// setup the test data - multiple null fields
		controlPanel.setSailUser(null);
		controlPanel.setSailBaseUrl(null);
		controlPanel.setSailExecutor(null);
		controlPanel.setSailComponentBuilder(null);
		controlPanel.setSailLayoutBuilder(null);
		controlPanel.setSail(null);

		// call the method under test and expect exception
		ValidationException e = assertThrows(ValidationException.class, () -> ExecuteSAIL.executeSAIL(controlPanel));

		// verify the exception contains all the bindings
		assertEquals(1, e.getMessages().size());
		List<String> bindings = toList(e.getMessages().get(0).getBindings());
		assertEquals(6, bindings.size());
		assertTrue(hasBinding(e.getMessages().get(0).getBindings(), ControlPanel.sailUserPropertyName));
		assertTrue(hasBinding(e.getMessages().get(0).getBindings(), ControlPanel.sailBaseUrlPropertyName));
		assertTrue(hasBinding(e.getMessages().get(0).getBindings(), ControlPanel.sailExecutorPropertyName));
		assertTrue(hasBinding(e.getMessages().get(0).getBindings(), ControlPanel.sailComponentBuilderPropertyName));
		assertTrue(hasBinding(e.getMessages().get(0).getBindings(), ControlPanel.sailLayoutBuilderPropertyName));
		assertTrue(hasBinding(e.getMessages().get(0).getBindings(), ControlPanel.sailPropertyName));
	}

	/**
	 * The Execute button shows what went wrong in the results, on the Results tab, rather than as an error message.
	 */
	@Test
	void anExecutionThatFailsPutsItsStackTraceInTheResults() {
		UserProxyExtension user = db.build(UserProxy.MODULE_NAME, UserProxy.DOCUMENT_NAME);
		controlPanel.setSailUser(user);
		controlPanel.setSailBaseUrl(BASE_URL);
		controlPanel.setSailExecutor(SailExecutor.primeFacesInlineWebDriver);
		controlPanel.setSailComponentBuilder(COMPONENT_BUILDER);
		controlPanel.setSailLayoutBuilder(LAYOUT_BUILDER);
		controlPanel.setSail("<notSail/>");

		ExecuteSAIL.executeSAIL(controlPanel);

		String results = controlPanel.getUnescapedResults();
		assertAll(() -> assertTrue((results != null) && results.contains("Exception"), () -> String.valueOf(results)),
					() -> assertEquals(Integer.valueOf(2), controlPanel.getTabIndex()));
	}

	/**
	 * Helper method to check if any binding in the iterable contains the given property name.
	 */
	private static boolean hasBinding(Iterable<String> bindings, String propertyName) {
		for (String binding : bindings) {
			if (binding.contains(propertyName)) {
				return true;
			}
		}
		return false;
	}

	/**
	 * Helper method to convert an iterable to a list.
	 */
	private static List<String> toList(Iterable<String> iterable) {
		List<String> list = new ArrayList<>();
		for (String item : iterable) {
			list.add(item);
		}
		return list;
	}
}
