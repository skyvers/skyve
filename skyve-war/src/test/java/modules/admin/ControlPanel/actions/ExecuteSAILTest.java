package modules.admin.ControlPanel.actions;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

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
 * Tests for the ExecuteSAIL action validation logic.
 * 
 * Note: The full execution logic is not tested here because it involves:
 * - FacesUtil.setSailFacesContextIfNeeded() (FacesContext setup)
 * - CORE.getPersistence() and CORE.getRepository() (static calls)
 * - XMLMetaData.unmarshalSAILString() (XML parsing)
 * - Dynamic class loading and instantiation
 */
public class ExecuteSAILTest extends AbstractH2Test {

	private DataBuilder db;
	private ControlPanelExtension controlPanel;

	@BeforeEach
	public void setup() {
		db = new DataBuilder().fixture(FixtureType.crud);
		controlPanel = db.build(ControlPanel.MODULE_NAME, ControlPanel.DOCUMENT_NAME);
	}

	@SuppressWarnings("boxing")
	@Test
	public void testExecuteSAILWithNullUserThrowsValidationException() {
		// setup the test data - all fields except user
		controlPanel.setSailUser(null);
		controlPanel.setSailBaseUrl("http://localhost:8080");
		controlPanel.setSailExecutor(SailExecutor.primeFacesInlineWebDriver);
		controlPanel.setSailComponentBuilder("org.skyve.impl.web.faces.pipeline.component.SkyveComponentBuilderChain");
		controlPanel.setSailLayoutBuilder("org.skyve.impl.web.faces.pipeline.layout.ResponsiveLayoutBuilder");
		controlPanel.setSail("<automation/>");

		// call the method under test and expect exception
		ValidationException e = assertThrows(ValidationException.class, () -> {
			ExecuteSAIL.executeSAIL(controlPanel);
		});

		// verify the exception contains the sailUser binding
		assertThat(e.getMessages().size(), is(1));
		assertThat(hasBinding(e.getMessages().get(0).getBindings(), ControlPanel.sailUserPropertyName), is(true));
	}

	@SuppressWarnings("boxing")
	@Test
	public void testExecuteSAILWithNullBaseUrlThrowsValidationException() {
		// setup the test data - all fields except baseUrl
		UserProxyExtension user = db.build(UserProxy.MODULE_NAME, UserProxy.DOCUMENT_NAME);
		controlPanel.setSailUser(user);
		controlPanel.setSailBaseUrl(null);
		controlPanel.setSailExecutor(SailExecutor.primeFacesInlineWebDriver);
		controlPanel.setSailComponentBuilder("org.skyve.impl.web.faces.pipeline.component.SkyveComponentBuilderChain");
		controlPanel.setSailLayoutBuilder("org.skyve.impl.web.faces.pipeline.layout.ResponsiveLayoutBuilder");
		controlPanel.setSail("<automation/>");

		// call the method under test and expect exception
		ValidationException e = assertThrows(ValidationException.class, () -> {
			ExecuteSAIL.executeSAIL(controlPanel);
		});

		// verify the exception contains the sailBaseUrl binding
		assertThat(e.getMessages().size(), is(1));
		assertThat(hasBinding(e.getMessages().get(0).getBindings(), ControlPanel.sailBaseUrlPropertyName), is(true));
	}

	@SuppressWarnings("boxing")
	@Test
	public void testExecuteSAILWithNullExecutorThrowsValidationException() {
		// setup the test data - all fields except executor
		UserProxyExtension user = db.build(UserProxy.MODULE_NAME, UserProxy.DOCUMENT_NAME);
		controlPanel.setSailUser(user);
		controlPanel.setSailBaseUrl("http://localhost:8080");
		controlPanel.setSailExecutor(null);
		controlPanel.setSailComponentBuilder("org.skyve.impl.web.faces.pipeline.component.SkyveComponentBuilderChain");
		controlPanel.setSailLayoutBuilder("org.skyve.impl.web.faces.pipeline.layout.ResponsiveLayoutBuilder");
		controlPanel.setSail("<automation/>");

		// call the method under test and expect exception
		ValidationException e = assertThrows(ValidationException.class, () -> {
			ExecuteSAIL.executeSAIL(controlPanel);
		});

		// verify the exception contains the sailExecutor binding
		assertThat(e.getMessages().size(), is(1));
		assertThat(hasBinding(e.getMessages().get(0).getBindings(), ControlPanel.sailExecutorPropertyName), is(true));
	}

	@SuppressWarnings("boxing")
	@Test
	public void testExecuteSAILWithNullComponentBuilderThrowsValidationException() {
		// setup the test data - all fields except componentBuilder
		UserProxyExtension user = db.build(UserProxy.MODULE_NAME, UserProxy.DOCUMENT_NAME);
		controlPanel.setSailUser(user);
		controlPanel.setSailBaseUrl("http://localhost:8080");
		controlPanel.setSailExecutor(SailExecutor.primeFacesInlineWebDriver);
		controlPanel.setSailComponentBuilder(null);
		controlPanel.setSailLayoutBuilder("org.skyve.impl.web.faces.pipeline.layout.ResponsiveLayoutBuilder");
		controlPanel.setSail("<automation/>");

		// call the method under test and expect exception
		ValidationException e = assertThrows(ValidationException.class, () -> {
			ExecuteSAIL.executeSAIL(controlPanel);
		});

		// verify the exception contains the sailComponentBuilder binding
		assertThat(e.getMessages().size(), is(1));
		assertThat(hasBinding(e.getMessages().get(0).getBindings(), ControlPanel.sailComponentBuilderPropertyName), is(true));
	}

	@SuppressWarnings("boxing")
	@Test
	public void testExecuteSAILWithNullLayoutBuilderThrowsValidationException() {
		// setup the test data - all fields except layoutBuilder
		UserProxyExtension user = db.build(UserProxy.MODULE_NAME, UserProxy.DOCUMENT_NAME);
		controlPanel.setSailUser(user);
		controlPanel.setSailBaseUrl("http://localhost:8080");
		controlPanel.setSailExecutor(SailExecutor.primeFacesInlineWebDriver);
		controlPanel.setSailComponentBuilder("org.skyve.impl.web.faces.pipeline.component.SkyveComponentBuilderChain");
		controlPanel.setSailLayoutBuilder(null);
		controlPanel.setSail("<automation/>");

		// call the method under test and expect exception
		ValidationException e = assertThrows(ValidationException.class, () -> {
			ExecuteSAIL.executeSAIL(controlPanel);
		});

		// verify the exception contains the sailLayoutBuilder binding
		assertThat(e.getMessages().size(), is(1));
		assertThat(hasBinding(e.getMessages().get(0).getBindings(), ControlPanel.sailLayoutBuilderPropertyName), is(true));
	}

	@SuppressWarnings("boxing")
	@Test
	public void testExecuteSAILWithNullSailThrowsValidationException() {
		// setup the test data - all fields except sail
		UserProxyExtension user = db.build(UserProxy.MODULE_NAME, UserProxy.DOCUMENT_NAME);
		controlPanel.setSailUser(user);
		controlPanel.setSailBaseUrl("http://localhost:8080");
		controlPanel.setSailExecutor(SailExecutor.primeFacesInlineWebDriver);
		controlPanel.setSailComponentBuilder("org.skyve.impl.web.faces.pipeline.component.SkyveComponentBuilderChain");
		controlPanel.setSailLayoutBuilder("org.skyve.impl.web.faces.pipeline.layout.ResponsiveLayoutBuilder");
		controlPanel.setSail(null);

		// call the method under test and expect exception
		ValidationException e = assertThrows(ValidationException.class, () -> {
			ExecuteSAIL.executeSAIL(controlPanel);
		});

		// verify the exception contains the sail binding
		assertThat(e.getMessages().size(), is(1));
		assertThat(hasBinding(e.getMessages().get(0).getBindings(), ControlPanel.sailPropertyName), is(true));
	}

	@SuppressWarnings("boxing")
	@Test
	public void testExecuteSAILWithMultipleNullFieldsThrowsValidationExceptionWithAllBindings() {
		// setup the test data - multiple null fields
		controlPanel.setSailUser(null);
		controlPanel.setSailBaseUrl(null);
		controlPanel.setSailExecutor(null);
		controlPanel.setSailComponentBuilder(null);
		controlPanel.setSailLayoutBuilder(null);
		controlPanel.setSail(null);

		// call the method under test and expect exception
		ValidationException e = assertThrows(ValidationException.class, () -> {
			ExecuteSAIL.executeSAIL(controlPanel);
		});

		// verify the exception contains all the bindings
		assertThat(e.getMessages().size(), is(1));
		List<String> bindings = toList(e.getMessages().get(0).getBindings());
		assertThat(bindings.size(), is(6));
		assertThat(hasBinding(e.getMessages().get(0).getBindings(), ControlPanel.sailUserPropertyName), is(true));
		assertThat(hasBinding(e.getMessages().get(0).getBindings(), ControlPanel.sailBaseUrlPropertyName), is(true));
		assertThat(hasBinding(e.getMessages().get(0).getBindings(), ControlPanel.sailExecutorPropertyName), is(true));
		assertThat(hasBinding(e.getMessages().get(0).getBindings(), ControlPanel.sailComponentBuilderPropertyName), is(true));
		assertThat(hasBinding(e.getMessages().get(0).getBindings(), ControlPanel.sailLayoutBuilderPropertyName), is(true));
		assertThat(hasBinding(e.getMessages().get(0).getBindings(), ControlPanel.sailPropertyName), is(true));
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
