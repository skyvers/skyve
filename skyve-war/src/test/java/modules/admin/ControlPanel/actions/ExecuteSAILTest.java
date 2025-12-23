package modules.admin.ControlPanel.actions;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.domain.messages.ValidationException;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import modules.admin.ControlPanel.ControlPanelExtension;
import modules.admin.domain.ControlPanel;
import modules.admin.domain.ControlPanel.SailExecutor;
import modules.admin.domain.UserProxy;

/**
 * Tests for the ExecuteSAIL action validation logic.
 * 
 * Note: The full execution logic is not tested here because it involves:
 * - FacesUtil.setSailFacesContextIfNeeded() (FacesContext setup)
 * - CORE.getPersistence() and CORE.getRepository() (static calls)
 * - XMLMetaData.unmarshalSAILString() (XML parsing)
 * - Dynamic class loading and instantiation
 * 
 * Per test requirements, we avoid mockStatic. Instead, we test the validation
 * logic which can be verified without external dependencies.
 */
public class ExecuteSAILTest {

	private DataBuilder db;
	private ControlPanelExtension controlPanel;

	@BeforeEach
	public void setup() {
		db = new DataBuilder().fixture(FixtureType.crud);
		controlPanel = db.build(ControlPanel.MODULE_NAME, ControlPanel.DOCUMENT_NAME);
	}

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
		assertThat(e.getMessages().get(0).getBindings().contains(ControlPanel.sailUserPropertyName), is(true));
	}

	@Test
	public void testExecuteSAILWithNullBaseUrlThrowsValidationException() {
		// setup the test data - all fields except baseUrl
		UserProxy user = db.build(UserProxy.MODULE_NAME, UserProxy.DOCUMENT_NAME);
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
		assertThat(e.getMessages().get(0).getBindings().contains(ControlPanel.sailBaseUrlPropertyName), is(true));
	}

	@Test
	public void testExecuteSAILWithNullExecutorThrowsValidationException() {
		// setup the test data - all fields except executor
		UserProxy user = db.build(UserProxy.MODULE_NAME, UserProxy.DOCUMENT_NAME);
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
		assertThat(e.getMessages().get(0).getBindings().contains(ControlPanel.sailExecutorPropertyName), is(true));
	}

	@Test
	public void testExecuteSAILWithNullComponentBuilderThrowsValidationException() {
		// setup the test data - all fields except componentBuilder
		UserProxy user = db.build(UserProxy.MODULE_NAME, UserProxy.DOCUMENT_NAME);
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
		assertThat(e.getMessages().get(0).getBindings().contains(ControlPanel.sailComponentBuilderPropertyName), is(true));
	}

	@Test
	public void testExecuteSAILWithNullLayoutBuilderThrowsValidationException() {
		// setup the test data - all fields except layoutBuilder
		UserProxy user = db.build(UserProxy.MODULE_NAME, UserProxy.DOCUMENT_NAME);
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
		assertThat(e.getMessages().get(0).getBindings().contains(ControlPanel.sailLayoutBuilderPropertyName), is(true));
	}

	@Test
	public void testExecuteSAILWithNullSailThrowsValidationException() {
		// setup the test data - all fields except sail
		UserProxy user = db.build(UserProxy.MODULE_NAME, UserProxy.DOCUMENT_NAME);
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
		assertThat(e.getMessages().get(0).getBindings().contains(ControlPanel.sailPropertyName), is(true));
	}

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
		assertThat(e.getMessages().get(0).getBindings().size(), is(6));
		assertThat(e.getMessages().get(0).getBindings().contains(ControlPanel.sailUserPropertyName), is(true));
		assertThat(e.getMessages().get(0).getBindings().contains(ControlPanel.sailBaseUrlPropertyName), is(true));
		assertThat(e.getMessages().get(0).getBindings().contains(ControlPanel.sailExecutorPropertyName), is(true));
		assertThat(e.getMessages().get(0).getBindings().contains(ControlPanel.sailComponentBuilderPropertyName), is(true));
		assertThat(e.getMessages().get(0).getBindings().contains(ControlPanel.sailLayoutBuilderPropertyName), is(true));
		assertThat(e.getMessages().get(0).getBindings().contains(ControlPanel.sailPropertyName), is(true));
	}
}
