package org.skyve.impl.web.service.smartclient;

import static org.mockito.Mockito.when;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Assertions;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.module.ModuleImpl;
import org.skyve.impl.metadata.view.ViewImpl;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.View;

import jakarta.servlet.ServletConfig;
import jakarta.servlet.ServletException;

import java.lang.reflect.Field;
import java.lang.reflect.Method;

/**
 * Test that we can instantiate a ViewJSONManipulator from SmartClientEditServlet from an init parameter.
 */
@SuppressWarnings("resource")
class ViewJSONManipulatorFactoryTest {
	@Mock
	private ServletConfig config;
	
	private AutoCloseable auto;
	
	@BeforeEach
	void setup() {
		auto = MockitoAnnotations.openMocks(this);
	}

	@AfterEach
	void tearDown() throws Exception {
		Field field = SmartClientEditServlet.class.getDeclaredField("MANIPULATOR_CLASS");
		field.setAccessible(true);
		field.set(null, null);

		if (auto != null) {
			auto.close();
		}
	}
	
	@Test
	void testSCESInitWithManipulator() throws ServletException {
		SmartClientEditServlet sces = Assertions.assertDoesNotThrow(SmartClientEditServlet::new);
		when(config.getInitParameter("manipulator")).thenReturn(TestExtendingViewJSONManipulator.class.getCanonicalName());
		sces.init(config);
	}

	@Test
	void testSCESInitWithUnknownManipulatorThrowsServletException() {
		SmartClientEditServlet sces = Assertions.assertDoesNotThrow(SmartClientEditServlet::new);
		when(config.getInitParameter("manipulator")).thenReturn("org.skyve.impl.web.service.smartclient.DoesNotExist");

		Assertions.assertThrows(ServletException.class, () -> sces.init(config));
	}

	@Test
	void testSCESInitWithBlankManipulatorDoesNothing() {
		SmartClientEditServlet sces = Assertions.assertDoesNotThrow(SmartClientEditServlet::new);
		when(config.getInitParameter("manipulator")).thenReturn("   ");

		Assertions.assertDoesNotThrow(() -> sces.init(config));
	}

	@Test
	void testSCESNewManipulatorUsesCustomImplementation() throws Exception {
		SmartClientEditServlet sces = Assertions.assertDoesNotThrow(SmartClientEditServlet::new);
		when(config.getInitParameter("manipulator")).thenReturn(TestExtendingViewJSONManipulator.class.getCanonicalName());
		sces.init(config);

		Method method = newManipulatorMethod();
		Object[] args = newManipulatorArgs();

		Assertions.assertInstanceOf(TestExtendingViewJSONManipulator.class, method.invoke(null, args));
	}

	@Test
	@SuppressWarnings("static-method")
	void testSCESNewManipulatorReturnsDefaultWhenManipulatorClassUnset() throws Exception {
		Method method = newManipulatorMethod();
		Object[] args = newManipulatorArgs();

		Object result = method.invoke(null, args);
		Assertions.assertEquals(ViewJSONManipulator.class, result.getClass());
	}

	@Test
	@SuppressWarnings("static-method")
	void testSCESNewManipulatorWrapsInstantiationErrorsInDomainException() throws Exception {
		Field field = SmartClientEditServlet.class.getDeclaredField("MANIPULATOR_CLASS");
		field.setAccessible(true);
		field.set(null, SmartClientViewRenderer.class);

		Method method = newManipulatorMethod();
		Object[] args = newManipulatorArgs();

		java.lang.reflect.InvocationTargetException ex = Assertions.assertThrows(java.lang.reflect.InvocationTargetException.class,
				() -> method.invoke(null, args));
		Assertions.assertInstanceOf(DomainException.class, ex.getCause());
	}

	private static Method newManipulatorMethod() throws NoSuchMethodException {
		Method method = SmartClientEditServlet.class.getDeclaredMethod("newManipulator",
				User.class,
				Module.class,
				Document.class,
				View.class,
				String.class,
				Bean.class,
				int.class,
				int.class,
				boolean.class);
		method.setAccessible(true);
		return method;
	}

	private static Object[] newManipulatorArgs() {
		User user = org.mockito.Mockito.mock(User.class);
		org.mockito.Mockito.when(user.getCustomer()).thenReturn(new CustomerImpl());
		DocumentImpl document = new DocumentImpl();
		document.setPersistent(new org.skyve.metadata.model.Persistent());
		ModuleImpl module = new ModuleImpl();
		ViewImpl view = new ViewImpl();
		return new Object[] {user,
				module,
			document,
				view,
				"external",
				null,
				Integer.valueOf(7),
				Integer.valueOf(8),
				Boolean.FALSE};
	}
}
