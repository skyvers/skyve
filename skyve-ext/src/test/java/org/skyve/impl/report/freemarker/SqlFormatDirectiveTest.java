package org.skyve.impl.report.freemarker;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.io.StringReader;
import java.io.StringWriter;
import java.sql.Date;
import java.sql.Timestamp;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.GeometryFactory;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.types.DateOnly;
import org.skyve.domain.types.DateTime;
import org.skyve.domain.types.TimeOnly;
import org.skyve.domain.types.converters.Converter;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.user.User;

import freemarker.template.TemplateDirectiveBody;
import freemarker.template.TemplateModel;
import freemarker.template.TemplateModelException;
import freemarker.template.TemplateScalarModel;
import freemarker.template.Configuration;
import freemarker.template.Template;

@SuppressWarnings({"static-method", "unchecked"})
class SqlFormatDirectiveTest {
	@AfterEach
	void tearDown() throws Exception {
		unbindPersistenceFromThread();
	}

	@Test
	void executeRejectsMissingParameters() {
		SqlFormatDirective directive = new SqlFormatDirective();

		assertThrows(TemplateModelException.class, () -> directive.execute(null, Map.of(), new TemplateModel[0], null));
	}

	@Test
	void executeRejectsLoopVariables() {
		SqlFormatDirective directive = new SqlFormatDirective();
		Map<String, TemplateModel> params = new HashMap<>();
		params.put("value", scalar("abc"));

		assertThrows(TemplateModelException.class, () -> directive.execute(null, params, new TemplateModel[] { scalar("x") }, null));
	}

	@Test
	void executeRejectsNestedBody() {
		SqlFormatDirective directive = new SqlFormatDirective();
		Map<String, TemplateModel> params = new HashMap<>();
		params.put("value", scalar("abc"));
		TemplateDirectiveBody body = mock(TemplateDirectiveBody.class);

		assertThrows(TemplateModelException.class, () -> directive.execute(null, params, new TemplateModel[0], body));
	}

	@Test
	void executeRejectsUnsupportedParameter() {
		SqlFormatDirective directive = new SqlFormatDirective();
		Map<String, TemplateModel> params = new HashMap<>();
		params.put("other", scalar("abc"));

		assertThrows(TemplateModelException.class, () -> directive.execute(null, params, new TemplateModel[0], null));
	}

	@Test
	void toDisplayFormatsBooleanAndGeometryAndConverters() throws Exception {
		Customer customer = mock(Customer.class);
		Converter<DateOnly> dateConverter = mock(Converter.class);
		Converter<DateTime> dateTimeConverter = mock(Converter.class);
		Converter<TimeOnly> timeConverter = mock(Converter.class);
		Converter<org.skyve.domain.types.Timestamp> timestampConverter = mock(Converter.class);
		when(customer.getDefaultDateConverter()).thenReturn(dateConverter);
		when(customer.getDefaultDateTimeConverter()).thenReturn(dateTimeConverter);
		when(customer.getDefaultTimeConverter()).thenReturn(timeConverter);
		when(customer.getDefaultTimestampConverter()).thenReturn(timestampConverter);
		when(dateConverter.toDisplayValue(any(DateOnly.class))).thenReturn("D");
		when(dateTimeConverter.toDisplayValue(any(DateTime.class))).thenReturn("DT");
		when(timeConverter.toDisplayValue(any(TimeOnly.class))).thenReturn("T");
		when(timestampConverter.toDisplayValue(any(org.skyve.domain.types.Timestamp.class))).thenReturn("TS");
		bindPersistenceWithCustomer(customer);

		SqlFormatDirective directive = new SqlFormatDirective();
		Method toDisplay = SqlFormatDirective.class.getDeclaredMethod("toDisplay", Object.class);
		toDisplay.setAccessible(true);

		assertEquals("Yes", toDisplay.invoke(directive, Boolean.TRUE));
		assertEquals("No", toDisplay.invoke(directive, Boolean.FALSE));
		assertEquals("", toDisplay.invoke(directive, new Object[] { null }));
		assertEquals("TS", toDisplay.invoke(directive, new Timestamp(System.currentTimeMillis())));
		assertEquals("D", toDisplay.invoke(directive, new Date(System.currentTimeMillis())));
		assertEquals("D", toDisplay.invoke(directive, new DateOnly(new Date(System.currentTimeMillis()))));
		assertEquals("T", toDisplay.invoke(directive, new TimeOnly(new java.util.Date())));
		assertEquals("DT", toDisplay.invoke(directive, new DateTime(new java.util.Date())));
		assertEquals("TS", toDisplay.invoke(directive, new org.skyve.domain.types.Timestamp(new Timestamp(System.currentTimeMillis()))));
		assertEquals("DT", toDisplay.invoke(directive, new java.util.Date()));
		assertEquals("POINT (1 2)", toDisplay.invoke(directive, new GeometryFactory().createPoint(new Coordinate(1, 2))));
		assertEquals("plain", toDisplay.invoke(directive, "plain"));
	}

	@Test
	void toDisplayThrowsDomainExceptionWithoutCustomer() throws Exception {
		SqlFormatDirective directive = new SqlFormatDirective();
		Method toDisplay = SqlFormatDirective.class.getDeclaredMethod("toDisplay", Object.class);
		toDisplay.setAccessible(true);

		assertThrows(DomainException.class, () -> invokeToDisplay(toDisplay, directive, Boolean.TRUE));
	}

	@Test
	void toDisplayRethrowsSkyveException() throws Exception {
		Customer customer = mock(Customer.class);
		Converter<DateOnly> dateConverter = mock(Converter.class);
		when(customer.getDefaultDateConverter()).thenReturn(dateConverter);
		DomainException expected = new DomainException("expected");
		when(dateConverter.toDisplayValue(any(DateOnly.class))).thenThrow(expected);
		bindPersistenceWithCustomer(customer);

		SqlFormatDirective directive = new SqlFormatDirective();
		Method toDisplay = SqlFormatDirective.class.getDeclaredMethod("toDisplay", Object.class);
		toDisplay.setAccessible(true);

		DateOnly value = new DateOnly(new Date(System.currentTimeMillis()));
		DomainException actual = assertThrows(DomainException.class, () -> invokeToDisplay(toDisplay, directive, value));
		assertEquals(expected, actual);
	}

	@Test
	void executeRendersValueViaFreeMarkerEnvironment() throws Exception {
		Customer customer = mock(Customer.class);
		bindPersistenceWithCustomer(customer);

		String output = renderDirective("<@sqlformat value='plain' />", Map.of());
		assertEquals("plain", output);
	}

	@Test
	void executeSkipsWriteWhenValueUnwrapsToNull() throws Exception {
		Customer customer = mock(Customer.class);
		bindPersistenceWithCustomer(customer);

		Configuration cfg = new Configuration(Configuration.VERSION_2_3_32);
		cfg.setClassicCompatible(true);
		cfg.setLocale(Locale.ROOT);
		cfg.setSharedVariable("sqlformat", new SqlFormatDirective());
		Template template = new Template("sqlformatNull", new StringReader("<@sqlformat value=missingValue />"), cfg);

		StringWriter out = new StringWriter();
		template.process(Map.of(), out);

		assertEquals("", out.toString());
	}

	@Test
	void toDisplayThrowsDomainExceptionWhenThreadLocalUserHasNoCustomer() throws Exception {
		bindPersistenceWithoutCustomer();

		SqlFormatDirective directive = new SqlFormatDirective();
		Method toDisplay = SqlFormatDirective.class.getDeclaredMethod("toDisplay", Object.class);
		toDisplay.setAccessible(true);

		assertThrows(DomainException.class, () -> invokeToDisplay(toDisplay, directive, "plain"));
	}

	private static Object invokeToDisplay(Method toDisplay, SqlFormatDirective directive, Object value) {
		try {
			return toDisplay.invoke(directive, value);
		}
		catch (ReflectiveOperationException e) {
			Throwable cause = e.getCause();
			if (cause instanceof RuntimeException runtime) {
				throw runtime;
			}
			throw new IllegalStateException("Could not invoke toDisplay", e);
		}
	}

	private static String renderDirective(String templateText, Map<String, Object> model) throws Exception {
		Configuration cfg = new Configuration(Configuration.VERSION_2_3_32);
		cfg.setLocale(Locale.ROOT);
		cfg.setSharedVariable("sqlformat", new SqlFormatDirective());
		Template template = new Template("sqlformat", new StringReader(templateText), cfg);
		StringWriter out = new StringWriter();
		template.process(model, out);
		return out.toString();
	}


	private static TemplateScalarModel scalar(String value) {
		return () -> value;
	}

	private static void bindPersistenceWithCustomer(Customer customer) throws Exception {
		AbstractPersistence persistence = mock(AbstractPersistence.class);
		User user = mock(User.class);
		when(user.getCustomer()).thenReturn(customer);
		when(persistence.getUser()).thenReturn(user);
		bindPersistenceToThread(persistence);
	}

	private static void bindPersistenceWithoutCustomer() throws Exception {
		AbstractPersistence persistence = mock(AbstractPersistence.class);
		User user = mock(User.class);
		when(user.getCustomer()).thenReturn(null);
		when(persistence.getUser()).thenReturn(user);
		bindPersistenceToThread(persistence);
	}

	private static void bindPersistenceToThread(AbstractPersistence persistence) throws Exception {
		Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
		field.setAccessible(true);
		((ThreadLocal<AbstractPersistence>) field.get(null)).set(persistence);
	}

	private static void unbindPersistenceFromThread() throws Exception {
		Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
		field.setAccessible(true);
		((ThreadLocal<AbstractPersistence>) field.get(null)).remove();
	}
}
