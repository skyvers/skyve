package org.skyve.impl.bizport;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.io.ByteArrayInputStream;
import java.lang.reflect.Field;
import java.nio.charset.StandardCharsets;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.messages.UploadException;
import org.skyve.domain.types.DateTime;
import org.skyve.domain.types.converters.Converter;
import org.skyve.impl.bizport.AbstractDataFileLoader.LoaderActivityType;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;

@SuppressWarnings({"static-method", "unchecked"})
class CSVLoaderTest {
	@AfterEach
	void tearDown() throws Exception {
		unbindPersistenceFromThread();
	}

	@Test
	void readsHeaderRowsStringsNumbersWhereAndDebugData() throws Exception {
		bindPersistenceWithCustomer(customer());
		CSVLoader loader = csv("name,amount\nAlice,12.5\n ,\n");

		assertTrue(loader.hasNextData());
		assertTrue(loader.isNoData());
		assertNull(loader.getValueMap());
		assertThat(loader.getWhere(0), is("Line 0 name."));
		assertThat(loader.debugData(), is("Line 0 Null"));

		loader.nextData();

		assertFalse(loader.isNoData());
		assertThat(loader.getStringFieldValue(0, true), is("Alice"));
		assertThat(loader.getStringFieldValue(0, false), is("Alice"));
		assertThat(loader.getNumericFieldValue(1, false), is(Double.valueOf(12.5)));
		assertThat(loader.debugData(), containsString("(1,name) = Alice"));

		loader.nextData();

		assertNull(loader.getStringFieldValue(0, true));
		assertThat(loader.getStringFieldValue(0, false), is(" "));
		assertThat(loader.getNumericFieldValue(1, false), is(Double.valueOf(0)));
	}

	@Test
	void getDateFieldValueUsesCustomerDateTimeConverter() throws Exception {
		Customer customer = customer();
		Converter<DateTime> converter = mock(Converter.class);
		DateTime expected = new DateTime(1_700_000_000_000L);
		when(customer.getDefaultDateTimeConverter()).thenReturn(converter);
		when(converter.fromDisplayValue("2023-11-14 22:13")).thenReturn(expected);
		bindPersistenceWithCustomer(customer);
		CSVLoader loader = csv("when\n2023-11-14 22:13\n");
		loader.nextData();

		assertThat(loader.getDateFieldValue(0), is(expected));
	}

	@Test
	void getDateFieldValueWrapsConversionFailure() throws Exception {
		Customer customer = customer();
		Converter<DateTime> converter = mock(Converter.class);
		when(customer.getDefaultDateTimeConverter()).thenReturn(converter);
		when(converter.fromDisplayValue("bad")).thenThrow(new IllegalArgumentException("nope"));
		bindPersistenceWithCustomer(customer);
		CSVLoader loader = csv("when\nbad\n");
		loader.nextData();

		assertThrows(DomainException.class, () -> loader.getDateFieldValue(0));
	}

	private static CSVLoader csv(String contents) {
		return new CSVLoader(LoaderActivityType.CREATE_ALL,
				new ByteArrayInputStream(contents.getBytes(StandardCharsets.UTF_8)),
				new UploadException(),
				"sales",
				"Order");
	}

	private static Customer customer() {
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		when(customer.getModule("sales")).thenReturn(module);
		when(module.getDocument(customer, "Order")).thenReturn(document);
		return customer;
	}

	private static void bindPersistenceWithCustomer(Customer customer) throws Exception {
		AbstractPersistence persistence = mock(AbstractPersistence.class);
		User user = mock(User.class);
		when(user.getCustomer()).thenReturn(customer);
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
