package org.skyve.impl.metadata.model.document.field;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.skyve.domain.types.converters.Converter;
import org.skyve.metadata.ConverterName;
import org.skyve.metadata.customer.Customer;

@ExtendWith(MockitoExtension.class)
public class ConvertibleFieldTest {

	@Mock
	private Converter<?> mockConverter;

	@Mock
	private Customer customer;

	@Test
	@SuppressWarnings("static-method")
	public void setConverterNameRoundtrip() {
		ConvertibleField field = new org.skyve.impl.metadata.model.document.field.Date();
		field.setConverterName(ConverterName.DD_MM_YYYY);
		assertThat(field.getConverterName(), is(ConverterName.DD_MM_YYYY));
	}

	@Test
	@SuppressWarnings("static-method")
	public void defaultConverterNameIsNull() {
		ConvertibleField field = new org.skyve.impl.metadata.model.document.field.Date();
		assertNull(field.getConverterName());
	}

	@Test
	public void setConverterRoundtrip() {
		ConvertibleField field = new org.skyve.impl.metadata.model.document.field.Date();
		field.setConverter(mockConverter);
		assertThat(field.getConverter(), is(mockConverter));
	}

	@Test
	@SuppressWarnings("static-method")
	public void defaultConverterIsNull() {
		ConvertibleField field = new org.skyve.impl.metadata.model.document.field.Date();
		assertNull(field.getConverter());
	}

	@Test
	public void getConverterForCustomerReturnsSetConverter() {
		ConvertibleField field = new org.skyve.impl.metadata.model.document.field.Date();
		field.setConverter(mockConverter);
		assertThat(field.getConverterForCustomer(customer), is(mockConverter));
		Mockito.verifyNoInteractions(customer);
	}

	@Test
	public void getConverterForCustomerWithDateTypeDelegatesToCustomer() {
		ConvertibleField field = new org.skyve.impl.metadata.model.document.field.Date();
		// no converter set — should delegate to customer default
		field.getConverterForCustomer(customer);
		Mockito.verify(customer).getDefaultDateConverter();
	}

	@Test
	public void getConverterForCustomerWithTimeTypeDelegatesToCustomer() {
		ConvertibleField field = new org.skyve.impl.metadata.model.document.field.Time();
		field.getConverterForCustomer(customer);
		Mockito.verify(customer).getDefaultTimeConverter();
	}

	@Test
	public void getConverterForCustomerWithDateTimeTypeDelegatesToCustomer() {
		ConvertibleField field = new org.skyve.impl.metadata.model.document.field.DateTime();
		field.getConverterForCustomer(customer);
		Mockito.verify(customer).getDefaultDateTimeConverter();
	}

	@Test
	public void getConverterForCustomerWithTimestampTypeDelegatesToCustomer() {
		ConvertibleField field = new org.skyve.impl.metadata.model.document.field.Timestamp();
		field.getConverterForCustomer(customer);
		Mockito.verify(customer).getDefaultTimestampConverter();
	}

	@Test
	@SuppressWarnings("static-method")
	public void getConverterForCustomerWithNonTemporalTypeReturnsNull() {
		ConvertibleField field = new org.skyve.impl.metadata.model.document.field.Text();
		// Text type is not temporal, no converter default — returns null
		assertNull(field.getConverterForCustomer(null));
	}
}
