package org.skyve.impl.bizport;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;
import java.util.Date;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.UploadException;
import org.skyve.domain.types.converters.Converter;
import org.skyve.impl.bizport.AbstractDataFileLoader.LoaderActivityType;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.persistence.DocumentFilter;
import org.skyve.persistence.DocumentQuery;

@SuppressWarnings({"static-method", "unchecked", "boxing"})
class AbstractDataFileLoaderTest {
	@AfterEach
	void tearDown() throws Exception {
		unbindPersistenceFromThread();
	}

	@Test
	void accessorsFieldOffsetWhereAndDebugDataUseCurrentLoaderState() throws Exception {
		bindPersistence(persistence(customer(), null, null));
		TestLoader loader = new TestLoader(LoaderActivityType.CREATE_ALL, new UploadException());
		DataFileField first = new DataFileField(null);
		first.setIndex((Integer) null);
		loader.getFields().add(first);
		loader.addField(new DataFileField(null, 4));
		loader.setDataIndex(2);
		loader.setDebugMode(true);
		loader.setEmptyAsZero(true);
		loader.setFieldOffset(3);

		assertThat(loader.isDebugMode(), is(true));
		assertThat(loader.getDataIndex(), is(2));
		assertThat(loader.getFields().get(0).getIndex(), is(Integer.valueOf(3)));
		assertThat(loader.getFields().get(1).getIndex(), is(Integer.valueOf(7)));
		assertThat(loader.getWhere(), is("Row 3."));
		assertThat(loader.getWhere(1), is("Row 3 column 2."));
		assertThat(loader.debugData(), containsString("(2,0) = yes"));
	}

	@Test
	void debugDataReportsNoDataRows() throws Exception {
		bindPersistence(persistence(customer(), null, null));
		TestLoader loader = new TestLoader(LoaderActivityType.CREATE_ALL, new UploadException());
		loader.noData = true;
		loader.setDataIndex(5);

		assertThat(loader.debugData(), is("Row 5 has no data."));
	}

	@Test
	void beanResultAddsErrorWhenNoFieldsWereProvided() throws Exception {
		UploadException problems = new UploadException();
		Bean bean = mock(Bean.class);
		bindPersistence(persistence(customer(), bean, null));
		TestLoader loader = new TestLoader(LoaderActivityType.CREATE_ALL, problems);

		assertSame(bean, loader.beanResult());
		assertThat(problems.hasErrors(), is(true));
		assertThat(problems.getErrors().iterator().next().getWhat(), containsString("No fields were provided"));
	}

	@Test
	void findBeanResultWithNoFiltersReturnsNullAndRecordsMissingFields() throws Exception {
		UploadException problems = new UploadException();
		DocumentFilter filter = mock(DocumentFilter.class);
		when(filter.isEmpty()).thenReturn(true);
		bindPersistence(persistence(customer(), null, filter));
		TestLoader loader = new TestLoader(LoaderActivityType.FIND, problems);

		assertThat(loader.beanResult(), is((Bean) null));
		assertThat(problems.hasErrors(), is(true));
	}

	@Test
	void beanResultConvertsSupportedAttributeTypesBeforeApplyingBindings() throws Exception {
		UploadException problems = new UploadException();
		Bean bean = mock(Bean.class);
		bindPersistence(persistence(customer(), bean, null));
		TestLoader loader = new TestLoader(LoaderActivityType.CREATE_FIND, problems);
		loader.addTestField("boolBinding", AttributeType.bool);
		loader.addTestField("textBinding", AttributeType.text);
		loader.addTestField("dateBinding", AttributeType.date);
		loader.addTestField("dateTimeBinding", AttributeType.dateTime);
		loader.addTestField("decimal10Binding", AttributeType.decimal10);
		loader.addTestField("decimal2Binding", AttributeType.decimal2);
		loader.addTestField("decimal5Binding", AttributeType.decimal5);
		loader.addTestField("integerBinding", AttributeType.integer);
		loader.addTestField("longIntegerBinding", AttributeType.longInteger);
		loader.addTestField("timeBinding", AttributeType.time);
		loader.addTestField("timestampBinding", AttributeType.timestamp);

		assertSame(bean, loader.beanResult());
		assertThat(problems.hasProblems(), is(true));
	}

	@Test
	void beanResultUsesFieldConverterWhenSupplied() throws Exception {
		UploadException problems = new UploadException();
		Bean bean = mock(Bean.class);
		bindPersistence(persistence(customer(), bean, null));
		TestLoader loader = new TestLoader(LoaderActivityType.CREATE_FIND, problems);
		@SuppressWarnings("rawtypes")
		Converter converter = mock(Converter.class);
		when(converter.fromDisplayValue("yes")).thenReturn("converted");
		DataFileField field = field("convertedBinding", AttributeType.text);
		field.setConverter(converter);
		loader.getFields().add(field);

		assertSame(bean, loader.beanResult());
	}

	@Test
	void beanResultsStopsAtNoDataAndAddsWarningWhenNothingLoaded() throws Exception {
		UploadException problems = new UploadException();
		bindPersistence(persistence(customer(), null, null));
		TestLoader loader = new TestLoader(LoaderActivityType.CREATE_ALL, problems);
		loader.hasNext = true;
		loader.noData = true;

		List<Bean> results = loader.beanResults();

		assertThat(results.isEmpty(), is(true));
		assertThat(problems.hasProblems(), is(true));
		assertThat(problems.getWarnings().iterator().next().getWhat(), is("No data has been loaded"));
	}

	private static Customer customer() {
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		when(customer.getModule("sales")).thenReturn(module);
		when(module.getDocument(customer, "Order")).thenReturn(document);
		return customer;
	}

	private static AbstractPersistence persistence(Customer customer, Bean newBean, DocumentFilter filter) throws Exception {
		AbstractPersistence persistence = mock(AbstractPersistence.class);
		User user = mock(User.class);
		when(user.getCustomer()).thenReturn(customer);
		when(persistence.getUser()).thenReturn(user);
		DocumentQuery query = mock(DocumentQuery.class);
		DocumentFilter effectiveFilter = (filter == null) ? mock(DocumentFilter.class) : filter;
		when(query.getFilter()).thenReturn(effectiveFilter);
		when(persistence.newDocumentQuery("sales", "Order")).thenReturn(query);
		Document document = customer.getModule("sales").getDocument(customer, "Order");
		when(document.newInstance(user)).thenReturn(newBean);
		return persistence;
	}

	private static DataFileField field(String binding, AttributeType type) {
		DataFileField field = new DataFileField(binding);
		field.setAttribute(attribute(binding, type));
		return field;
	}

	private static Attribute attribute(String name, AttributeType type) {
		Attribute attribute = mock(Attribute.class);
		when(attribute.getName()).thenReturn(name);
		when(attribute.getAttributeType()).thenReturn(type);
		when(attribute.getLocalisedDisplayName()).thenReturn(name);
		return attribute;
	}

	private static void bindPersistence(AbstractPersistence persistence) throws Exception {
		Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
		field.setAccessible(true);
		((ThreadLocal<AbstractPersistence>) field.get(null)).set(persistence);
	}

	private static void unbindPersistenceFromThread() throws Exception {
		Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
		field.setAccessible(true);
		((ThreadLocal<AbstractPersistence>) field.get(null)).remove();
	}

	private static final class TestLoader extends AbstractDataFileLoader {
		private boolean hasNext;
		private boolean noData;

		private TestLoader(LoaderActivityType activityType, UploadException exception) {
			super(activityType, exception, "sales", "Order");
		}

		@Override
		boolean hasNextData() {
			return hasNext;
		}

		@Override
		void nextData() {
			hasNext = false;
		}

		@Override
		boolean isNoData() {
			return noData;
		}

		private void addTestField(String binding, AttributeType type) {
			getFields().add(field(binding, type));
		}

		@Override
		String getStringFieldValue(int index, boolean emptyAsNull) {
			return (index == 0) ? "yes" : "cell-" + index;
		}

		@Override
		Double getNumericFieldValue(int index, boolean emptyAsZero) {
			return Double.valueOf(12.5);
		}

		@Override
		Date getDateFieldValue(int index) {
			return new Date(1_700_000_000_000L);
		}
	}
}
