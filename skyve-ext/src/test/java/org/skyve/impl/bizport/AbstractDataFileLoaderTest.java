package org.skyve.impl.bizport;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.UploadException;
import org.skyve.domain.types.converters.Converter;
import org.skyve.impl.bizport.AbstractDataFileLoader.LoaderActivityType;
import org.skyve.impl.bizport.DataFileField.LoadAction;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.Relation;
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
	void settersAndNullBindingFieldsUpdateLoaderStateWithoutMetadataLookup() throws Exception {
		UploadException first = new UploadException();
		UploadException second = new UploadException();
		bindPersistence(persistence(customer(), null, null));
		TestLoader loader = new TestLoader(LoaderActivityType.CREATE_ALL, first);

		loader.setFieldIndex(3);
		loader.setException(second);
		loader.setActivityType(LoaderActivityType.FIND);
		loader.addField((String) null);
		loader.addField(null, LoadAction.CONFIRM_VALUE, true, null);

		assertSame(second, loader.getException());
		assertThat(loader.getFields().size(), is(2));
		assertThat(loader.getFields().get(0).getIndex(), is(Integer.valueOf(0)));
		assertThat(loader.getFields().get(1).getIndex(), is(Integer.valueOf(1)));
		assertThat(loader.getFields().get(1).getLoadAction(), is(LoadAction.CONFIRM_VALUE));
		assertThat(loader.getFields().get(1).isRequired(), is(true));
	}

	@Test
	void addFieldStripsExpressionBracesAndFinalisesScalarAttribute() throws Exception {
		Customer customer = customer();
		Document document = customer.getModule("sales").getDocument(customer, "Order");
		Attribute name = attribute("name", AttributeType.text);
		doReturn(String.class).when(name).getImplementingType();
		when(document.getAttribute("name")).thenReturn(name);
		bindPersistence(persistence(customer, null, null));
		TestLoader loader = new TestLoader(LoaderActivityType.CREATE_ALL, new UploadException());

		loader.addField("{name}");

		DataFileField field = loader.getFields().get(0);
		assertThat(field.getBinding(), is("name"));
		assertThat(field.getAttribute(), is(name));
		assertThat(field.getLoadAction(), is(LoadAction.SET_VALUE));
		assertThat(field.getIndex(), is(Integer.valueOf(0)));
	}

	@Test
	void addFieldDefaultsCompoundBindingsToLookupEqualsForCreateFind() throws Exception {
		Customer customer = customer();
		Module module = customer.getModule("sales");
		Document order = module.getDocument(customer, "Order");
		Document company = mock(Document.class);
		Relation companyRelation = relation("company", "Company");
		Attribute companyName = attribute("name", AttributeType.text);
		doReturn(String.class).when(companyName).getImplementingType();
		when(order.getAttribute("company")).thenReturn(companyRelation);
		when(module.getDocument(customer, "Company")).thenReturn(company);
		when(company.getOwningModuleName()).thenReturn("sales");
		when(company.getName()).thenReturn("Company");
		when(company.getAttribute("name")).thenReturn(companyName);
		bindPersistence(persistence(customer, null, null));
		TestLoader loader = new TestLoader(LoaderActivityType.CREATE_FIND, new UploadException());

		loader.addField("company.name");

		DataFileField field = loader.getFields().get(0);
		assertThat(field.getBinding(), is("company.name"));
		assertThat(field.getAttribute(), is(companyName));
		assertThat(field.getLoadAction(), is(LoadAction.LOOKUP_EQUALS));
	}

	@Test
	void addFieldRedirectsAssociationBindingToBizKeyLookupContains() throws Exception {
		Customer customer = customer();
		Document document = customer.getModule("sales").getDocument(customer, "Order");
		Relation company = relation("company", "Company");
		when(document.getAttribute("company")).thenReturn(company);
		bindPersistence(persistence(customer, null, null));
		TestLoader loader = new TestLoader(LoaderActivityType.CREATE_ALL, new UploadException());

		loader.addField("company");

		DataFileField field = loader.getFields().get(0);
		assertThat(field.getBinding(), is("company.bizKey"));
		assertThat(field.getAttribute(), is(company));
		assertThat(field.getLoadAction(), is(LoadAction.LOOKUP_CONTAINS));
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
	void beanResultFallsBackToDateConversionWhenConverterReturnsNull() throws Exception {
		UploadException problems = new UploadException();
		Bean bean = mock(Bean.class);
		bindPersistence(persistence(customer(), bean, null));
		TestLoader loader = new TestLoader(LoaderActivityType.CREATE_FIND, problems);
		@SuppressWarnings("rawtypes")
		Converter converter = mock(Converter.class);
		when(converter.fromDisplayValue("yes")).thenReturn(null);
		DataFileField field = field("convertedBinding", AttributeType.dateTime);
		field.setIndex(0);
		field.setConverter(converter);
		loader.getFields().add(field);

		assertSame(bean, loader.beanResult());
		verify(converter).fromDisplayValue("yes");
	}

	@Test
	void beanResultAddsWarningWhenRequiredFieldValueIsMissing() throws Exception {
		UploadException problems = new UploadException();
		Bean bean = mock(Bean.class);
		bindPersistence(persistence(customer(), bean, null));
		TestLoader loader = new TestLoader(LoaderActivityType.CREATE_FIND, problems);
		loader.nullStringIndexes.add(Integer.valueOf(0));
		DataFileField field = field("requiredText", AttributeType.text);
		field.setIndex(0);
		field.setRequired(true);
		loader.getFields().add(field);

		assertSame(bean, loader.beanResult());

		assertThat(problems.getWarnings().iterator().hasNext(), is(true));
		assertThat(problems.getWarnings().iterator().next().getWhat(), containsString("A value was expected"));
	}

	@Test
	void beanResultIgnoresFieldsWithoutBinding() throws Exception {
		UploadException problems = new UploadException();
		Bean bean = mock(Bean.class);
		bindPersistence(persistence(customer(), bean, null));
		TestLoader loader = new TestLoader(LoaderActivityType.CREATE_FIND, problems);
		loader.getFields().add(field(null, AttributeType.text));

		assertSame(bean, loader.beanResult());
		assertThat(problems.hasProblems(), is(false));
	}

	@Test
	void findBeanResultBuildsEqualsLikeAndContainsFilters() throws Exception {
		UploadException problems = new UploadException();
		DocumentFilter filter = mock(DocumentFilter.class);
		when(filter.isEmpty()).thenReturn(false);
		bindPersistence(persistence(customer(), null, filter));
		TestLoader loader = new TestLoader(LoaderActivityType.FIND, problems);
		DataFileField equals = field("name", AttributeType.text);
		equals.setLoadAction(LoadAction.LOOKUP_EQUALS);
		equals.setIndex(0);
		DataFileField like = field("description", AttributeType.text);
		like.setLoadAction(LoadAction.LOOKUP_LIKE);
		like.setIndex(1);
		DataFileField contains = field("notes", AttributeType.text);
		contains.setLoadAction(LoadAction.LOOKUP_CONTAINS);
		contains.setIndex(2);
		loader.getFields().add(equals);
		loader.getFields().add(like);
		loader.getFields().add(contains);

		assertThat(loader.beanResult(), is((Bean) null));

		verify(filter).addEquals("name", "yes");
		verify(filter).addLike("description", "cell-1");
		verify(filter).addLike("notes", "%cell-2%");
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

	@Test
	void beanResultsAddsNonNullBeanResultWithoutNoDataWarning() throws Exception {
		UploadException problems = new UploadException();
		Bean bean = mock(Bean.class);
		bindPersistence(persistence(customer(), bean, null));
		TestLoader loader = new TestLoader(LoaderActivityType.CREATE_ALL, problems);
		loader.hasNext = true;

		List<Bean> results = loader.beanResults();

		assertThat(results, is(List.of(bean)));
		assertThat(hasWarning(problems, "No data has been loaded"), is(false));
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

	private static Relation relation(String name, String documentName) {
		Relation relation = mock(Relation.class);
		when(relation.getName()).thenReturn(name);
		when(relation.getAttributeType()).thenReturn(AttributeType.association);
		when(relation.getLocalisedDisplayName()).thenReturn(name);
		when(relation.getDocumentName()).thenReturn(documentName);
		return relation;
	}

	private static boolean hasWarning(UploadException problems, String warning) {
		for (UploadException.Problem problem : problems.getWarnings()) {
			if (warning.equals(problem.getWhat())) {
				return true;
			}
		}
		return false;
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
		private final Set<Integer> nullStringIndexes = new HashSet<>();

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
			if (nullStringIndexes.contains(Integer.valueOf(index))) {
				return null;
			}
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
