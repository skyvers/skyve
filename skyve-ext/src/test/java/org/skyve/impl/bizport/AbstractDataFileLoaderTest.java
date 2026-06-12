package org.skyve.impl.bizport;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.TreeMap;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.skyve.domain.Bean;
import org.skyve.domain.DynamicBean;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.messages.UploadException;
import org.skyve.domain.types.converters.Converter;
import org.skyve.impl.bizport.AbstractDataFileLoader.LoaderActivityType;
import org.skyve.impl.bizport.DataFileField.LoadAction;
import org.skyve.impl.metadata.model.document.field.Text;
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
import org.skyve.util.Binder;

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
	void addFieldUsesConvertibleFieldConverterWhenNoFieldConverterProvided() throws Exception {
		Customer customer = customer();
		Document document = customer.getModule("sales").getDocument(customer, "Order");
		Text name = new Text();
		name.setName("name");
		name.setDisplayName("name");
		name.setLength(50);
		@SuppressWarnings("rawtypes")
		Converter converter = mock(Converter.class);
		name.setConverter(converter);
		when(document.getAttribute("name")).thenReturn(name);
		bindPersistence(persistence(customer, null, null));
		TestLoader loader = new TestLoader(LoaderActivityType.CREATE_ALL, new UploadException());

		loader.addField("name");

		assertSame(converter, loader.getFields().get(0).getConverter());
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
	void addFieldsInDebugModeFinalisesScalarAndAssociationFields() throws Exception {
		Customer customer = customer();
		Document document = customer.getModule("sales").getDocument(customer, "Order");
		Attribute name = attribute("name", AttributeType.text);
		Relation company = relation("company", "Company");
		doReturn(String.class).when(name).getImplementingType();
		when(document.getAttribute("name")).thenReturn(name);
		when(document.getAttribute("company")).thenReturn(company);
		bindPersistence(persistence(customer, null, null));
		TestLoader loader = new TestLoader(LoaderActivityType.CREATE_ALL, new UploadException());
		loader.setDebugMode(true);

		loader.addFields("name", "company");

		assertThat(loader.getFields().get(0).getBinding(), is("name"));
		assertThat(loader.getFields().get(0).getAttribute(), is(name));
		assertThat(loader.getFields().get(1).getBinding(), is("company.bizKey"));
		assertThat(loader.getFields().get(1).getAttribute(), is(company));
	}

	@Test
	void addDataFileFieldAssignsNextIndexWhenIndexIsNull() throws Exception {
		bindPersistence(persistence(customer(), null, null));
		TestLoader loader = new TestLoader(LoaderActivityType.CREATE_ALL, new UploadException());

		loader.addField(new DataFileField(null));

		assertThat(loader.getFields().get(0).getIndex(), is(Integer.valueOf(0)));
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
		loader.setDebugMode(true);
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
	void beanResultAddsConverterWarningWhenStringValueCannotBeRead() throws Exception {
		UploadException problems = new UploadException();
		Bean bean = mock(Bean.class);
		bindPersistence(persistence(customer(), bean, null));
		TestLoader loader = new TestLoader(LoaderActivityType.CREATE_FIND, problems);
		loader.throwStringIndexes.add(Integer.valueOf(0));
		@SuppressWarnings("rawtypes")
		Converter converter = mock(Converter.class);
		DataFileField field = field("convertedBinding", AttributeType.text);
		field.setIndex(0);
		field.setConverter(converter);
		loader.getFields().add(field);

		assertSame(bean, loader.beanResult());

		assertThat(problems.getWarnings().iterator().next().getWhat(), containsString("using Converter"));
	}

	@Test
	void beanResultAddsErrorWhenNumericValueCannotBeConverted() throws Exception {
		UploadException problems = new UploadException();
		Bean bean = mock(Bean.class);
		bindPersistence(persistence(customer(), bean, null));
		TestLoader loader = new TestLoader(LoaderActivityType.CREATE_FIND, problems);
		loader.throwNumericIndexes.add(Integer.valueOf(0));
		DataFileField field = field("amount", AttributeType.decimal2);
		field.setIndex(0);
		loader.getFields().add(field);

		assertSame(bean, loader.beanResult());

		assertThat(problems.getErrors().iterator().next().getWhat(), containsString("invalid or the wrong type"));
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
	void beanResultAddsErrorWhenBindingHasNoResolvedAttribute() throws Exception {
		UploadException problems = new UploadException();
		Bean bean = mock(Bean.class);
		bindPersistence(persistence(customer(), bean, null));
		TestLoader loader = new TestLoader(LoaderActivityType.CREATE_FIND, problems);
		loader.setDebugMode(true);
		DataFileField field = new DataFileField("missing");
		field.setIndex(0);
		loader.getFields().add(field);

		assertSame(bean, loader.beanResult());

		assertThat(problems.hasErrors(), is(true));
		assertThat(problems.getErrors().iterator().next().getWhat(), containsString("missing"));
	}

	@Test
	void findBeanResultBuildsEqualsLikeAndContainsFilters() throws Exception {
		UploadException problems = new UploadException();
		DocumentFilter filter = mock(DocumentFilter.class);
		when(filter.isEmpty()).thenReturn(false);
		bindPersistence(persistence(customer(), null, filter));
		TestLoader loader = new TestLoader(LoaderActivityType.FIND, problems);
		loader.setDebugMode(true);
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
	void findBeanResultReturnsMatchedBeanWhenFilterIsNotEmpty() throws Exception {
		UploadException problems = new UploadException();
		Bean bean = mock(Bean.class);
		when(bean.getBizKey()).thenReturn("Order 1");
		DocumentFilter filter = mock(DocumentFilter.class);
		when(filter.isEmpty()).thenReturn(false);
		bindPersistence(persistence(customer(), bean, filter));
		TestLoader loader = new TestLoader(LoaderActivityType.FIND, problems);
		loader.setDebugMode(true);
		DataFileField equals = field("name", AttributeType.text);
		equals.setLoadAction(LoadAction.LOOKUP_EQUALS);
		equals.setIndex(0);
		loader.getFields().add(equals);

		assertSame(bean, loader.beanResult());
		verify(filter).addEquals("name", "yes");
	}

	@Test
	void lookupBeanInFindModeThrowsWhenNoExistingBeanMatches() throws Exception {
		Customer customer = customer();
		Module module = customer.getModule("sales");
		Document order = module.getDocument(customer, "Order");
		Document company = mock(Document.class);
		Relation companyRelation = relation("company", "Company");
		when(order.getAttribute("company")).thenReturn(companyRelation);
		when(order.getOwningModuleName()).thenReturn("sales");
		when(order.getName()).thenReturn("Order");
		when(order.getLocalisedSingularAlias()).thenReturn("Order");
		when(order.getLocalisedPluralAlias()).thenReturn("Orders");
		when(module.getDocument(customer, "Company")).thenReturn(company);
		when(company.getOwningModuleName()).thenReturn("sales");
		when(company.getName()).thenReturn("Company");
		when(company.getLocalisedSingularAlias()).thenReturn("Company");
		when(company.getLocalisedPluralAlias()).thenReturn("Companies");
		AbstractPersistence persistence = persistence(customer, null, null);
		DocumentQuery lookup = mock(DocumentQuery.class);
		DocumentFilter filter = mock(DocumentFilter.class);
		when(persistence.newDocumentQuery(anyString(), anyString())).thenReturn(lookup);
		when(lookup.getFilter()).thenReturn(filter);
		when(filter.addEquals("company", "Acme")).thenReturn(filter);
		when(lookup.beanResult()).thenReturn(null);
		bindPersistence(persistence);
		TestLoader loader = new TestLoader(LoaderActivityType.FIND, new UploadException());
		DataFileField field = field("company", AttributeType.association);
		field.setLoadAction(LoadAction.LOOKUP_EQUALS);
		Bean bean = mock(Bean.class);
		StringBuilder problems = new StringBuilder();

		DomainException thrown = assertThrows(DomainException.class,
				() -> loader.lookupBean(bean, field, "Acme", problems));

		assertThat(thrown.getMessage(), containsString("doesn't match any existing Orders"));
		verify(filter).addEquals("company", "Acme");
	}

	@Test
	void lookupBeanSetsFoundReferenceForCreateFindCompoundBinding() throws Exception {
		Customer customer = customer();
		stubCompanyLookupMetadata(customer);
		AbstractPersistence persistence = persistence(customer, null, null);
		DocumentQuery lookup = mock(DocumentQuery.class);
		DocumentFilter filter = mock(DocumentFilter.class);
		Bean found = new DynamicBean("sales", "Company", new TreeMap<>());
		when(persistence.newDocumentQuery("sales", "Company")).thenReturn(lookup);
		when(lookup.getFilter()).thenReturn(filter);
		when(lookup.beanResult()).thenReturn(found);
		bindPersistence(persistence);
		TestLoader loader = new TestLoader(LoaderActivityType.CREATE_FIND, new UploadException());
		DataFileField field = field("company.name", AttributeType.text);
		field.setLoadAction(LoadAction.LOOKUP_CONTAINS);
		TreeMap<String, Object> properties = new TreeMap<>();
		properties.put("company", null);
		Bean order = new DynamicBean("sales", "Order", properties);

		loader.lookupBean(order, field, "Acme", new StringBuilder());

		verify(filter).addLike("name", "%Acme%");
		assertSame(found, Binder.get(order, "company"));
	}

	@Test
	void lookupBeanUsesLikeLookupAndSetsFoundReferenceInDebugMode() throws Exception {
		Customer customer = customer();
		stubCompanyLookupMetadata(customer);
		AbstractPersistence persistence = persistence(customer, null, null);
		DocumentQuery lookup = mock(DocumentQuery.class);
		DocumentFilter filter = mock(DocumentFilter.class);
		TreeMap<String, Object> foundProperties = new TreeMap<>();
		foundProperties.put(Bean.DOCUMENT_ID, "company-1");
		Bean found = new DynamicBean("sales", "Company", foundProperties);
		when(persistence.newDocumentQuery("sales", "Company")).thenReturn(lookup);
		when(lookup.getFilter()).thenReturn(filter);
		when(lookup.beanResult()).thenReturn(found);
		bindPersistence(persistence);
		TestLoader loader = new TestLoader(LoaderActivityType.CREATE_FIND, new UploadException());
		loader.setDebugMode(true);
		DataFileField field = field("company.name", AttributeType.text);
		field.setLoadAction(LoadAction.LOOKUP_LIKE);
		Bean order = dynamicOrder();

		loader.lookupBean(order, field, "Acme", new StringBuilder());

		verify(filter).addLike("name", "Acme");
		assertSame(found, Binder.get(order, "company"));
	}

	@Test
	void lookupBeanConfirmValueRejectsDifferentExistingReference() throws Exception {
		Customer customer = customer();
		stubCompanyLookupMetadata(customer);
		AbstractPersistence persistence = persistence(customer, null, null);
		DocumentQuery lookup = mock(DocumentQuery.class);
		DocumentFilter filter = mock(DocumentFilter.class);
		Bean existing = new DynamicBean("sales", "Company", new TreeMap<>());
		Bean found = new DynamicBean("sales", "Company", new TreeMap<>());
		when(persistence.newDocumentQuery("sales", "Company")).thenReturn(lookup);
		when(lookup.getFilter()).thenReturn(filter);
		when(lookup.beanResult()).thenReturn(found);
		bindPersistence(persistence);
		TestLoader loader = new TestLoader(LoaderActivityType.CREATE_FIND, new UploadException());
		DataFileField field = field("company.name", AttributeType.text);
		field.setLoadAction(LoadAction.CONFIRM_VALUE);
		TreeMap<String, Object> properties = new TreeMap<>();
		properties.put("company", null);
		Bean order = new DynamicBean("sales", "Order", properties);
		Binder.set(order, "company", existing);
		StringBuilder problems = new StringBuilder();

		DomainException thrown = assertThrows(DomainException.class,
				() -> loader.lookupBean(order, field, "Acme", problems));

		verify(filter).addEquals("name", "Acme");
		assertThat(thrown.getMessage(), containsString("doesn't match the existing value"));
	}

	@Test
	void lookupBeanConfirmValueAcceptsMatchingExistingReference() throws Exception {
		Customer customer = customer();
		stubCompanyLookupMetadata(customer);
		AbstractPersistence persistence = persistence(customer, null, null);
		DocumentQuery lookup = mock(DocumentQuery.class);
		DocumentFilter filter = mock(DocumentFilter.class);
		Bean existing = mock(Bean.class);
		when(persistence.newDocumentQuery("sales", "Company")).thenReturn(lookup);
		when(lookup.getFilter()).thenReturn(filter);
		when(lookup.beanResult()).thenReturn(existing);
		bindPersistence(persistence);
		TestLoader loader = new TestLoader(LoaderActivityType.CREATE_FIND, new UploadException());
		DataFileField field = field("company.name", AttributeType.text);
		field.setLoadAction(LoadAction.CONFIRM_VALUE);
		Bean order = dynamicOrder();
		Binder.set(order, "company", existing);

		loader.lookupBean(order, field, "Acme", new StringBuilder());

		verify(filter).addEquals("name", "Acme");
		assertSame(existing, Binder.get(order, "company"));
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
		when(query.beanResult()).thenReturn(newBean);
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

	private static void stubCompanyLookupMetadata(Customer customer) {
		Module module = customer.getModule("sales");
		Document order = module.getDocument(customer, "Order");
		Document company = mock(Document.class);
		Relation companyRelation = relation("company", "Company");
		Text companyName = new Text();
		companyName.setName("name");
		companyName.setDisplayName("name");
		companyName.setLength(50);
		when(order.getAttribute("company")).thenReturn(companyRelation);
		when(order.getOwningModuleName()).thenReturn("sales");
		when(order.getName()).thenReturn("Order");
		when(module.getDocument(customer, "Company")).thenReturn(company);
		when(company.getOwningModuleName()).thenReturn("sales");
		when(company.getName()).thenReturn("Company");
		when(company.getAttribute("name")).thenReturn(companyName);
	}

	private static Bean dynamicOrder() {
		TreeMap<String, Object> properties = new TreeMap<>();
		properties.put("company", null);
		return new DynamicBean("sales", "Order", properties);
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
		private final Set<Integer> throwStringIndexes = new HashSet<>();
		private final Set<Integer> throwNumericIndexes = new HashSet<>();

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
			if (throwStringIndexes.contains(Integer.valueOf(index))) {
				throw new IllegalStateException("bad string");
			}
			if (nullStringIndexes.contains(Integer.valueOf(index))) {
				return null;
			}
			return (index == 0) ? "yes" : "cell-" + index;
		}

		@Override
		Double getNumericFieldValue(int index, boolean emptyAsZero) {
			if (throwNumericIndexes.contains(Integer.valueOf(index))) {
				throw new IllegalStateException("bad number");
			}
			return Double.valueOf(12.5);
		}

		@Override
		Date getDateFieldValue(int index) {
			return new Date(1_700_000_000_000L);
		}
	}
}
