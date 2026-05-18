package org.skyve.util;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.withSettings;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.util.Date;
import java.util.Map;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.impl.domain.AbstractBean;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.domain.DynamicBean;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.Attribute.UsageType;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.DomainType;
import org.skyve.metadata.model.Dynamic;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.model.document.Reference;
import org.skyve.metadata.repository.ProvidedRepository;
import org.skyve.metadata.user.User;
import org.skyve.domain.Bean;
import org.skyve.util.test.SkyveFixture;
import org.skyve.util.test.SkyveFixture.FixtureType;

@SuppressWarnings("static-method")
class DataBuilderTest {

	private static DataBuilder newBuilderWithMockUser() {
		User user = mock(User.class);
		when(user.getCustomer()).thenReturn(mock(org.skyve.metadata.customer.Customer.class));
		AbstractPersistence persistence = mock(AbstractPersistence.class, withSettings().defaultAnswer(CALLS_REAL_METHODS));
		persistence.setUser(user);
		persistence.setForThread();
		return new DataBuilder();
	}

	private static final class TestFactory {
		private final DynamicBean fixtureByType;
		private final DynamicBean fixtureByName;
		private final DynamicBean fallback;

		private TestFactory(DynamicBean fixtureByType, DynamicBean fixtureByName, DynamicBean fallback) {
			this.fixtureByType = fixtureByType;
			this.fixtureByName = fixtureByName;
			this.fallback = fallback;
		}

		@SkyveFixture(types = { FixtureType.crud })
		public DynamicBean forCrud() {
			return fixtureByType;
		}

		@SkyveFixture(names = { "seedA" })
		public DynamicBean forSeedA() {
			return fixtureByName;
		}

		@SuppressWarnings("unused")
		public DynamicBean defaultBean() {
			return fallback;
		}

		@SkyveFixture(types = { FixtureType.sail })
		public String wrongReturnType() {
			return "ignored";
		}

		@SuppressWarnings("unused")
		public DynamicBean wrongArgs(String ignored) {
			return fallback;
		}
	}

	private static final class NoEligibleFactory {
		@SuppressWarnings("unused")
		public String onlyString() {
			return "x";
		}

		@SuppressWarnings("unused")
		public Bean withArgs(String ignored) {
			return null;
		}
	}

	public static final class RandomScalarBean extends AbstractBean {
		private static final long serialVersionUID = 1L;
		private String bizId;
		private String bizCustomer;
		private String bizDataGroupId;
		private String bizUserId;
		private Boolean boolValue;
		private String colourValue;
		private String idValue;
		private Date dateValue;
		private Integer integerValue;
		private Long longValue;

		@Override
		public String getBizId() {
			return bizId;
		}

		public void setBizIdValue(String bizId) {
			this.bizId = bizId;
		}

		@Override
		public String getBizModule() {
			return "admin";
		}

		@Override
		public String getBizDocument() {
			return "Contact";
		}

		@Override
		public String getBizCustomer() {
			return bizCustomer;
		}

		@Override
		public void setBizCustomer(String bizCustomer) {
			this.bizCustomer = bizCustomer;
		}

		@Override
		public String getBizDataGroupId() {
			return bizDataGroupId;
		}

		@Override
		public void setBizDataGroupId(String bizDataGroupId) {
			this.bizDataGroupId = bizDataGroupId;
		}

		@Override
		public String getBizUserId() {
			return bizUserId;
		}

		@Override
		public void setBizUserId(String bizUserId) {
			this.bizUserId = bizUserId;
		}

		@Override
		public String getBizKey() {
			return null;
		}

		public Boolean getBoolValue() {
			return boolValue;
		}

		public void setBoolValue(Boolean boolValue) {
			this.boolValue = boolValue;
		}

		public String getColourValue() {
			return colourValue;
		}

		public void setColourValue(String colourValue) {
			this.colourValue = colourValue;
		}

		public String getIdValue() {
			return idValue;
		}

		public void setIdValue(String idValue) {
			this.idValue = idValue;
		}

		public Date getDateValue() {
			return dateValue;
		}

		public void setDateValue(Date dateValue) {
			this.dateValue = dateValue;
		}

		public Integer getIntegerValue() {
			return integerValue;
		}

		public void setIntegerValue(Integer integerValue) {
			this.integerValue = integerValue;
		}

		public Long getLongValue() {
			return longValue;
		}

		public void setLongValue(Long longValue) {
			this.longValue = longValue;
		}
	}

	private static Object getField(Object target, String fieldName) {
		try {
			Field field = DataBuilder.class.getDeclaredField(fieldName);
			field.setAccessible(true);
			return field.get(target);
		}
		catch (ReflectiveOperationException e) {
			throw new AssertionError(e);
		}
	}

	private static void clearPersistenceThreadLocal() {
		try {
			Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
			field.setAccessible(true);
			@SuppressWarnings("unchecked")
			ThreadLocal<AbstractPersistence> threadLocal = (ThreadLocal<AbstractPersistence>) field.get(null);
			threadLocal.remove();
		}
		catch (ReflectiveOperationException e) {
			throw new AssertionError(e);
		}
	}

	private static boolean invokeFilter(DataBuilder builder, Attribute attribute) {
		try {
			java.lang.reflect.Method method = DataBuilder.class.getDeclaredMethod("filter", Attribute.class);
			method.setAccessible(true);
			return ((Boolean) method.invoke(builder, attribute)).booleanValue();
		}
		catch (ReflectiveOperationException e) {
			throw new AssertionError(e);
		}
	}

	@SuppressWarnings("boxing")
	private static Attribute scalarAttribute(String name, boolean required, boolean persistent, UsageType usage, boolean deprecated) {
		Attribute attribute = mock(Attribute.class);
		when(attribute.getName()).thenReturn(name);
		when(attribute.getAttributeType()).thenReturn(AttributeType.text);
		when(attribute.isRequired()).thenReturn(required);
		when(attribute.isPersistent()).thenReturn(persistent);
		when(attribute.getUsage()).thenReturn(usage);
		when(attribute.isDeprecated()).thenReturn(deprecated);
		return attribute;
	}

	@SuppressWarnings("boxing")
	private static Reference referenceAttribute(String name, boolean required, boolean persistent) {
		Reference attribute = mock(Reference.class);
		when(attribute.getName()).thenReturn(name);
		when(attribute.getAttributeType()).thenReturn(AttributeType.association);
		when(attribute.isRequired()).thenReturn(required);
		when(attribute.isPersistent()).thenReturn(persistent);
		when(attribute.getUsage()).thenReturn(UsageType.both);
		when(attribute.isDeprecated()).thenReturn(false);
		return attribute;
	}

	private static Document mockBuildableDocument(String moduleName, String documentName, Bean bean) {
		Document document = mock(Document.class);
		when(document.getOwningModuleName()).thenReturn(moduleName);
		when(document.getName()).thenReturn(documentName);
		try {
			when(document.newInstance(org.mockito.ArgumentMatchers.any(User.class))).thenReturn(bean);
		}
		catch (Exception e) {
			throw new AssertionError(e);
		}
		when(document.getAllAttributes(org.mockito.ArgumentMatchers.any())).thenReturn(java.util.Collections.emptyList());
		return document;
	}

	@Test
	void constructorAppliesCrudFixtureDefaults() {
		try {
			DataBuilder builder = newBuilderWithMockUser();

			assertThat(getField(builder, "fixture"), is("crud"));
			assertThat(getField(builder, "depth"), is((Object) Integer.valueOf(Integer.MAX_VALUE)));
			assertThat(getField(builder, "optionalReferences"), is((Object) Boolean.FALSE));
		}
		finally {
			clearPersistenceThreadLocal();
		}
	}

	@Test
	void fixtureTypeResetsDepthAndOptionalReferences() {
		try {
			DataBuilder builder = newBuilderWithMockUser();

			builder.fixture(FixtureType.sail);
			assertThat(getField(builder, "fixture"), is("sail"));
			assertThat(getField(builder, "depth"), is((Object) Integer.valueOf(0)));
			assertThat(getField(builder, "optionalReferences"), is((Object) Boolean.TRUE));

			builder.fixture(FixtureType.crud);
			assertThat(getField(builder, "depth"), is((Object) Integer.valueOf(Integer.MAX_VALUE)));
			assertThat(getField(builder, "optionalReferences"), is((Object) Boolean.FALSE));
		}
		finally {
			clearPersistenceThreadLocal();
		}
	}

	@Test
	void singleFlagRequiredAndOptionalAffectScalarAndReferenceFlags() {
		try {
			DataBuilder builder = newBuilderWithMockUser();

			builder.required(false);
			assertThat(getField(builder, "requiredScalars"), is((Object) Boolean.FALSE));
			assertThat(getField(builder, "requiredReferences"), is((Object) Boolean.FALSE));

			builder.optional(false);
			assertThat(getField(builder, "optionalScalars"), is((Object) Boolean.FALSE));
			assertThat(getField(builder, "optionalReferences"), is((Object) Boolean.FALSE));
		}
		finally {
			clearPersistenceThreadLocal();
		}
	}

	@Test
	void fluentConfigurationUpdatesInternalFilters() {
		try {
			DataBuilder builder = newBuilderWithMockUser();

			builder.fixture("seedA")
					.required(false, true)
					.optional(true, false)
					.persistent(false)
					.transients(false)
					.view(false)
					.domain(false)
					.deprecated(false)
					.type(AttributeType.text, false)
					.name("myField", false)
					.cardinality("children", 2)
					.depth(3)
					.depth("association", 1);

			assertThat(getField(builder, "fixture"), is("seedA"));
			assertThat(getField(builder, "requiredScalars"), is((Object) Boolean.FALSE));
			assertThat(getField(builder, "requiredReferences"), is((Object) Boolean.TRUE));
			assertThat(getField(builder, "optionalScalars"), is((Object) Boolean.TRUE));
			assertThat(getField(builder, "optionalReferences"), is((Object) Boolean.FALSE));
			assertThat(getField(builder, "persistent"), is((Object) Boolean.FALSE));
			assertThat(getField(builder, "transients"), is((Object) Boolean.FALSE));
			assertThat(getField(builder, "view"), is((Object) Boolean.FALSE));
			assertThat(getField(builder, "domain"), is((Object) Boolean.FALSE));
			assertThat(getField(builder, "deprecated"), is((Object) Boolean.FALSE));
			assertThat(getField(builder, "depth"), is((Object) Integer.valueOf(3)));

			@SuppressWarnings("unchecked")
			Map<AttributeType, Boolean> types = (Map<AttributeType, Boolean>) getField(builder, "types");
			assertThat(types.get(AttributeType.text), is(Boolean.FALSE));

			@SuppressWarnings("unchecked")
			Map<String, Boolean> names = (Map<String, Boolean>) getField(builder, "names");
			assertThat(names.get("myField"), is(Boolean.FALSE));

			@SuppressWarnings("unchecked")
			Map<String, Integer> cardinalities = (Map<String, Integer>) getField(builder, "cardinalities");
			assertThat(cardinalities.get("children"), is(Integer.valueOf(2)));

			@SuppressWarnings("unchecked")
			Map<String, Integer> depths = (Map<String, Integer>) getField(builder, "depths");
			assertThat(depths.get("association"), is(Integer.valueOf(1)));
		}
		finally {
			clearPersistenceThreadLocal();
		}
	}

	@Test
	void filterExcludesRequiredReferenceWhenRequiredReferencesDisabled() {
		try {
			DataBuilder builder = newBuilderWithMockUser().required(true, false);
			Reference attribute = referenceAttribute("manager", true, true);
			assertThat(Boolean.valueOf(invokeFilter(builder, attribute)), is(Boolean.TRUE));
		}
		finally {
			clearPersistenceThreadLocal();
		}
	}

	@Test
	void filterExcludesOptionalScalarWhenOptionalScalarsDisabled() {
		try {
			DataBuilder builder = newBuilderWithMockUser().optional(false, true);
			Attribute attribute = scalarAttribute("nickname", false, true, UsageType.both, false);
			assertThat(Boolean.valueOf(invokeFilter(builder, attribute)), is(Boolean.TRUE));
		}
		finally {
			clearPersistenceThreadLocal();
		}
	}

	@Test
	void filterExcludesTransientWhenTransientsDisabled() {
		try {
			DataBuilder builder = newBuilderWithMockUser().transients(false);
			Attribute attribute = scalarAttribute("computed", false, false, UsageType.both, false);
			assertThat(Boolean.valueOf(invokeFilter(builder, attribute)), is(Boolean.TRUE));
		}
		finally {
			clearPersistenceThreadLocal();
		}
	}

	@Test
	void filterExcludesViewUsageWhenViewDisabled() {
		try {
			DataBuilder builder = newBuilderWithMockUser().view(false);
			Attribute attribute = scalarAttribute("viewOnly", false, true, UsageType.view, false);
			assertThat(Boolean.valueOf(invokeFilter(builder, attribute)), is(Boolean.TRUE));
		}
		finally {
			clearPersistenceThreadLocal();
		}
	}

	@Test
	void filterExcludesDomainUsageWhenDomainDisabled() {
		try {
			DataBuilder builder = newBuilderWithMockUser().domain(false);
			Attribute attribute = scalarAttribute("domainOnly", false, true, UsageType.domain, false);
			assertThat(Boolean.valueOf(invokeFilter(builder, attribute)), is(Boolean.TRUE));
		}
		finally {
			clearPersistenceThreadLocal();
		}
	}

	@Test
	void filterExcludesDeprecatedWhenDeprecatedDisabled() {
		try {
			DataBuilder builder = newBuilderWithMockUser().deprecated(false);
			Attribute attribute = scalarAttribute("legacy", false, true, UsageType.both, true);
			assertThat(Boolean.valueOf(invokeFilter(builder, attribute)), is(Boolean.TRUE));
		}
		finally {
			clearPersistenceThreadLocal();
		}
	}

	@Test
	void filterExcludesByConfiguredTypeAndName() {
		try {
			DataBuilder builder = newBuilderWithMockUser().type(AttributeType.text, false).name("blocked", false);
			Attribute byType = scalarAttribute("other", false, true, UsageType.both, false);
			Attribute byName = scalarAttribute("blocked", false, true, UsageType.both, false);

			assertThat(Boolean.valueOf(invokeFilter(builder, byType)), is(Boolean.TRUE));
			assertThat(Boolean.valueOf(invokeFilter(builder, byName)), is(Boolean.TRUE));
		}
		finally {
			clearPersistenceThreadLocal();
		}
	}

	@Test
	void filterAllowsAttributeWhenNoExclusionRuleMatches() {
		try {
			DataBuilder builder = newBuilderWithMockUser();
			Attribute attribute = scalarAttribute("name", true, true, UsageType.both, false);
			assertThat(Boolean.valueOf(invokeFilter(builder, attribute)), is(Boolean.FALSE));
		}
		finally {
			clearPersistenceThreadLocal();
		}
	}

	@Test
	void factoryBuildWithModuleAndDocumentBuildsBeanWithoutFactoryRecursion() {
		try {
			DataBuilder builder = newBuilderWithMockUser();
			Module module = mock(Module.class);
			Bean bean = mock(Bean.class);
			when(bean.getBizId()).thenReturn("B1");
			Document document = mockBuildableDocument("admin", "Contact", bean);

			Bean result = builder.factoryBuild(module, document);
			assertThat(result, is(bean));
		}
		finally {
			clearPersistenceThreadLocal();
		}
	}

	private static Bean invokeDataFactory(DataBuilder builder, DocumentImpl document, int currentDepth) {
		try {
			Method method = DataBuilder.class.getDeclaredMethod("dataFactory", DocumentImpl.class, int.class);
			method.setAccessible(true);
			return (Bean) method.invoke(builder, document, Integer.valueOf(currentDepth));
		}
		catch (ReflectiveOperationException e) {
			throw new AssertionError(e);
		}
	}

	private static String invokeRandomDomainValue(Customer customer, Document document, Attribute attribute, Bean bean) {
		try {
			Method method = DataBuilder.class.getDeclaredMethod("randomDomainValue", Customer.class, Document.class, Attribute.class, Bean.class);
			method.setAccessible(true);
			return (String) method.invoke(null, customer, document, attribute, bean);
		}
		catch (ReflectiveOperationException e) {
			throw new AssertionError(e);
		}
	}

	private static ProvidedRepository getProvidedRepository() {
		try {
			Field field = ProvidedRepositoryFactory.class.getDeclaredField("repository");
			field.setAccessible(true);
			return (ProvidedRepository) field.get(null);
		}
		catch (ReflectiveOperationException e) {
			throw new AssertionError(e);
		}
	}

	private static void setProvidedRepository(ProvidedRepository repository) {
		try {
			Field field = ProvidedRepositoryFactory.class.getDeclaredField("repository");
			field.setAccessible(true);
			field.set(null, repository);
		}
		catch (ReflectiveOperationException e) {
			throw new AssertionError(e);
		}
	}

	private static void stubConstantDomainValues(CustomerImpl customer, Attribute attribute, java.util.List<DomainValue> values) {
		try {
			when(customer.getConstantDomainValues(null, "admin", "Contact", attribute)).thenReturn(values);
		}
		catch (Exception e) {
			throw new AssertionError(e);
		}
	}

	private static Bean invokeRandomBean(DataBuilder builder, Module module, Document document) {
		try {
			Method method = DataBuilder.class.getDeclaredMethod("randomBean", Module.class, Document.class);
			method.setAccessible(true);
			return (Bean) method.invoke(builder, module, document);
		}
		catch (ReflectiveOperationException e) {
			throw new AssertionError(e);
		}
	}

	@Test
	void factoryBuildWithDocumentResolvesModuleFromCustomer() {
		try {
			DataBuilder builder = newBuilderWithMockUser();
			Customer customer = (Customer) getField(builder, "customer");
			Module module = mock(Module.class);
			Bean bean = mock(Bean.class);
			when(bean.getBizId()).thenReturn("B2");
			Document document = mockBuildableDocument("admin", "Contact", bean);

			when(customer.getModule("admin")).thenReturn(module);
			Bean result = builder.factoryBuild(document);
			assertThat(result, is(bean));
		}
		finally {
			clearPersistenceThreadLocal();
		}
	}

	@Test
	void factoryBuildWithModuleAndNameResolvesDocument() {
		try {
			DataBuilder builder = newBuilderWithMockUser();
			Customer customer = (Customer) getField(builder, "customer");
			Module module = mock(Module.class);
			Bean bean = mock(Bean.class);
			when(bean.getBizId()).thenReturn("B3");
			Document document = mockBuildableDocument("admin", "Contact", bean);

			when(customer.getModule("admin")).thenReturn(module);
			when(module.getDocument(customer, "Contact")).thenReturn(document);

			Bean result = builder.factoryBuild("admin", "Contact");
			assertThat(result, is(bean));
		}
		finally {
			clearPersistenceThreadLocal();
		}
	}

	@Test
	void dataFactorySelectsTypeAnnotatedMethodForFixture() {
		ProvidedRepository original = getProvidedRepository();
		try {
			DataBuilder builder = newBuilderWithMockUser();
			Customer customer = (Customer) getField(builder, "customer");
			DynamicBean typeBean = mock(DynamicBean.class);
			DynamicBean nameBean = mock(DynamicBean.class);
			DynamicBean fallbackBean = mock(DynamicBean.class);
			TestFactory factory = new TestFactory(typeBean, nameBean, fallbackBean);

			ProvidedRepository repository = mock(ProvidedRepository.class);
			when(repository.getDataFactory(customer, "admin", "Contact")).thenReturn(factory);
			setProvidedRepository(repository);

			DocumentImpl document = new DocumentImpl();
			document.setOwningModuleName("admin");
			document.setName("Contact");
			document.setDynamism(mock(Dynamic.class));

			Bean result = invokeDataFactory(builder, document, 1);
			assertThat(result, is(typeBean));
		}
		finally {
			setProvidedRepository(original);
			clearPersistenceThreadLocal();
		}
	}

	@Test
	void dataFactorySelectsNameAnnotatedMethodForFixtureName() {
		ProvidedRepository original = getProvidedRepository();
		try {
			DataBuilder builder = newBuilderWithMockUser().fixture("seedA");
			Customer customer = (Customer) getField(builder, "customer");
			DynamicBean typeBean = mock(DynamicBean.class);
			DynamicBean nameBean = mock(DynamicBean.class);
			DynamicBean fallbackBean = mock(DynamicBean.class);
			TestFactory factory = new TestFactory(typeBean, nameBean, fallbackBean);

			ProvidedRepository repository = mock(ProvidedRepository.class);
			when(repository.getDataFactory(customer, "admin", "Contact")).thenReturn(factory);
			setProvidedRepository(repository);

			DocumentImpl document = new DocumentImpl();
			document.setOwningModuleName("admin");
			document.setName("Contact");
			document.setDynamism(mock(Dynamic.class));

			Bean result = invokeDataFactory(builder, document, 1);
			assertThat(result, is(nameBean));
		}
		finally {
			setProvidedRepository(original);
			clearPersistenceThreadLocal();
		}
	}

	@Test
	void dataFactoryFallsBackToUnannotatedMethodWhenNoFixtureMatch() {
		ProvidedRepository original = getProvidedRepository();
		try {
			DataBuilder builder = newBuilderWithMockUser().fixture("not-present");
			Customer customer = (Customer) getField(builder, "customer");
			DynamicBean typeBean = mock(DynamicBean.class);
			DynamicBean nameBean = mock(DynamicBean.class);
			DynamicBean fallbackBean = mock(DynamicBean.class);
			TestFactory factory = new TestFactory(typeBean, nameBean, fallbackBean);

			ProvidedRepository repository = mock(ProvidedRepository.class);
			when(repository.getDataFactory(customer, "admin", "Contact")).thenReturn(factory);
			setProvidedRepository(repository);

			DocumentImpl document = new DocumentImpl();
			document.setOwningModuleName("admin");
			document.setName("Contact");
			document.setDynamism(mock(Dynamic.class));

			Bean result = invokeDataFactory(builder, document, 1);
			assertThat(result, is(fallbackBean));
		}
		finally {
			setProvidedRepository(original);
			clearPersistenceThreadLocal();
		}
	}

	@Test
	void dataFactoryReturnsNullWhenNoFactoryExistsOrNoEligibleMethods() {
		ProvidedRepository original = getProvidedRepository();
		try {
			DataBuilder builder = newBuilderWithMockUser().fixture("seedA");
			Customer customer = (Customer) getField(builder, "customer");

			ProvidedRepository repository = mock(ProvidedRepository.class);
			when(repository.getDataFactory(customer, "admin", "Contact")).thenReturn(null, new NoEligibleFactory());
			setProvidedRepository(repository);

			DocumentImpl document = new DocumentImpl();
			document.setOwningModuleName("admin");
			document.setName("Contact");
			document.setDynamism(mock(Dynamic.class));

			assertThat(invokeDataFactory(builder, document, 1), is((Bean) null));
			assertThat(invokeDataFactory(builder, document, 1), is((Bean) null));
		}
		finally {
			setProvidedRepository(original);
			clearPersistenceThreadLocal();
		}
	}

	@Test
	void traceHelpersAreInvokableAndProduceIndentedOutput() {
		try {
			DataBuilder.setTrace(true);
			assertThat(getField(DataBuilder.class, "trace"), is((Object) Boolean.TRUE));

			Method method = DataBuilder.class.getDeclaredMethod("trace", StringBuilder.class, int.class);
			method.setAccessible(true);

			PrintStream originalOut = System.out;
			ByteArrayOutputStream out = new ByteArrayOutputStream();
			System.setOut(new PrintStream(out));
			try {
				method.invoke(null, new StringBuilder("line"), Integer.valueOf(2));
			}
			finally {
				System.setOut(originalOut);
			}

			assertThat(out.toString(), is("        line\n"));
		}
		catch (ReflectiveOperationException e) {
			throw new AssertionError(e);
		}
		finally {
			DataBuilder.setTrace(false);
			assertThat(getField(DataBuilder.class, "trace"), is((Object) Boolean.FALSE));
		}
	}

	@Test
	void buildOverloadsDelegateAndFailFastForNonDocumentImplInput() {
		try {
			DataBuilder builder = newBuilderWithMockUser();
			Customer customer = (Customer) getField(builder, "customer");
			Module module = mock(Module.class);
			Document document = mockBuildableDocument("admin", "Contact", mock(Bean.class));

			when(customer.getModule("admin")).thenReturn(module);
			when(module.getDocument(customer, "Contact")).thenReturn(document);

			assertThrows(org.skyve.metadata.MetaDataException.class, () -> builder.build(module, document));
			assertThrows(org.skyve.metadata.MetaDataException.class, () -> builder.build(document));
			assertThrows(org.skyve.metadata.MetaDataException.class, () -> builder.build("admin", "Contact"));
		}
		finally {
			clearPersistenceThreadLocal();
		}
	}

	@Test
	void buildOverloadsReachRuntimePathsAndFailWithoutCdiContainer() {
		ProvidedRepository original = getProvidedRepository();
		try {
			CustomerImpl customer = new CustomerImpl();
			customer.setName("cust");
			User user = mock(User.class);
			when(user.getCustomer()).thenReturn(customer);
			when(user.getDataGroupId()).thenReturn("DG");
			when(user.getId()).thenReturn("U1");

			AbstractPersistence persistence = mock(AbstractPersistence.class, withSettings().defaultAnswer(CALLS_REAL_METHODS));
			persistence.setUser(user);
			persistence.setForThread();

			DataBuilder builder = new DataBuilder().depth(0);
			Module module = mock(Module.class);
			when(module.getName()).thenReturn("admin");
			DocumentImpl document = new DocumentImpl();
			document.setOwningModuleName("admin");
			document.setName("Contact");
			document.setDynamism(mock(Dynamic.class));
			when(module.getDocument(customer, "Contact")).thenReturn(document);

			ProvidedRepository repository = mock(ProvidedRepository.class);
			when(repository.getDataFactory(customer, "admin", "Contact")).thenReturn(null);
			when(repository.getModule(customer, "admin")).thenReturn(module);
			setProvidedRepository(repository);

			assertThrows(org.skyve.metadata.MetaDataException.class, () -> builder.build(module, document));
			assertThrows(org.skyve.metadata.MetaDataException.class, () -> builder.build(document));
			assertThrows(org.skyve.metadata.MetaDataException.class, () -> builder.build("admin", "Contact"));
		}
		finally {
			setProvidedRepository(original);
			clearPersistenceThreadLocal();
		}
	}

	@Test
	void buildUsesFactoryBeanWhenFactoryReturnsMatch() {
		ProvidedRepository original = getProvidedRepository();
		try {
			DataBuilder builder = newBuilderWithMockUser();
			Customer customer = (Customer) getField(builder, "customer");
			DynamicBean typeBean = mock(DynamicBean.class);
			DynamicBean nameBean = mock(DynamicBean.class);
			DynamicBean fallbackBean = mock(DynamicBean.class);
			TestFactory factory = new TestFactory(typeBean, nameBean, fallbackBean);

			ProvidedRepository repository = mock(ProvidedRepository.class);
			when(repository.getDataFactory(customer, "admin", "Contact")).thenReturn(factory);
			setProvidedRepository(repository);

			Module module = mock(Module.class);
			when(module.getName()).thenReturn("admin");
			DocumentImpl document = new DocumentImpl();
			document.setOwningModuleName("admin");
			document.setName("Contact");
			document.setDynamism(mock(Dynamic.class));

			Bean result = builder.build(module, document);
			assertThat(result, is(typeBean));
		}
		finally {
			setProvidedRepository(original);
			clearPersistenceThreadLocal();
		}
	}

	@Test
	void randomDomainValueCoversNullAndConstantDomainBranches() {
		ProvidedRepository original = getProvidedRepository();
		try {
			Customer customer = mock(Customer.class);
			Document document = mock(Document.class);
			Attribute attribute = mock(Attribute.class);
			Bean bean = mock(Bean.class);

			when(attribute.getDomainType()).thenReturn(null);
			assertThat(invokeRandomDomainValue(customer, document, attribute, bean), is((String) null));

			CustomerImpl internalCustomer = mock(CustomerImpl.class);
			DocumentImpl internalDocument = new DocumentImpl();
			internalDocument.setOwningModuleName("admin");
			internalDocument.setName("Contact");

			ProvidedRepository repository = mock(ProvidedRepository.class);
			when(repository.getBizlet(internalCustomer, internalDocument, true)).thenReturn(null);
			when(repository.getMetaDataBizlet(internalCustomer, internalDocument)).thenReturn(null);
			setProvidedRepository(repository);

			when(attribute.getDomainType()).thenReturn(DomainType.constant);
			stubConstantDomainValues(internalCustomer, attribute, java.util.Collections.emptyList());
			assertThat(invokeRandomDomainValue(internalCustomer, internalDocument, attribute, bean), is(""));

			stubConstantDomainValues(internalCustomer, attribute, java.util.List.of(new DomainValue("A", "A")));
			assertThat(invokeRandomDomainValue(internalCustomer, internalDocument, attribute, bean), is("A"));
		}
		finally {
			setProvidedRepository(original);
		}
	}

	@Test
	void randomBeanPopulatesCoreScalarTypes() {
		try {
			DataBuilder builder = newBuilderWithMockUser();
			Customer customer = (Customer) getField(builder, "customer");
			Module module = mock(Module.class);
			when(module.getName()).thenReturn("admin");

			RandomScalarBean bean = new RandomScalarBean();
			Document document = mock(Document.class);
			when(document.newInstance(org.mockito.ArgumentMatchers.any(User.class))).thenReturn(bean);

			Attribute boolAttribute = mock(Attribute.class);
			when(boolAttribute.getName()).thenReturn("boolValue");
			when(boolAttribute.getAttributeType()).thenReturn(AttributeType.bool);
			when(boolAttribute.getDomainType()).thenReturn(null);

			Attribute colourAttribute = mock(Attribute.class);
			when(colourAttribute.getName()).thenReturn("colourValue");
			when(colourAttribute.getAttributeType()).thenReturn(AttributeType.colour);
			when(colourAttribute.getDomainType()).thenReturn(null);

			Attribute idAttribute = mock(Attribute.class);
			when(idAttribute.getName()).thenReturn("idValue");
			when(idAttribute.getAttributeType()).thenReturn(AttributeType.id);
			when(idAttribute.getDomainType()).thenReturn(null);

			Attribute dateAttribute = mock(Attribute.class);
			when(dateAttribute.getName()).thenReturn("dateValue");
			when(dateAttribute.getAttributeType()).thenReturn(AttributeType.date);
			when(dateAttribute.getDomainType()).thenReturn(null);

			Attribute integerAttribute = mock(Attribute.class);
			when(integerAttribute.getName()).thenReturn("integerValue");
			when(integerAttribute.getAttributeType()).thenReturn(AttributeType.integer);
			when(integerAttribute.getDomainType()).thenReturn(null);

			Attribute longAttribute = mock(Attribute.class);
			when(longAttribute.getName()).thenReturn("longValue");
			when(longAttribute.getAttributeType()).thenReturn(AttributeType.longInteger);
			when(longAttribute.getDomainType()).thenReturn(null);

			doReturn(java.util.List.of(boolAttribute,
									colourAttribute,
									idAttribute,
									dateAttribute,
									integerAttribute,
									longAttribute)).when(document).getAllAttributes(customer);

			RandomScalarBean result = (RandomScalarBean) invokeRandomBean(builder, module, document);
			assertThat(result.getBoolValue(), is(Boolean.FALSE));
			assertThat(result.getColourValue(), is("#FFFFFF"));
			assertNotNull(result.getIdValue());
			assertNotNull(result.getDateValue());
			assertNotNull(result.getIntegerValue());
			assertNotNull(result.getLongValue());
		}
		catch (Exception e) {
			throw new AssertionError(e);
		}
		finally {
			clearPersistenceThreadLocal();
		}
	}
}
