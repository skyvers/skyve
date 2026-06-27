package org.skyve.impl.metadata.repository.customer;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;
import java.util.Collections;
import java.util.List;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.metadata.ConverterName;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.repository.ProvidedRepository;

@SuppressWarnings("static-method")
class CustomerMetaDataTest {

	@BeforeAll
	static void setUpRepository() {
		ProvidedRepository mockRepository = mock(ProvidedRepository.class);
		Module mockModule = mock(Module.class);
		when(mockRepository.getModule(any(), any())).thenReturn(mockModule);
		when(mockModule.getDocumentRefs()).thenReturn(Collections.emptyMap());
		ProvidedRepositoryFactory.set(mockRepository);
	}

	@AfterAll
	static void tearDownRepository() throws Exception {
		Field field = ProvidedRepositoryFactory.class.getDeclaredField("repository");
		field.setAccessible(true);
		field.set(null, null);
	}

	@Test
	void testSetAndGetLanguage() {
		CustomerMetaData customer = new CustomerMetaData();
		assertNull(customer.getLanguage());
		customer.setLanguage("en");
		assertEquals("en", customer.getLanguage());
	}

	@Test
	void testSetAndGetDefaultDateConverter() {
		CustomerMetaData customer = new CustomerMetaData();
		assertNull(customer.getDefaultDateConverter());
		customer.setDefaultDateConverter(ConverterName.DD_MMM_YYYY);
		assertEquals(ConverterName.DD_MMM_YYYY, customer.getDefaultDateConverter());
	}

	@Test
	void testSetAndGetDefaultDateTimeConverter() {
		CustomerMetaData customer = new CustomerMetaData();
		assertNull(customer.getDefaultDateTimeConverter());
		customer.setDefaultDateTimeConverter(ConverterName.DD_MMM_YYYY_HH_MI);
		assertEquals(ConverterName.DD_MMM_YYYY_HH_MI, customer.getDefaultDateTimeConverter());
	}

	@Test
	void testSetAndGetDefaultTimeConverter() {
		CustomerMetaData customer = new CustomerMetaData();
		assertNull(customer.getDefaultTimeConverter());
		customer.setDefaultTimeConverter(ConverterName.HH_MI);
		assertEquals(ConverterName.HH_MI, customer.getDefaultTimeConverter());
	}

	@Test
	void testSetAndGetDefaultTimestampConverter() {
		CustomerMetaData customer = new CustomerMetaData();
		assertNull(customer.getDefaultTimestampConverter());
		customer.setDefaultTimestampConverter(ConverterName.DD_MMM_YYYY_HH_MI_SS);
		assertEquals(ConverterName.DD_MMM_YYYY_HH_MI_SS, customer.getDefaultTimestampConverter());
	}

	@Test
	void testGetTextSearchRolesNotNull() {
		CustomerMetaData customer = new CustomerMetaData();
		assertNotNull(customer.getTextSearchRoles());
		assertTrue(customer.getTextSearchRoles().isEmpty());
	}

	@Test
	void testGetFlagRolesNotNull() {
		CustomerMetaData customer = new CustomerMetaData();
		assertNotNull(customer.getFlagRoles());
		assertTrue(customer.getFlagRoles().isEmpty());
	}

	@Test
	void testGetSwitchModeRolesNotNull() {
		CustomerMetaData customer = new CustomerMetaData();
		assertNotNull(customer.getSwitchModeRoles());
		assertTrue(customer.getSwitchModeRoles().isEmpty());
	}

	@Test
	void testGetInterceptorsNotNull() {
		CustomerMetaData customer = new CustomerMetaData();
		assertNotNull(customer.getInterceptors());
		assertTrue(customer.getInterceptors().isEmpty());
	}

	@Test
	void testGetObserversNotNull() {
		CustomerMetaData customer = new CustomerMetaData();
		assertNotNull(customer.getObservers());
		assertTrue(customer.getObservers().isEmpty());
	}

	@Test
	void testSetAndGetJFreeChartPostProcessorClassName() {
		CustomerMetaData customer = new CustomerMetaData();
		assertNull(customer.getJFreeChartPostProcessorClassName());
		customer.setJFreeChartPostProcessorClassName("com.example.MyProcessor");
		assertEquals("com.example.MyProcessor", customer.getJFreeChartPostProcessorClassName());
	}

	@Test
	void testSetAndGetPrimeFacesChartPostProcessorClassName() {
		CustomerMetaData customer = new CustomerMetaData();
		assertNull(customer.getPrimeFacesChartPostProcessorClassName());
		customer.setPrimeFacesChartPostProcessorClassName("com.example.PFProcessor");
		assertEquals("com.example.PFProcessor", customer.getPrimeFacesChartPostProcessorClassName());
	}

	@Test
	void testSetAndGetModules() {
		CustomerMetaData customer = new CustomerMetaData();
		assertNull(customer.getModules());
		CustomerModulesMetaData modules = new CustomerModulesMetaData();
		customer.setModules(modules);
		assertEquals(modules, customer.getModules());
	}

	@Test
	void testSetAndGetRoles() {
		CustomerMetaData customer = new CustomerMetaData();
		assertNull(customer.getRoles());
		CustomerRolesMetaData roles = new CustomerRolesMetaData();
		customer.setRoles(roles);
		assertEquals(roles, customer.getRoles());
	}

	@Test
	void testSetAndGetUiResources() {
		CustomerMetaData customer = new CustomerMetaData();
		assertNull(customer.getUiResources());
		UIResources uiResources = new UIResources();
		customer.setUiResources(uiResources);
		assertEquals(uiResources, customer.getUiResources());
	}

	@Test
	void testSetAndGetHtmlResources() {
		CustomerMetaData customer = new CustomerMetaData();
		assertNull(customer.getHtmlResources());
		HTMLResourcesMetaData htmlResources = new HTMLResourcesMetaData();
		customer.setHtmlResources(htmlResources);
		assertEquals(htmlResources, customer.getHtmlResources());
	}

	@Test
	void testSetAndGetLoginResources() {
		CustomerMetaData customer = new CustomerMetaData();
		assertNull(customer.getLoginResources());
		LoginResourcesMetaData loginResources = new LoginResourcesMetaData();
		customer.setLoginResources(loginResources);
		assertEquals(loginResources, customer.getLoginResources());
	}

	@Test
	void testSetAndGetLastModifiedMillis() {
		CustomerMetaData customer = new CustomerMetaData();
		customer.setLastModifiedMillis(12345L);
		assertEquals(12345L, customer.getLastModifiedMillis());
	}

	@Test
	void testGetPropertiesNotNull() {
		CustomerMetaData customer = new CustomerMetaData();
		assertNotNull(customer.getProperties());
	}

	// ── convert() ───────────────────────────────────────────────────────────

	private static CustomerMetaData createMinimalCustomerMetaData() {
		CustomerMetaData customer = new CustomerMetaData();
		customer.setName("test");
		customer.setDefaultDateConverter(ConverterName.DD_MMM_YYYY);
		customer.setDefaultDateTimeConverter(ConverterName.DD_MMM_YYYY_HH_MI);
		customer.setDefaultTimeConverter(ConverterName.HH_MI);
		customer.setDefaultTimestampConverter(ConverterName.DD_MMM_YYYY_HH_MI_SS);

		CustomerModulesMetaData modules = new CustomerModulesMetaData();
		modules.setHomeModule("admin");
		CustomerModuleMetaData module = new CustomerModuleMetaData();
		module.setName("admin");
		modules.getModules().add(module);
		customer.setModules(modules);

		return customer;
	}

	@Test
	void convertThrowsWhenNameIsNull() {
		CustomerMetaData customer = createMinimalCustomerMetaData();
		customer.setName(null);
		assertThrows(MetaDataException.class, () -> customer.convert("test"));
	}

	@Test
	void convertThrowsWhenDefaultDateConverterIsNull() {
		CustomerMetaData customer = createMinimalCustomerMetaData();
		customer.setDefaultDateConverter(null);
		assertThrows(MetaDataException.class, () -> customer.convert("test"));
	}

	@Test
	void convertThrowsWhenDefaultDateTimeConverterIsNull() {
		CustomerMetaData customer = createMinimalCustomerMetaData();
		customer.setDefaultDateTimeConverter(null);
		assertThrows(MetaDataException.class, () -> customer.convert("test"));
	}

	@Test
	void convertThrowsWhenDefaultTimeConverterIsNull() {
		CustomerMetaData customer = createMinimalCustomerMetaData();
		customer.setDefaultTimeConverter(null);
		assertThrows(MetaDataException.class, () -> customer.convert("test"));
	}

	@Test
	void convertThrowsWhenDefaultTimestampConverterIsNull() {
		CustomerMetaData customer = createMinimalCustomerMetaData();
		customer.setDefaultTimestampConverter(null);
		assertThrows(MetaDataException.class, () -> customer.convert("test"));
	}

	@Test
	void convertThrowsWhenHomeModuleIsNull() {
		CustomerMetaData customer = createMinimalCustomerMetaData();
		customer.getModules().setHomeModule(null);
		assertThrows(MetaDataException.class, () -> customer.convert("test"));
	}

	@Test
	void convertThrowsWhenDateConverterIsWrongType() {
		CustomerMetaData customer = createMinimalCustomerMetaData();
		// HH_MI is a time converter, not a date converter
		customer.setDefaultDateConverter(ConverterName.HH_MI);
		assertThrows(MetaDataException.class, () -> customer.convert("test"));
	}

	@Test
	void convertThrowsWhenTimeConverterIsWrongType() {
		CustomerMetaData customer = createMinimalCustomerMetaData();
		// DD_MMM_YYYY is a date converter, not a time converter
		customer.setDefaultTimeConverter(ConverterName.DD_MMM_YYYY);
		assertThrows(MetaDataException.class, () -> customer.convert("test"));
	}

	@Test
	void convertThrowsWhenDateTimeConverterIsWrongType() {
		CustomerMetaData customer = createMinimalCustomerMetaData();
		// DD_MMM_YYYY is a date converter, not a dateTime converter
		customer.setDefaultDateTimeConverter(ConverterName.DD_MMM_YYYY);
		assertThrows(MetaDataException.class, () -> customer.convert("test"));
	}

	@Test
	void convertThrowsWhenTimestampConverterIsWrongType() {
		CustomerMetaData customer = createMinimalCustomerMetaData();
		// DD_MMM_YYYY is a date converter, not a timestamp converter
		customer.setDefaultTimestampConverter(ConverterName.DD_MMM_YYYY);
		assertThrows(MetaDataException.class, () -> customer.convert("test"));
	}

	@Test
	void convertThrowsWhenRoleNameIsNull() {
		CustomerMetaData customer = createMinimalCustomerMetaData();
		CustomerRolesMetaData roles = new CustomerRolesMetaData();
		CustomerRoleMetaData role = new CustomerRoleMetaData();
		// name is null by default
		roles.getRoles().add(role);
		customer.setRoles(roles);
		assertThrows(MetaDataException.class, () -> customer.convert("test"));
	}

	@Test
	void convertThrowsOnDuplicateRoleName() {
		CustomerMetaData customer = createMinimalCustomerMetaData();
		CustomerRolesMetaData roles = new CustomerRolesMetaData();
		CustomerRoleMetaData role1 = new CustomerRoleMetaData();
		role1.setName("admin");
		CustomerRoleMetaData role2 = new CustomerRoleMetaData();
		role2.setName("admin"); // duplicate
		roles.getRoles().add(role1);
		roles.getRoles().add(role2);
		customer.setRoles(roles);
		assertThrows(MetaDataException.class, () -> customer.convert("test"));
	}

	@Test
	void convertThrowsWhenInterceptorClassNameIsNull() {
		CustomerMetaData customer = createMinimalCustomerMetaData();
		InterceptorMetaDataImpl interceptor = new InterceptorMetaDataImpl();
		// className is null by default
		customer.getInterceptors().add(interceptor);
		assertThrows(MetaDataException.class, () -> customer.convert("test"));
	}

	@Test
	void convertThrowsWhenObserverClassNameIsNull() {
		CustomerMetaData customer = createMinimalCustomerMetaData();
		ObserverMetaDataImpl observer = new ObserverMetaDataImpl();
		// className is null by default
		customer.getObservers().add(observer);
		assertThrows(MetaDataException.class, () -> customer.convert("test"));
	}

	@Test
	void convertThrowsOnDuplicateInterceptor() {
		CustomerMetaData customer = createMinimalCustomerMetaData();
		InterceptorMetaDataImpl interceptor1 = new InterceptorMetaDataImpl();
		interceptor1.setClassName("org.example.MyInterceptor");
		InterceptorMetaDataImpl interceptor2 = new InterceptorMetaDataImpl();
		interceptor2.setClassName("org.example.MyInterceptor"); // duplicate
		customer.getInterceptors().add(interceptor1);
		customer.getInterceptors().add(interceptor2);
		assertThrows(MetaDataException.class, () -> customer.convert("test"));
	}

	@Test
	void convertThrowsOnDuplicateObserver() {
		CustomerMetaData customer = createMinimalCustomerMetaData();
		ObserverMetaDataImpl observer1 = new ObserverMetaDataImpl();
		observer1.setClassName("org.example.MyObserver");
		ObserverMetaDataImpl observer2 = new ObserverMetaDataImpl();
		observer2.setClassName("org.example.MyObserver"); // duplicate
		customer.getObservers().add(observer1);
		customer.getObservers().add(observer2);
		assertThrows(MetaDataException.class, () -> customer.convert("test"));
	}

	@Test
	void convertSucceedsWithMinimalCustomer() {
		CustomerMetaData customer = createMinimalCustomerMetaData();
		assertNotNull(customer.convert("test"));
	}

	@Test
	void convertSucceedsWithRolesAndModuleRoles() {
		CustomerMetaData customer = createMinimalCustomerMetaData();
		CustomerRolesMetaData rolesMetaData = new CustomerRolesMetaData();
		CustomerRoleMetaData role = new CustomerRoleMetaData();
		role.setName("AdminRole");
		CustomerModuleRoleMetaData moduleRole = new CustomerModuleRoleMetaData();
		moduleRole.setName("BasicUser");
		moduleRole.setModuleName("admin");
		role.getRoles().add(moduleRole);
		rolesMetaData.getRoles().add(role);
		customer.setRoles(rolesMetaData);
		assertNotNull(customer.convert("test"));
	}

	@Test
	void convertThrowsWhenModuleRoleNameIsNull() {
		CustomerMetaData customer = createMinimalCustomerMetaData();
		CustomerRolesMetaData rolesMetaData = new CustomerRolesMetaData();
		CustomerRoleMetaData role = new CustomerRoleMetaData();
		role.setName("AdminRole");
		CustomerModuleRoleMetaData moduleRole = new CustomerModuleRoleMetaData();
		moduleRole.setName(null); // null name - should throw
		moduleRole.setModuleName("admin");
		role.getRoles().add(moduleRole);
		rolesMetaData.getRoles().add(role);
		customer.setRoles(rolesMetaData);
		assertThrows(MetaDataException.class, () -> customer.convert("test"));
	}

	@Test
	void convertThrowsWhenModuleRoleModuleNotValid() {
		CustomerMetaData customer = createMinimalCustomerMetaData();
		CustomerRolesMetaData rolesMetaData = new CustomerRolesMetaData();
		CustomerRoleMetaData role = new CustomerRoleMetaData();
		role.setName("AdminRole");
		CustomerModuleRoleMetaData moduleRole = new CustomerModuleRoleMetaData();
		moduleRole.setName("SomeRole");
		moduleRole.setModuleName("nonExistentModule"); // not a valid module
		role.getRoles().add(moduleRole);
		rolesMetaData.getRoles().add(role);
		customer.setRoles(rolesMetaData);
		assertThrows(MetaDataException.class, () -> customer.convert("test"));
	}

	@Test
	void convertSucceedsWithTextSearchFeatureRole() {
		CustomerMetaData customer = createMinimalCustomerMetaData();

		CustomerRolesMetaData rolesMetaData = new CustomerRolesMetaData();
		CustomerRoleMetaData role = new CustomerRoleMetaData();
		role.setName("SearchRole");
		CustomerModuleRoleMetaData moduleRole = new CustomerModuleRoleMetaData();
		moduleRole.setName("BasicUser");
		moduleRole.setModuleName("admin");
		role.getRoles().add(moduleRole);
		rolesMetaData.getRoles().add(role);
		customer.setRoles(rolesMetaData);

		CustomerFeatureRoleMetaData featureRole = new CustomerFeatureRoleMetaData();
		featureRole.setName("SearchRole"); // reference to existing customer role
		List<CustomerFeatureRoleMetaData> textSearchRoles = customer.getTextSearchRoles();
		textSearchRoles.add(featureRole);

		assertNotNull(customer.convert("test"));
	}

	@Test
	void convertSucceedsWithFlagFeatureRoleByModuleName() {
		CustomerMetaData customer = createMinimalCustomerMetaData();
		CustomerFeatureRoleMetaData featureRole = new CustomerFeatureRoleMetaData();
		featureRole.setName("BasicUser");
		featureRole.setModuleName("admin");
		customer.getFlagRoles().add(featureRole);
		assertNotNull(customer.convert("test"));
	}

	@Test
	void convertSucceedsWithSwitchModeFeatureRoleByModuleName() {
		CustomerMetaData customer = createMinimalCustomerMetaData();
		CustomerFeatureRoleMetaData featureRole = new CustomerFeatureRoleMetaData();
		featureRole.setName("BasicUser");
		featureRole.setModuleName("admin");
		customer.getSwitchModeRoles().add(featureRole);
		assertNotNull(customer.convert("test"));
	}

	@Test
	void convertThrowsWhenFeatureRoleNameIsNull() {
		CustomerMetaData customer = createMinimalCustomerMetaData();
		CustomerFeatureRoleMetaData featureRole = new CustomerFeatureRoleMetaData();
		featureRole.setName(null); // null name - should throw
		featureRole.setModuleName("admin");
		customer.getFlagRoles().add(featureRole);
		assertThrows(MetaDataException.class, () -> customer.convert("test"));
	}

	@Test
	void convertThrowsWhenFeatureRoleModuleIsInvalid() {
		CustomerMetaData customer = createMinimalCustomerMetaData();
		CustomerFeatureRoleMetaData featureRole = new CustomerFeatureRoleMetaData();
		featureRole.setName("SomeRole");
		featureRole.setModuleName("nonExistentModule"); // invalid module
		customer.getFlagRoles().add(featureRole);
		assertThrows(MetaDataException.class, () -> customer.convert("test"));
	}

	@Test
	void convertThrowsWhenFeatureRoleNameRefersToUnknownCustomerRole() {
		CustomerMetaData customer = createMinimalCustomerMetaData();
		// no roles defined, so this customer role name won't be found
		CustomerFeatureRoleMetaData featureRole = new CustomerFeatureRoleMetaData();
		featureRole.setName("NonExistentCustomerRole"); // no matching customer role
		// no module name set - will try to find by customer role name
		customer.getTextSearchRoles().add(featureRole);
		assertThrows(MetaDataException.class, () -> customer.convert("test"));
	}

	@Test
	void convertThrowsWhenDuplicateFeatureRole() {
		CustomerMetaData customer = createMinimalCustomerMetaData();
		CustomerFeatureRoleMetaData featureRole1 = new CustomerFeatureRoleMetaData();
		featureRole1.setName("BasicUser");
		featureRole1.setModuleName("admin");
		CustomerFeatureRoleMetaData featureRole2 = new CustomerFeatureRoleMetaData();
		featureRole2.setName("BasicUser");
		featureRole2.setModuleName("admin"); // duplicate
		customer.getFlagRoles().add(featureRole1);
		customer.getFlagRoles().add(featureRole2);
		assertThrows(MetaDataException.class, () -> customer.convert("test"));
	}
}
