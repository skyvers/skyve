package org.skyve.impl.metadata.customer;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import org.skyve.domain.types.converters.date.DD_MMM_YYYY;
import org.skyve.domain.types.converters.datetime.DD_MM_YYYY_HH24_MI;
import org.skyve.domain.types.converters.time.HH24_MI;
import org.skyve.domain.types.converters.timestamp.DD_MM_YYYY_HH24_MI_SS;
import org.skyve.impl.metadata.repository.customer.CustomerRoleMetaData;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.controller.Interceptor;
import org.skyve.metadata.controller.Observer;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.metadata.customer.CustomerRole;
import org.skyve.metadata.customer.InterceptorMetaData;
import org.skyve.metadata.customer.ObserverMetaData;
import org.skyve.metadata.customer.UIResources;
import org.skyve.metadata.customer.HTMLResources;
import org.skyve.metadata.customer.LoginResources;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.web.WebContext;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@SuppressWarnings("static-method")
class CustomerImplTest {

	@Test
	void testSetAndGetName() {
		CustomerImpl customer = new CustomerImpl();
		assertNull(customer.getName());
		customer.setName("acme");
		assertEquals("acme", customer.getName());
	}

	@Test
	void testSetAndGetLanguageTag() {
		CustomerImpl customer = new CustomerImpl();
		assertNull(customer.getLanguageTag());
		customer.setLanguageTag("en-AU");
		assertEquals("en-AU", customer.getLanguageTag());
	}

	@Test
	void testSetAndGetAllowModuleRoles() {
		CustomerImpl customer = new CustomerImpl();
		// default
		assertTrue(customer.isAllowModuleRoles());
		customer.setAllowModuleRoles(false);
		assertFalse(customer.isAllowModuleRoles());
	}

	@Test
	void testSetAndGetDefaultDateConverter() {
		CustomerImpl customer = new CustomerImpl();
		assertNull(customer.getDefaultDateConverter());
		DD_MMM_YYYY converter = new DD_MMM_YYYY();
		customer.setDefaultDateConverter(converter);
		assertEquals(converter, customer.getDefaultDateConverter());
	}

	@Test
	void testGetModulesReturnsEmptyListByDefault() {
		CustomerImpl customer = new CustomerImpl();
		List<Module> modules = customer.getModules();
		assertNotNull(modules);
		assertTrue(modules.isEmpty());
	}

	@Test
	void testGetRolesReturnsEmptyCollectionByDefault() {
		CustomerImpl customer = new CustomerImpl();
		Collection<CustomerRole> roles = customer.getRoles();
		assertNotNull(roles);
		assertTrue(roles.isEmpty());
	}

	@Test
	void testGetRoleReturnsNullWhenNotFound() {
		CustomerImpl customer = new CustomerImpl();
		assertNull(customer.getRole("nonexistent"));
	}

	@Test
	void testGetInterceptorsReturnsEmptyCollectionByDefault() {
		CustomerImpl customer = new CustomerImpl();
		assertNotNull(customer.getInterceptors());
		assertTrue(customer.getInterceptors().isEmpty());
	}

	@Test
	void testGetTextSearchRolesReturnsEmptySetByDefault() {
		CustomerImpl customer = new CustomerImpl();
		assertNotNull(customer.getTextSearchRoles());
		assertTrue(customer.getTextSearchRoles().isEmpty());
	}

	@Test
	void testGetFlagRolesReturnsEmptySetByDefault() {
		CustomerImpl customer = new CustomerImpl();
		assertNotNull(customer.getFlagRoles());
		assertTrue(customer.getFlagRoles().isEmpty());
	}

	@Test
	void testGetSwitchModeRolesReturnsEmptySetByDefault() {
		CustomerImpl customer = new CustomerImpl();
		assertNotNull(customer.getSwitchModeRoles());
		assertTrue(customer.getSwitchModeRoles().isEmpty());
	}

	@Test
	void testToStringIncludesName() {
		CustomerImpl customer = new CustomerImpl();
		customer.setName("acme");
		assertTrue(customer.toString().contains("acme"));
	}

	@Test
	void testGetDefaultActionsNotNull() {
		CustomerImpl customer = new CustomerImpl();
		assertNotNull(customer.getDefaultActions());
	}

	@Test
	void testGetModuleEntriesNotNull() {
		CustomerImpl customer = new CustomerImpl();
		assertNotNull(customer.getModuleEntries());
	}

	@Test
	void testInterceptBeforeNewInstanceWithNoInterceptors() throws Exception {
		CustomerImpl customer = new CustomerImpl();
		assertFalse(customer.interceptBeforeNewInstance(null));
	}

	@Test
	void testInterceptAfterNewInstanceWithNoInterceptors() throws Exception {
		CustomerImpl customer = new CustomerImpl();
		assertDoesNotThrow(() -> customer.interceptAfterNewInstance(null));
	}

	@Test
	void testInterceptBeforeValidateWithNoInterceptors() throws Exception {
		CustomerImpl customer = new CustomerImpl();
		assertFalse(customer.interceptBeforeValidate(null, null));
	}

	@Test
	void testInterceptAfterValidateWithNoInterceptors() throws Exception {
		CustomerImpl customer = new CustomerImpl();
		assertDoesNotThrow(() -> customer.interceptAfterValidate(null, null));
	}

	@Test
	void testInterceptBeforeGetConstantDomainValuesWithNoInterceptors() throws Exception {
		CustomerImpl customer = new CustomerImpl();
		assertFalse(customer.interceptBeforeGetConstantDomainValues("attr"));
	}

	@Test
	void testInterceptAfterGetConstantDomainValuesWithNoInterceptors() throws Exception {
		CustomerImpl customer = new CustomerImpl();
		assertDoesNotThrow(() -> customer.interceptAfterGetConstantDomainValues("attr", new ArrayList<>()));
	}

	@Test
	void testInterceptBeforeGetVariantDomainValuesWithNoInterceptors() throws Exception {
		CustomerImpl customer = new CustomerImpl();
		assertFalse(customer.interceptBeforeGetVariantDomainValues("attr"));
	}

	@Test
	void testInterceptAfterGetVariantDomainValuesWithNoInterceptors() throws Exception {
		CustomerImpl customer = new CustomerImpl();
		assertDoesNotThrow(() -> customer.interceptAfterGetVariantDomainValues("attr", new ArrayList<>()));
	}

	@Test
	void testInterceptBeforeGetDynamicDomainValuesWithNoInterceptors() throws Exception {
		CustomerImpl customer = new CustomerImpl();
		assertFalse(customer.interceptBeforeGetDynamicDomainValues("attr", null));
	}

	@Test
	void testInterceptAfterGetDynamicDomainValuesWithNoInterceptors() throws Exception {
		CustomerImpl customer = new CustomerImpl();
		assertDoesNotThrow(() -> customer.interceptAfterGetDynamicDomainValues("attr", null, new ArrayList<>()));
	}

	@Test
	void testInterceptBeforeCompleteWithNoInterceptors() throws Exception {
		CustomerImpl customer = new CustomerImpl();
		assertFalse(customer.interceptBeforeComplete("attr", "val", null));
	}

	@Test
	void testInterceptAfterCompleteWithNoInterceptors() throws Exception {
		CustomerImpl customer = new CustomerImpl();
		assertDoesNotThrow(() -> customer.interceptAfterComplete("attr", "val", null, new ArrayList<>()));
	}

	@Test
	void testInterceptBeforeSaveWithNoInterceptors() throws Exception {
		CustomerImpl customer = new CustomerImpl();
		assertFalse(customer.interceptBeforeSave(null, null));
	}

	@Test
	void testInterceptAfterSaveWithNoInterceptors() throws Exception {
		CustomerImpl customer = new CustomerImpl();
		assertDoesNotThrow(() -> customer.interceptAfterSave(null, null));
	}

	@Test
	void testInterceptBeforePreSaveWithNoInterceptors() throws Exception {
		CustomerImpl customer = new CustomerImpl();
		assertFalse(customer.interceptBeforePreSave(null));
	}

	@Test
	void testInterceptAfterPreSaveWithNoInterceptors() throws Exception {
		CustomerImpl customer = new CustomerImpl();
		assertDoesNotThrow(() -> customer.interceptAfterPreSave(null));
	}

	@Test
	void testInterceptBeforePostSaveWithNoInterceptors() throws Exception {
		CustomerImpl customer = new CustomerImpl();
		assertFalse(customer.interceptBeforePostSave(null));
	}

	@Test
	void testInterceptAfterPostSaveWithNoInterceptors() throws Exception {
		CustomerImpl customer = new CustomerImpl();
		assertDoesNotThrow(() -> customer.interceptAfterPostSave(null));
	}

	@Test
	void testInterceptBeforeDeleteWithNoInterceptors() throws Exception {
		CustomerImpl customer = new CustomerImpl();
		assertFalse(customer.interceptBeforeDelete(null, null));
	}

	@Test
	void testInterceptAfterDeleteWithNoInterceptors() throws Exception {
		CustomerImpl customer = new CustomerImpl();
		assertDoesNotThrow(() -> customer.interceptAfterDelete(null, null));
	}

	@Test
	void testInterceptBeforePreDeleteWithNoInterceptors() throws Exception {
		CustomerImpl customer = new CustomerImpl();
		assertFalse(customer.interceptBeforePreDelete(null));
	}

	@Test
	void testInterceptAfterPreDeleteWithNoInterceptors() throws Exception {
		CustomerImpl customer = new CustomerImpl();
		assertDoesNotThrow(() -> customer.interceptAfterPreDelete(null));
	}

	@Test
	void testInterceptBeforePostDeleteWithNoInterceptors() throws Exception {
		CustomerImpl customer = new CustomerImpl();
		assertFalse(customer.interceptBeforePostDelete(null));
	}

	@Test
	void testInterceptAfterPostDeleteWithNoInterceptors() throws Exception {
		CustomerImpl customer = new CustomerImpl();
		assertDoesNotThrow(() -> customer.interceptAfterPostDelete(null));
	}

	@Test
	void testInterceptBeforePostLoadWithNoInterceptors() throws Exception {
		CustomerImpl customer = new CustomerImpl();
		assertFalse(customer.interceptBeforePostLoad(null));
	}

	@Test
	void testInterceptAfterPostLoadWithNoInterceptors() throws Exception {
		CustomerImpl customer = new CustomerImpl();
		assertDoesNotThrow(() -> customer.interceptAfterPostLoad(null));
	}

	@Test
	void testInterceptBeforePreExecuteWithNoInterceptors() throws Exception {
		CustomerImpl customer = new CustomerImpl();
		assertFalse(customer.interceptBeforePreExecute(null, null, null, (WebContext) null));
	}

	@Test
	void testInterceptAfterPreExecuteWithNoInterceptors() throws Exception {
		CustomerImpl customer = new CustomerImpl();
		assertDoesNotThrow(() -> customer.interceptAfterPreExecute(null, null, null, (WebContext) null));
	}

	@Test
	void testInterceptBeforePreRerenderWithNoInterceptors() throws Exception {
		CustomerImpl customer = new CustomerImpl();
		assertFalse(customer.interceptBeforePreRerender(null, null, (WebContext) null));
	}

	@Test
	void testInterceptAfterPreRerenderWithNoInterceptors() throws Exception {
		CustomerImpl customer = new CustomerImpl();
		assertDoesNotThrow(() -> customer.interceptAfterPreRerender(null, null, (WebContext) null));
	}

	@Test
	void testInterceptBeforeServerSideActionWithNoInterceptors() throws Exception {
		CustomerImpl customer = new CustomerImpl();
		assertFalse(customer.interceptBeforeServerSideAction(null, null, null, (WebContext) null));
	}

	@Test
	void testInterceptAfterServerSideActionWithNoInterceptors() throws Exception {
		CustomerImpl customer = new CustomerImpl();
		assertDoesNotThrow(() -> customer.interceptAfterServerSideAction(null, null, null, (WebContext) null));
	}

	@Test
	void testInterceptBeforeDownloadActionWithNoInterceptors() throws Exception {
		CustomerImpl customer = new CustomerImpl();
		assertFalse(customer.interceptBeforeDownloadAction(null, null, null, (WebContext) null));
	}

	@Test
	void testInterceptAfterDownloadActionWithNoInterceptors() throws Exception {
		CustomerImpl customer = new CustomerImpl();
		assertDoesNotThrow(() -> customer.interceptAfterDownloadAction(null, null, null, null, (WebContext) null));
	}

	@Test
	void testInterceptBeforeUploadActionWithNoInterceptors() throws Exception {
		CustomerImpl customer = new CustomerImpl();
		assertFalse(customer.interceptBeforeUploadAction(null, null, null, null, (WebContext) null));
	}

	@Test
	void testInterceptAfterUploadActionWithNoInterceptors() throws Exception {
		CustomerImpl customer = new CustomerImpl();
		assertDoesNotThrow(() -> customer.interceptAfterUploadAction(null, null, null, null, (WebContext) null));
	}

	@Test
	void testInterceptBeforeBizImportActionWithNoInterceptors() throws Exception {
		CustomerImpl customer = new CustomerImpl();
		assertFalse(customer.interceptBeforeBizImportAction(null, null, null, null));
	}

	@Test
	void testInterceptAfterBizImportActionWithNoInterceptors() throws Exception {
		CustomerImpl customer = new CustomerImpl();
		assertDoesNotThrow(() -> customer.interceptAfterBizImportAction(null, null, null, null));
	}

	@Test
	void testInterceptBeforeBizExportActionWithNoInterceptors() throws Exception {
		CustomerImpl customer = new CustomerImpl();
		assertFalse(customer.interceptBeforeBizExportAction(null, null, (WebContext) null));
	}

	@Test
	void testInterceptAfterBizExportActionWithNoInterceptors() throws Exception {
		CustomerImpl customer = new CustomerImpl();
		assertDoesNotThrow(() -> customer.interceptAfterBizExportAction(null, null, null, (WebContext) null));
	}

	@Test
	void testInterceptBeforePostRenderWithNoInterceptors() {
		CustomerImpl customer = new CustomerImpl();
		assertFalse(customer.interceptBeforePostRender(null, (WebContext) null));
	}

	@Test
	void testInterceptAfterPostRenderWithNoInterceptors() {
		CustomerImpl customer = new CustomerImpl();
		assertDoesNotThrow(() -> customer.interceptAfterPostRender(null, (WebContext) null));
	}

        // --- observer support ---

        @Test
        void testPutObserverReturnsTrueFirstTime() {
                ObserverMetaData observer = Mockito.mock(ObserverMetaData.class);
                Mockito.when(observer.getClassName()).thenReturn("com.example.MyObserver");
                CustomerImpl customer = new CustomerImpl();
                assertTrue(customer.putObserver(observer));
        }

        @Test
        void testPutObserverReturnsFalseForDuplicate() {
                ObserverMetaData observer = Mockito.mock(ObserverMetaData.class);
                Mockito.when(observer.getClassName()).thenReturn("com.example.MyObserver");
                CustomerImpl customer = new CustomerImpl();
                customer.putObserver(observer);
                assertFalse(customer.putObserver(observer));
        }

        @Test
        void testGetObserversContainsPutObserver() {
                ObserverMetaData observer = Mockito.mock(ObserverMetaData.class);
                Mockito.when(observer.getClassName()).thenReturn("com.example.X");
                CustomerImpl customer = new CustomerImpl();
                customer.putObserver(observer);
                assertTrue(customer.getObservers().contains(observer));
        }

        @Test
        void testNotifyStartupWithNoObserversDoesNotThrow() {
                CustomerImpl customer = new CustomerImpl();
                assertDoesNotThrow(customer::notifyStartup);
        }

        @Test
        void testNotifyShutdownWithNoObserversDoesNotThrow() {
                CustomerImpl customer = new CustomerImpl();
                assertDoesNotThrow(customer::notifyShutdown);
        }

        @Test
        void testNotifyBeforeBackupWithNoObserversDoesNotThrow() {
                CustomerImpl customer = new CustomerImpl();
                assertDoesNotThrow(customer::notifyBeforeBackup);
        }

        @Test
        void testNotifyAfterBackupWithNoObserversDoesNotThrow() {
                CustomerImpl customer = new CustomerImpl();
                assertDoesNotThrow(customer::notifyAfterBackup);
        }

        @Test
        void testNotifyBeforeRestoreWithNoObserversDoesNotThrow() {
                CustomerImpl customer = new CustomerImpl();
                assertDoesNotThrow(customer::notifyBeforeRestore);
        }

        @Test
        void testNotifyAfterRestoreWithNoObserversDoesNotThrow() {
                CustomerImpl customer = new CustomerImpl();
                assertDoesNotThrow(customer::notifyAfterRestore);
        }

        // --- role support ---

        @Test
        void testPutRoleReturnsTrueFirstTime() {
                CustomerRoleMetaData role = new CustomerRoleMetaData();
                role.setName("Viewer");
                CustomerImpl customer = new CustomerImpl();
                assertTrue(customer.putRole(role));
        }

        @Test
        void testPutRoleReturnsFalseForDuplicate() {
                CustomerRoleMetaData role = new CustomerRoleMetaData();
                role.setName("Viewer");
                CustomerImpl customer = new CustomerImpl();
                customer.putRole(role);
                assertFalse(customer.putRole(role));
        }

        @Test
        void testGetRoleAfterPut() {
                CustomerRoleMetaData role = new CustomerRoleMetaData();
                role.setName("Editor");
                CustomerImpl customer = new CustomerImpl();
                customer.putRole(role);
                assertEquals(role, customer.getRole("Editor"));
        }

        @Test
        void testGetRoleReturnsNullForMissing() {
                CustomerImpl customer = new CustomerImpl();
                assertNull(customer.getRole("NonExistent"));
        }

        // --- resource getters/setters ---

        @Test
        void testSetAndGetUiResources() {
                UIResources res = Mockito.mock(UIResources.class);
                CustomerImpl customer = new CustomerImpl();
                assertNull(customer.getUiResources());
                customer.setUiResources(res);
                assertEquals(res, customer.getUiResources());
        }

        @Test
        void testSetAndGetHtmlResources() {
                HTMLResources res = Mockito.mock(HTMLResources.class);
                CustomerImpl customer = new CustomerImpl();
                assertNull(customer.getHtmlResources());
                customer.setHtmlResources(res);
                assertEquals(res, customer.getHtmlResources());
        }

        @Test
        void testSetAndGetLoginResources() {
                LoginResources res = Mockito.mock(LoginResources.class);
                CustomerImpl customer = new CustomerImpl();
                assertNull(customer.getLoginResources());
                customer.setLoginResources(res);
                assertEquals(res, customer.getLoginResources());
        }

        @Test
        void testSetAndGetJFreeChartPostProcessorClassName() {
                CustomerImpl customer = new CustomerImpl();
                assertNull(customer.getJFreeChartPostProcessorClassName());
                customer.setJFreeChartPostProcessorClassName("com.example.Charts");
                assertEquals("com.example.Charts", customer.getJFreeChartPostProcessorClassName());
        }

        @Test
        void testSetAndGetPrimeFacesChartPostProcessorClassName() {
                CustomerImpl customer = new CustomerImpl();
                assertNull(customer.getPrimeFacesChartPostProcessorClassName());
                customer.setPrimeFacesChartPostProcessorClassName("com.example.PF");
                assertEquals("com.example.PF", customer.getPrimeFacesChartPostProcessorClassName());
        }

        @Test
        void testSetHomeModuleName() {
                CustomerImpl customer = new CustomerImpl();
                assertDoesNotThrow(() -> customer.setHomeModuleName("admin"));
        }

        // --- interceptor support ---

        @Test
        void testPutInterceptorReturnsTrueFirstTime() {
                CustomerImpl customer = new CustomerImpl();
                InterceptorMetaData interceptor = Mockito.mock(InterceptorMetaData.class);
                Mockito.when(interceptor.getClassName()).thenReturn("com.example.MyInterceptor");
                assertTrue(customer.putInterceptor(interceptor));
        }

        @Test
        void testPutInterceptorReturnsFalseForDuplicate() {
                CustomerImpl customer = new CustomerImpl();
                InterceptorMetaData interceptor = Mockito.mock(InterceptorMetaData.class);
                Mockito.when(interceptor.getClassName()).thenReturn("com.example.MyInterceptor");
                customer.putInterceptor(interceptor);
                assertFalse(customer.putInterceptor(interceptor));
        }

        @Test
        void testGetInterceptorsContainsPutInterceptor() {
                CustomerImpl customer = new CustomerImpl();
                InterceptorMetaData interceptor = Mockito.mock(InterceptorMetaData.class);
                Mockito.when(interceptor.getClassName()).thenReturn("com.example.X");
                customer.putInterceptor(interceptor);
                Collection<InterceptorMetaData> result = customer.getInterceptors();
                assertNotNull(result);
                assertEquals(1, result.size());
                assertTrue(result.contains(interceptor));
        }

        @Test
        void testNotifyLoginWithNoObserversDoesNotThrow() {
                CustomerImpl customer = new CustomerImpl();
                org.skyve.metadata.user.User user = Mockito.mock(org.skyve.metadata.user.User.class);
                jakarta.servlet.http.HttpSession session = Mockito.mock(jakarta.servlet.http.HttpSession.class);
                assertDoesNotThrow(() -> customer.notifyLogin(user, session));
        }

        @Test
        void testNotifyLogoutWithNoObserversDoesNotThrow() {
                CustomerImpl customer = new CustomerImpl();
                org.skyve.metadata.user.User user = Mockito.mock(org.skyve.metadata.user.User.class);
                jakarta.servlet.http.HttpSession session = Mockito.mock(jakarta.servlet.http.HttpSession.class);
                assertDoesNotThrow(() -> customer.notifyLogout(user, session));
        }

	// ---- lastModifiedMillis / lastCheckedMillis ----

	@Test
	void lastModifiedMillisDefaultsToMaxValue() {
		CustomerImpl customer = new CustomerImpl();
		assertEquals(Long.MAX_VALUE, customer.getLastModifiedMillis());
	}

	@Test
	void lastCheckedMillisRoundTrips() {
		CustomerImpl customer = new CustomerImpl();
		customer.setLastCheckedMillis(99L);
		assertEquals(99L, customer.getLastCheckedMillis());
	}

	@Test
	void defaultDateTimeConverterRoundTrips() {
		CustomerImpl customer = new CustomerImpl();
		assertNull(customer.getDefaultDateTimeConverter());
		DD_MM_YYYY_HH24_MI converter = new DD_MM_YYYY_HH24_MI();
		customer.setDefaultDateTimeConverter(converter);
		assertEquals(converter, customer.getDefaultDateTimeConverter());
	}

	@Test
	void defaultTimeConverterRoundTrips() {
		CustomerImpl customer = new CustomerImpl();
		assertNull(customer.getDefaultTimeConverter());
		HH24_MI converter = new HH24_MI();
		customer.setDefaultTimeConverter(converter);
		assertEquals(converter, customer.getDefaultTimeConverter());
	}

	@Test
	void defaultTimestampConverterRoundTrips() {
		CustomerImpl customer = new CustomerImpl();
		assertNull(customer.getDefaultTimestampConverter());
		DD_MM_YYYY_HH24_MI_SS converter = new DD_MM_YYYY_HH24_MI_SS();
		customer.setDefaultTimestampConverter(converter);
		assertEquals(converter, customer.getDefaultTimestampConverter());
	}

	// ---- helper to build a customer with one mock interceptor ----

	private static CustomerImpl customerWithInterceptor(Interceptor impl) {
		InterceptorMetaData imd = mock(InterceptorMetaData.class);
		when(imd.getClassName()).thenReturn("com.example.MockInterceptor");
		when(imd.getInterceptor()).thenReturn(impl);
		CustomerImpl customer = new CustomerImpl();
		customer.putInterceptor(imd);
		return customer;
	}

	private static CustomerImpl customerWithObserver(Observer impl) {
		ObserverMetaData omd = mock(ObserverMetaData.class);
		when(omd.getClassName()).thenReturn("com.example.MockObserver");
		when(omd.getObserver()).thenReturn(impl);
		CustomerImpl customer = new CustomerImpl();
		customer.putObserver(omd);
		return customer;
	}

	// ---- interceptBeforeNewInstance with interceptor ----

	@Test
	void interceptBeforeNewInstanceReturnsTrueWhenInterceptorVetoes() throws Exception {
		Interceptor interceptor = mock(Interceptor.class);
		when(interceptor.beforeNewInstance(any())).thenReturn(true);
		CustomerImpl customer = customerWithInterceptor(interceptor);
		assertTrue(customer.interceptBeforeNewInstance(null));
	}

	@Test
	void interceptBeforeNewInstanceReturnsFalseWhenInterceptorAllows() throws Exception {
		Interceptor interceptor = mock(Interceptor.class);
		when(interceptor.beforeNewInstance(any())).thenReturn(false);
		CustomerImpl customer = customerWithInterceptor(interceptor);
		assertFalse(customer.interceptBeforeNewInstance(null));
	}

	@Test
	void interceptAfterNewInstanceCallsInterceptor() throws Exception {
		Interceptor interceptor = mock(Interceptor.class);
		CustomerImpl customer = customerWithInterceptor(interceptor);
		customer.interceptAfterNewInstance(null);
		verify(interceptor).afterNewInstance(null);
	}

	// ---- interceptBeforeValidate with interceptor ----

	@Test
	void interceptBeforeValidateReturnsTrueWhenInterceptorVetoes() throws Exception {
		Interceptor interceptor = mock(Interceptor.class);
		when(interceptor.beforeValidate(any(), any())).thenReturn(true);
		CustomerImpl customer = customerWithInterceptor(interceptor);
		assertTrue(customer.interceptBeforeValidate(null, null));
	}

	@Test
	void interceptBeforeValidateReturnsFalseWhenInterceptorAllows() throws Exception {
		Interceptor interceptor = mock(Interceptor.class);
		when(interceptor.beforeValidate(any(), any())).thenReturn(false);
		CustomerImpl customer = customerWithInterceptor(interceptor);
		assertFalse(customer.interceptBeforeValidate(null, null));
	}

	@Test
	void interceptAfterValidateCallsInterceptor() throws Exception {
		Interceptor interceptor = mock(Interceptor.class);
		CustomerImpl customer = customerWithInterceptor(interceptor);
		customer.interceptAfterValidate(null, null);
		verify(interceptor).afterValidate(null, null);
	}

	// ---- interceptBeforeGetConstantDomainValues with interceptor ----

	@Test
	void interceptBeforeGetConstantDomainValuesReturnsTrueWhenInterceptorVetoes() throws Exception {
		Interceptor interceptor = mock(Interceptor.class);
		when(interceptor.beforeGetConstantDomainValues("attr")).thenReturn(true);
		CustomerImpl customer = customerWithInterceptor(interceptor);
		assertTrue(customer.interceptBeforeGetConstantDomainValues("attr"));
	}

	@Test
	void interceptBeforeGetConstantDomainValuesReturnsFalseWhenInterceptorAllows() throws Exception {
		Interceptor interceptor = mock(Interceptor.class);
		when(interceptor.beforeGetConstantDomainValues("attr")).thenReturn(false);
		CustomerImpl customer = customerWithInterceptor(interceptor);
		assertFalse(customer.interceptBeforeGetConstantDomainValues("attr"));
	}

	@Test
	void interceptAfterGetConstantDomainValuesCallsInterceptor() throws Exception {
		Interceptor interceptor = mock(Interceptor.class);
		CustomerImpl customer = customerWithInterceptor(interceptor);
		customer.interceptAfterGetConstantDomainValues("attr", new ArrayList<>());
		verify(interceptor).afterGetConstantDomainValues(any(), any());
	}

	// ---- interceptBeforeGetVariantDomainValues with interceptor ----

	@Test
	void interceptBeforeGetVariantDomainValuesReturnsTrueWhenInterceptorVetoes() throws Exception {
		Interceptor interceptor = mock(Interceptor.class);
		when(interceptor.beforeGetVariantDomainValues("attr")).thenReturn(true);
		CustomerImpl customer = customerWithInterceptor(interceptor);
		assertTrue(customer.interceptBeforeGetVariantDomainValues("attr"));
	}

	@Test
	void interceptBeforeGetVariantDomainValuesReturnsFalseWhenInterceptorAllows() throws Exception {
		Interceptor interceptor = mock(Interceptor.class);
		when(interceptor.beforeGetVariantDomainValues("attr")).thenReturn(false);
		CustomerImpl customer = customerWithInterceptor(interceptor);
		assertFalse(customer.interceptBeforeGetVariantDomainValues("attr"));
	}

	@Test
	void interceptAfterGetVariantDomainValuesCallsInterceptor() throws Exception {
		Interceptor interceptor = mock(Interceptor.class);
		CustomerImpl customer = customerWithInterceptor(interceptor);
		customer.interceptAfterGetVariantDomainValues("attr", new ArrayList<>());
		verify(interceptor).afterGetVariantDomainValues(any(), any());
	}

	// ---- interceptBeforeGetDynamicDomainValues with interceptor ----

	@Test
	void interceptBeforeGetDynamicDomainValuesReturnsTrueWhenInterceptorVetoes() throws Exception {
		Interceptor interceptor = mock(Interceptor.class);
		when(interceptor.beforeGetDynamicDomainValues(any(), any())).thenReturn(true);
		CustomerImpl customer = customerWithInterceptor(interceptor);
		assertTrue(customer.interceptBeforeGetDynamicDomainValues("attr", null));
	}

	@Test
	void interceptBeforeGetDynamicDomainValuesReturnsFalseWhenInterceptorAllows() throws Exception {
		Interceptor interceptor = mock(Interceptor.class);
		when(interceptor.beforeGetDynamicDomainValues(any(), any())).thenReturn(false);
		CustomerImpl customer = customerWithInterceptor(interceptor);
		assertFalse(customer.interceptBeforeGetDynamicDomainValues("attr", null));
	}

	@Test
	void interceptAfterGetDynamicDomainValuesCallsInterceptor() throws Exception {
		Interceptor interceptor = mock(Interceptor.class);
		CustomerImpl customer = customerWithInterceptor(interceptor);
		customer.interceptAfterGetDynamicDomainValues("attr", null, new ArrayList<>());
		verify(interceptor).afterGetDynamicDomainValues(any(), any(), any());
	}

	// ---- interceptBeforeComplete with interceptor ----

	@Test
	void interceptBeforeCompleteReturnsTrueWhenInterceptorVetoes() throws Exception {
		Interceptor interceptor = mock(Interceptor.class);
		when(interceptor.beforeComplete(any(), any(), any())).thenReturn(true);
		CustomerImpl customer = customerWithInterceptor(interceptor);
		assertTrue(customer.interceptBeforeComplete("attr", "val", null));
	}

	@Test
	void interceptBeforeCompleteReturnsFalseWhenInterceptorAllows() throws Exception {
		Interceptor interceptor = mock(Interceptor.class);
		when(interceptor.beforeComplete(any(), any(), any())).thenReturn(false);
		CustomerImpl customer = customerWithInterceptor(interceptor);
		assertFalse(customer.interceptBeforeComplete("attr", "val", null));
	}

	@Test
	void interceptAfterCompleteCallsInterceptor() throws Exception {
		Interceptor interceptor = mock(Interceptor.class);
		CustomerImpl customer = customerWithInterceptor(interceptor);
		customer.interceptAfterComplete("attr", "val", null, new ArrayList<>());
		verify(interceptor).afterComplete(any(), any(), any(), any());
	}

	// ---- interceptBeforeSave with interceptor ----

	@Test
	void interceptBeforeSaveReturnsTrueWhenInterceptorVetoes() throws Exception {
		Interceptor interceptor = mock(Interceptor.class);
		when(interceptor.beforeSave(any(), any())).thenReturn(true);
		CustomerImpl customer = customerWithInterceptor(interceptor);
		assertTrue(customer.interceptBeforeSave(null, null));
	}

	@Test
	void interceptBeforeSaveReturnsFalseWhenInterceptorAllows() throws Exception {
		Interceptor interceptor = mock(Interceptor.class);
		when(interceptor.beforeSave(any(), any())).thenReturn(false);
		CustomerImpl customer = customerWithInterceptor(interceptor);
		assertFalse(customer.interceptBeforeSave(null, null));
	}

	@Test
	void interceptAfterSaveCallsInterceptor() throws Exception {
		Interceptor interceptor = mock(Interceptor.class);
		CustomerImpl customer = customerWithInterceptor(interceptor);
		customer.interceptAfterSave(null, null);
		verify(interceptor).afterSave(any(), any());
	}

	// ---- interceptBeforePreSave with interceptor ----

	@Test
	void interceptBeforePreSaveReturnsTrueWhenInterceptorVetoes() throws Exception {
		Interceptor interceptor = mock(Interceptor.class);
		when(interceptor.beforePreSave(any())).thenReturn(true);
		CustomerImpl customer = customerWithInterceptor(interceptor);
		assertTrue(customer.interceptBeforePreSave(null));
	}

	@Test
	void interceptBeforePreSaveReturnsFalseWhenInterceptorAllows() throws Exception {
		Interceptor interceptor = mock(Interceptor.class);
		when(interceptor.beforePreSave(any())).thenReturn(false);
		CustomerImpl customer = customerWithInterceptor(interceptor);
		assertFalse(customer.interceptBeforePreSave(null));
	}

	@Test
	void interceptAfterPreSaveCallsInterceptor() throws Exception {
		Interceptor interceptor = mock(Interceptor.class);
		CustomerImpl customer = customerWithInterceptor(interceptor);
		customer.interceptAfterPreSave(null);
		verify(interceptor).afterPreSave(null);
	}

	// ---- interceptBeforePostSave with interceptor ----

	@Test
	void interceptBeforePostSaveReturnsTrueWhenInterceptorVetoes() throws Exception {
		Interceptor interceptor = mock(Interceptor.class);
		when(interceptor.beforePostSave(any())).thenReturn(true);
		CustomerImpl customer = customerWithInterceptor(interceptor);
		assertTrue(customer.interceptBeforePostSave(null));
	}

	@Test
	void interceptBeforePostSaveReturnsFalseWhenInterceptorAllows() throws Exception {
		Interceptor interceptor = mock(Interceptor.class);
		when(interceptor.beforePostSave(any())).thenReturn(false);
		CustomerImpl customer = customerWithInterceptor(interceptor);
		assertFalse(customer.interceptBeforePostSave(null));
	}

	@Test
	void interceptAfterPostSaveCallsInterceptor() throws Exception {
		Interceptor interceptor = mock(Interceptor.class);
		CustomerImpl customer = customerWithInterceptor(interceptor);
		customer.interceptAfterPostSave(null);
		verify(interceptor).afterPostSave(null);
	}

	// ---- interceptBeforeDelete with interceptor ----

	@Test
	void interceptBeforeDeleteReturnsTrueWhenInterceptorVetoes() throws Exception {
		Interceptor interceptor = mock(Interceptor.class);
		when(interceptor.beforeDelete(any(), any())).thenReturn(true);
		CustomerImpl customer = customerWithInterceptor(interceptor);
		assertTrue(customer.interceptBeforeDelete(null, null));
	}

	@Test
	void interceptBeforeDeleteReturnsFalseWhenInterceptorAllows() throws Exception {
		Interceptor interceptor = mock(Interceptor.class);
		when(interceptor.beforeDelete(any(), any())).thenReturn(false);
		CustomerImpl customer = customerWithInterceptor(interceptor);
		assertFalse(customer.interceptBeforeDelete(null, null));
	}

	@Test
	void interceptAfterDeleteCallsInterceptor() throws Exception {
		Interceptor interceptor = mock(Interceptor.class);
		CustomerImpl customer = customerWithInterceptor(interceptor);
		customer.interceptAfterDelete(null, null);
		verify(interceptor).afterDelete(any(), any());
	}

	// ---- interceptBeforePreDelete with interceptor ----

	@Test
	void interceptBeforePreDeleteReturnsTrueWhenInterceptorVetoes() throws Exception {
		Interceptor interceptor = mock(Interceptor.class);
		when(interceptor.beforePreDelete(any())).thenReturn(true);
		CustomerImpl customer = customerWithInterceptor(interceptor);
		assertTrue(customer.interceptBeforePreDelete(null));
	}

	@Test
	void interceptBeforePreDeleteReturnsFalseWhenInterceptorAllows() throws Exception {
		Interceptor interceptor = mock(Interceptor.class);
		when(interceptor.beforePreDelete(any())).thenReturn(false);
		CustomerImpl customer = customerWithInterceptor(interceptor);
		assertFalse(customer.interceptBeforePreDelete(null));
	}

	@Test
	void interceptAfterPreDeleteCallsInterceptor() throws Exception {
		Interceptor interceptor = mock(Interceptor.class);
		CustomerImpl customer = customerWithInterceptor(interceptor);
		customer.interceptAfterPreDelete(null);
		verify(interceptor).afterPreDelete(null);
	}

	// ---- interceptBeforePostDelete with interceptor ----

	@Test
	void interceptBeforePostDeleteReturnsTrueWhenInterceptorVetoes() throws Exception {
		Interceptor interceptor = mock(Interceptor.class);
		when(interceptor.beforePostDelete(any())).thenReturn(true);
		CustomerImpl customer = customerWithInterceptor(interceptor);
		assertTrue(customer.interceptBeforePostDelete(null));
	}

	@Test
	void interceptBeforePostDeleteReturnsFalseWhenInterceptorAllows() throws Exception {
		Interceptor interceptor = mock(Interceptor.class);
		when(interceptor.beforePostDelete(any())).thenReturn(false);
		CustomerImpl customer = customerWithInterceptor(interceptor);
		assertFalse(customer.interceptBeforePostDelete(null));
	}

	@Test
	void interceptAfterPostDeleteCallsInterceptor() throws Exception {
		Interceptor interceptor = mock(Interceptor.class);
		CustomerImpl customer = customerWithInterceptor(interceptor);
		customer.interceptAfterPostDelete(null);
		verify(interceptor).afterPostDelete(null);
	}

	// ---- interceptBeforePostLoad with interceptor ----

	@Test
	void interceptBeforePostLoadReturnsTrueWhenInterceptorVetoes() throws Exception {
		Interceptor interceptor = mock(Interceptor.class);
		when(interceptor.beforePostLoad(any())).thenReturn(true);
		CustomerImpl customer = customerWithInterceptor(interceptor);
		assertTrue(customer.interceptBeforePostLoad(null));
	}

	@Test
	void interceptBeforePostLoadReturnsFalseWhenInterceptorAllows() throws Exception {
		Interceptor interceptor = mock(Interceptor.class);
		when(interceptor.beforePostLoad(any())).thenReturn(false);
		CustomerImpl customer = customerWithInterceptor(interceptor);
		assertFalse(customer.interceptBeforePostLoad(null));
	}

	@Test
	void interceptAfterPostLoadCallsInterceptor() throws Exception {
		Interceptor interceptor = mock(Interceptor.class);
		CustomerImpl customer = customerWithInterceptor(interceptor);
		customer.interceptAfterPostLoad(null);
		verify(interceptor).afterPostLoad(null);
	}

	// ---- interceptBeforePreExecute with interceptor ----

	@Test
	void interceptBeforePreExecuteReturnsTrueWhenInterceptorVetoes() throws Exception {
		Interceptor interceptor = mock(Interceptor.class);
		when(interceptor.beforePreExecute(any(), any(), any(), any())).thenReturn(true);
		CustomerImpl customer = customerWithInterceptor(interceptor);
		assertTrue(customer.interceptBeforePreExecute(null, null, null, (WebContext) null));
	}

	@Test
	void interceptBeforePreExecuteReturnsFalseWhenInterceptorAllows() throws Exception {
		Interceptor interceptor = mock(Interceptor.class);
		when(interceptor.beforePreExecute(any(), any(), any(), any())).thenReturn(false);
		CustomerImpl customer = customerWithInterceptor(interceptor);
		assertFalse(customer.interceptBeforePreExecute(null, null, null, (WebContext) null));
	}

	@Test
	void interceptAfterPreExecuteCallsInterceptor() throws Exception {
		Interceptor interceptor = mock(Interceptor.class);
		CustomerImpl customer = customerWithInterceptor(interceptor);
		customer.interceptAfterPreExecute((ImplicitActionName) null, null, null, (WebContext) null);
		verify(interceptor).afterPreExecute(any(), any(), any(), any());
	}

	// ---- interceptBeforePreRerender with interceptor ----

	@Test
	void interceptBeforePreRerenderReturnsTrueWhenInterceptorVetoes() throws Exception {
		Interceptor interceptor = mock(Interceptor.class);
		when(interceptor.beforePreRerender(any(), any(), any())).thenReturn(true);
		CustomerImpl customer = customerWithInterceptor(interceptor);
		assertTrue(customer.interceptBeforePreRerender(null, null, (WebContext) null));
	}

	@Test
	void interceptBeforePreRerenderReturnsFalseWhenInterceptorAllows() throws Exception {
		Interceptor interceptor = mock(Interceptor.class);
		when(interceptor.beforePreRerender(any(), any(), any())).thenReturn(false);
		CustomerImpl customer = customerWithInterceptor(interceptor);
		assertFalse(customer.interceptBeforePreRerender(null, null, (WebContext) null));
	}

	@Test
	void interceptAfterPreRerenderCallsInterceptor() throws Exception {
		Interceptor interceptor = mock(Interceptor.class);
		CustomerImpl customer = customerWithInterceptor(interceptor);
		customer.interceptAfterPreRerender(null, null, (WebContext) null);
		verify(interceptor).afterPreRerender(any(), any(), any());
	}

	// ---- interceptBeforeServerSideAction with interceptor ----

	@Test
	void interceptBeforeServerSideActionReturnsTrueWhenInterceptorVetoes() throws Exception {
		Interceptor interceptor = mock(Interceptor.class);
		when(interceptor.beforeServerSideAction(any(), any(), any(), any())).thenReturn(true);
		CustomerImpl customer = customerWithInterceptor(interceptor);
		assertTrue(customer.interceptBeforeServerSideAction(null, null, null, (WebContext) null));
	}

	@Test
	void interceptBeforeServerSideActionReturnsFalseWhenInterceptorAllows() throws Exception {
		Interceptor interceptor = mock(Interceptor.class);
		when(interceptor.beforeServerSideAction(any(), any(), any(), any())).thenReturn(false);
		CustomerImpl customer = customerWithInterceptor(interceptor);
		assertFalse(customer.interceptBeforeServerSideAction(null, null, null, (WebContext) null));
	}

	@Test
	@SuppressWarnings("unchecked")
	void interceptAfterServerSideActionCallsInterceptor() throws Exception {
		Interceptor interceptor = mock(Interceptor.class);
		CustomerImpl customer = customerWithInterceptor(interceptor);
		customer.interceptAfterServerSideAction(null, null, (ServerSideActionResult<org.skyve.domain.Bean>) null, (WebContext) null);
		verify(interceptor).afterServerSideAction(any(), any(), any(), any());
	}

	// ---- interceptBeforeDownloadAction with interceptor ----

	@Test
	void interceptBeforeDownloadActionReturnsTrueWhenInterceptorVetoes() throws Exception {
		Interceptor interceptor = mock(Interceptor.class);
		when(interceptor.beforeDownloadAction(any(), any(), any(), any())).thenReturn(true);
		CustomerImpl customer = customerWithInterceptor(interceptor);
		assertTrue(customer.interceptBeforeDownloadAction(null, null, null, (WebContext) null));
	}

	@Test
	void interceptBeforeDownloadActionReturnsFalseWhenInterceptorAllows() throws Exception {
		Interceptor interceptor = mock(Interceptor.class);
		when(interceptor.beforeDownloadAction(any(), any(), any(), any())).thenReturn(false);
		CustomerImpl customer = customerWithInterceptor(interceptor);
		assertFalse(customer.interceptBeforeDownloadAction(null, null, null, (WebContext) null));
	}

	@Test
	void interceptAfterDownloadActionCallsInterceptor() throws Exception {
		Interceptor interceptor = mock(Interceptor.class);
		CustomerImpl customer = customerWithInterceptor(interceptor);
		customer.interceptAfterDownloadAction(null, null, null, null, (WebContext) null);
		verify(interceptor).afterDownloadAction(any(), any(), any(), any(), any());
	}

	// ---- interceptBeforeUploadAction with interceptor ----

	@Test
	void interceptBeforeUploadActionReturnsTrueWhenInterceptorVetoes() throws Exception {
		Interceptor interceptor = mock(Interceptor.class);
		when(interceptor.beforeUploadAction(any(), any(), any(), any(), any())).thenReturn(true);
		CustomerImpl customer = customerWithInterceptor(interceptor);
		assertTrue(customer.interceptBeforeUploadAction(null, null, null, null, (WebContext) null));
	}

	@Test
	void interceptBeforeUploadActionReturnsFalseWhenInterceptorAllows() throws Exception {
		Interceptor interceptor = mock(Interceptor.class);
		when(interceptor.beforeUploadAction(any(), any(), any(), any(), any())).thenReturn(false);
		CustomerImpl customer = customerWithInterceptor(interceptor);
		assertFalse(customer.interceptBeforeUploadAction(null, null, null, null, (WebContext) null));
	}

	@Test
	void interceptAfterUploadActionCallsInterceptor() throws Exception {
		Interceptor interceptor = mock(Interceptor.class);
		CustomerImpl customer = customerWithInterceptor(interceptor);
		customer.interceptAfterUploadAction(null, null, null, null, (WebContext) null);
		verify(interceptor).afterUploadAction(any(), any(), any(), any(), any());
	}

	// ---- interceptBeforeBizImportAction with interceptor ----

	@Test
	void interceptBeforeBizImportActionReturnsTrueWhenInterceptorVetoes() throws Exception {
		Interceptor interceptor = mock(Interceptor.class);
		when(interceptor.beforeBizImportAction(any(), any(), any(), any())).thenReturn(true);
		CustomerImpl customer = customerWithInterceptor(interceptor);
		assertTrue(customer.interceptBeforeBizImportAction(null, null, null, null));
	}

	@Test
	void interceptBeforeBizImportActionReturnsFalseWhenInterceptorAllows() throws Exception {
		Interceptor interceptor = mock(Interceptor.class);
		when(interceptor.beforeBizImportAction(any(), any(), any(), any())).thenReturn(false);
		CustomerImpl customer = customerWithInterceptor(interceptor);
		assertFalse(customer.interceptBeforeBizImportAction(null, null, null, null));
	}

	@Test
	void interceptAfterBizImportActionCallsInterceptor() throws Exception {
		Interceptor interceptor = mock(Interceptor.class);
		CustomerImpl customer = customerWithInterceptor(interceptor);
		customer.interceptAfterBizImportAction(null, null, null, null);
		verify(interceptor).afterBizImportAction(any(), any(), any(), any());
	}

	// ---- interceptBeforeBizExportAction with interceptor ----

	@Test
	void interceptBeforeBizExportActionReturnsTrueWhenInterceptorVetoes() throws Exception {
		Interceptor interceptor = mock(Interceptor.class);
		when(interceptor.beforeBizExportAction(any(), any(), any())).thenReturn(true);
		CustomerImpl customer = customerWithInterceptor(interceptor);
		assertTrue(customer.interceptBeforeBizExportAction(null, null, (WebContext) null));
	}

	@Test
	void interceptBeforeBizExportActionReturnsFalseWhenInterceptorAllows() throws Exception {
		Interceptor interceptor = mock(Interceptor.class);
		when(interceptor.beforeBizExportAction(any(), any(), any())).thenReturn(false);
		CustomerImpl customer = customerWithInterceptor(interceptor);
		assertFalse(customer.interceptBeforeBizExportAction(null, null, (WebContext) null));
	}

	@Test
	void interceptAfterBizExportActionCallsInterceptor() throws Exception {
		Interceptor interceptor = mock(Interceptor.class);
		CustomerImpl customer = customerWithInterceptor(interceptor);
		customer.interceptAfterBizExportAction(null, null, null, (WebContext) null);
		verify(interceptor).afterBizExportAction(any(), any(), any(), any());
	}

	// ---- interceptBeforePostRender with interceptor ----

	@Test
	void interceptBeforePostRenderReturnsTrueWhenInterceptorVetoes() {
		Interceptor interceptor = mock(Interceptor.class);
		when(interceptor.beforePostRender(any(), any())).thenReturn(true);
		CustomerImpl customer = customerWithInterceptor(interceptor);
		assertTrue(customer.interceptBeforePostRender(null, (WebContext) null));
	}

	@Test
	void interceptBeforePostRenderReturnsFalseWhenInterceptorAllows() {
		Interceptor interceptor = mock(Interceptor.class);
		when(interceptor.beforePostRender(any(), any())).thenReturn(false);
		CustomerImpl customer = customerWithInterceptor(interceptor);
		assertFalse(customer.interceptBeforePostRender(null, (WebContext) null));
	}

	@Test
	void interceptAfterPostRenderCallsInterceptor() {
		Interceptor interceptor = mock(Interceptor.class);
		CustomerImpl customer = customerWithInterceptor(interceptor);
		customer.interceptAfterPostRender(null, (WebContext) null);
		verify(interceptor).afterPostRender(any(), any());
	}

	// ---- observer notify methods with observer ----

	@Test
	void notifyStartupCallsObserver() {
		Observer observer = mock(Observer.class);
		CustomerImpl customer = customerWithObserver(observer);
		customer.notifyStartup();
		verify(observer).startup(customer);
	}

	@Test
	void notifyShutdownCallsObserver() {
		Observer observer = mock(Observer.class);
		CustomerImpl customer = customerWithObserver(observer);
		customer.notifyShutdown();
		verify(observer).shutdown(customer);
	}

	@Test
	void notifyBeforeBackupCallsObserver() {
		Observer observer = mock(Observer.class);
		CustomerImpl customer = customerWithObserver(observer);
		customer.notifyBeforeBackup();
		verify(observer).beforeBackup(customer);
	}

	@Test
	void notifyAfterBackupCallsObserver() {
		Observer observer = mock(Observer.class);
		CustomerImpl customer = customerWithObserver(observer);
		customer.notifyAfterBackup();
		verify(observer).afterBackup(customer);
	}

	@Test
	void notifyBeforeRestoreCallsObserver() {
		Observer observer = mock(Observer.class);
		CustomerImpl customer = customerWithObserver(observer);
		customer.notifyBeforeRestore();
		verify(observer).beforeRestore(customer);
	}

	@Test
	void notifyAfterRestoreCallsObserver() {
		Observer observer = mock(Observer.class);
		CustomerImpl customer = customerWithObserver(observer);
		customer.notifyAfterRestore();
		verify(observer).afterRestore(customer);
	}

	@Test
	void notifyLoginCallsObserver() throws Exception {
		Observer observer = mock(Observer.class);
		CustomerImpl customer = customerWithObserver(observer);
		org.skyve.metadata.user.User user = mock(org.skyve.metadata.user.User.class);
		jakarta.servlet.http.HttpSession session = mock(jakarta.servlet.http.HttpSession.class);
		customer.notifyLogin(user, session);
		verify(observer).login(user, session);
	}

	@Test
	void notifyLogoutCallsObserver() throws Exception {
		Observer observer = mock(Observer.class);
		CustomerImpl customer = customerWithObserver(observer);
		org.skyve.metadata.user.User user = mock(org.skyve.metadata.user.User.class);
		jakarta.servlet.http.HttpSession session = mock(jakarta.servlet.http.HttpSession.class);
		customer.notifyLogout(user, session);
		verify(observer).logout(user, session);
	}

	// ---- derivations / exportedReferences ----

	@Test
	@SuppressWarnings("static-method")
	void getBaseDocumentReturnsNullWhenDerivationsMapIsEmpty() {
		CustomerImpl customer = new CustomerImpl();
		Document doc = Mockito.mock(Document.class);
		Mockito.when(doc.getOwningModuleName()).thenReturn("admin");
		Mockito.when(doc.getName()).thenReturn("User");
		assertNull(customer.getBaseDocument(doc));
	}

	@Test
	@SuppressWarnings("static-method")
	void getDerivedDocumentsReturnsEmptyListWhenDerivationsMapIsEmpty() {
		CustomerImpl customer = new CustomerImpl();
		Document doc = Mockito.mock(Document.class);
		Mockito.when(doc.getOwningModuleName()).thenReturn("admin");
		Mockito.when(doc.getName()).thenReturn("User");
		List<String> result = customer.getDerivedDocuments(doc);
		assertNotNull(result);
		assertTrue(result.isEmpty());
	}

	@Test
	@SuppressWarnings({"static-method", "unchecked"})
	void getDerivedDocumentsReturnsMatchingEntriesWhenDerivationsPopulated() throws Exception {
		CustomerImpl customer = new CustomerImpl();
		java.lang.reflect.Field derivationsField = CustomerImpl.class.getDeclaredField("derivations");
		derivationsField.setAccessible(true);
		Map<String, String> derivations = (Map<String, String>) derivationsField.get(customer);
		derivations.put("admin.SpecialUser", "admin.User");

		Document doc = Mockito.mock(Document.class);
		Mockito.when(doc.getOwningModuleName()).thenReturn("admin");
		Mockito.when(doc.getName()).thenReturn("User");

		List<String> result = customer.getDerivedDocuments(doc);
		assertEquals(1, result.size());
		assertEquals("admin.SpecialUser", result.get(0));
	}

	@Test
	@SuppressWarnings({"static-method", "unchecked"})
	void getBaseDocumentReturnsValueWhenDerivationsContainsKey() throws Exception {
		CustomerImpl customer = new CustomerImpl();
		java.lang.reflect.Field derivationsField = CustomerImpl.class.getDeclaredField("derivations");
		derivationsField.setAccessible(true);
		Map<String, String> derivations = (Map<String, String>) derivationsField.get(customer);
		derivations.put("admin.SpecialUser", "admin.User");

		Document doc = Mockito.mock(Document.class);
		Mockito.when(doc.getOwningModuleName()).thenReturn("admin");
		Mockito.when(doc.getName()).thenReturn("SpecialUser");

		assertEquals("admin.User", customer.getBaseDocument(doc));
	}

	@Test
	@SuppressWarnings("static-method")
	void getExportedReferencesReturnsNullWhenMapIsEmpty() {
		CustomerImpl customer = new CustomerImpl();
		Document doc = Mockito.mock(Document.class);
		Mockito.when(doc.getOwningModuleName()).thenReturn("admin");
		Mockito.when(doc.getName()).thenReturn("User");
		assertNull(customer.getExportedReferences(doc));
	}
}