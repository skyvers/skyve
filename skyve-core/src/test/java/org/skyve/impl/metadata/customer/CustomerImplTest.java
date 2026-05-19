package org.skyve.impl.metadata.customer;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import org.skyve.domain.types.converters.date.DD_MMM_YYYY;
import org.skyve.domain.types.converters.datetime.DD_MM_YYYY_HH24_MI;
import org.skyve.domain.types.converters.time.HH24_MI;
import org.skyve.domain.types.converters.timestamp.DD_MM_YYYY_HH24_MI_SS;
import org.skyve.impl.metadata.repository.customer.CustomerRoleMetaData;
import org.skyve.metadata.customer.CustomerRole;
import org.skyve.metadata.customer.InterceptorMetaData;
import org.skyve.metadata.customer.ObserverMetaData;
import org.skyve.metadata.customer.UIResources;
import org.skyve.metadata.customer.HTMLResources;
import org.skyve.metadata.customer.LoginResources;
import org.skyve.metadata.module.Module;
import org.skyve.web.WebContext;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

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
		customer.interceptAfterNewInstance(null);
	}

	@Test
	void testInterceptBeforeValidateWithNoInterceptors() throws Exception {
		CustomerImpl customer = new CustomerImpl();
		assertFalse(customer.interceptBeforeValidate(null, null));
	}

	@Test
	void testInterceptAfterValidateWithNoInterceptors() throws Exception {
		CustomerImpl customer = new CustomerImpl();
		customer.interceptAfterValidate(null, null);
	}

	@Test
	void testInterceptBeforeGetConstantDomainValuesWithNoInterceptors() throws Exception {
		CustomerImpl customer = new CustomerImpl();
		assertFalse(customer.interceptBeforeGetConstantDomainValues("attr"));
	}

	@Test
	void testInterceptAfterGetConstantDomainValuesWithNoInterceptors() throws Exception {
		CustomerImpl customer = new CustomerImpl();
		customer.interceptAfterGetConstantDomainValues("attr", new ArrayList<>());
	}

	@Test
	void testInterceptBeforeGetVariantDomainValuesWithNoInterceptors() throws Exception {
		CustomerImpl customer = new CustomerImpl();
		assertFalse(customer.interceptBeforeGetVariantDomainValues("attr"));
	}

	@Test
	void testInterceptAfterGetVariantDomainValuesWithNoInterceptors() throws Exception {
		CustomerImpl customer = new CustomerImpl();
		customer.interceptAfterGetVariantDomainValues("attr", new ArrayList<>());
	}

	@Test
	void testInterceptBeforeGetDynamicDomainValuesWithNoInterceptors() throws Exception {
		CustomerImpl customer = new CustomerImpl();
		assertFalse(customer.interceptBeforeGetDynamicDomainValues("attr", null));
	}

	@Test
	void testInterceptAfterGetDynamicDomainValuesWithNoInterceptors() throws Exception {
		CustomerImpl customer = new CustomerImpl();
		customer.interceptAfterGetDynamicDomainValues("attr", null, new ArrayList<>());
	}

	@Test
	void testInterceptBeforeCompleteWithNoInterceptors() throws Exception {
		CustomerImpl customer = new CustomerImpl();
		assertFalse(customer.interceptBeforeComplete("attr", "val", null));
	}

	@Test
	void testInterceptAfterCompleteWithNoInterceptors() throws Exception {
		CustomerImpl customer = new CustomerImpl();
		customer.interceptAfterComplete("attr", "val", null, new ArrayList<>());
	}

	@Test
	void testInterceptBeforeSaveWithNoInterceptors() throws Exception {
		CustomerImpl customer = new CustomerImpl();
		assertFalse(customer.interceptBeforeSave(null, null));
	}

	@Test
	void testInterceptAfterSaveWithNoInterceptors() throws Exception {
		CustomerImpl customer = new CustomerImpl();
		customer.interceptAfterSave(null, null);
	}

	@Test
	void testInterceptBeforePreSaveWithNoInterceptors() throws Exception {
		CustomerImpl customer = new CustomerImpl();
		assertFalse(customer.interceptBeforePreSave(null));
	}

	@Test
	void testInterceptAfterPreSaveWithNoInterceptors() throws Exception {
		CustomerImpl customer = new CustomerImpl();
		customer.interceptAfterPreSave(null);
	}

	@Test
	void testInterceptBeforePostSaveWithNoInterceptors() throws Exception {
		CustomerImpl customer = new CustomerImpl();
		assertFalse(customer.interceptBeforePostSave(null));
	}

	@Test
	void testInterceptAfterPostSaveWithNoInterceptors() throws Exception {
		CustomerImpl customer = new CustomerImpl();
		customer.interceptAfterPostSave(null);
	}

	@Test
	void testInterceptBeforeDeleteWithNoInterceptors() throws Exception {
		CustomerImpl customer = new CustomerImpl();
		assertFalse(customer.interceptBeforeDelete(null, null));
	}

	@Test
	void testInterceptAfterDeleteWithNoInterceptors() throws Exception {
		CustomerImpl customer = new CustomerImpl();
		customer.interceptAfterDelete(null, null);
	}

	@Test
	void testInterceptBeforePreDeleteWithNoInterceptors() throws Exception {
		CustomerImpl customer = new CustomerImpl();
		assertFalse(customer.interceptBeforePreDelete(null));
	}

	@Test
	void testInterceptAfterPreDeleteWithNoInterceptors() throws Exception {
		CustomerImpl customer = new CustomerImpl();
		customer.interceptAfterPreDelete(null);
	}

	@Test
	void testInterceptBeforePostDeleteWithNoInterceptors() throws Exception {
		CustomerImpl customer = new CustomerImpl();
		assertFalse(customer.interceptBeforePostDelete(null));
	}

	@Test
	void testInterceptAfterPostDeleteWithNoInterceptors() throws Exception {
		CustomerImpl customer = new CustomerImpl();
		customer.interceptAfterPostDelete(null);
	}

	@Test
	void testInterceptBeforePostLoadWithNoInterceptors() throws Exception {
		CustomerImpl customer = new CustomerImpl();
		assertFalse(customer.interceptBeforePostLoad(null));
	}

	@Test
	void testInterceptAfterPostLoadWithNoInterceptors() throws Exception {
		CustomerImpl customer = new CustomerImpl();
		customer.interceptAfterPostLoad(null);
	}

	@Test
	void testInterceptBeforePreExecuteWithNoInterceptors() throws Exception {
		CustomerImpl customer = new CustomerImpl();
		assertFalse(customer.interceptBeforePreExecute(null, null, null, (WebContext) null));
	}

	@Test
	void testInterceptAfterPreExecuteWithNoInterceptors() throws Exception {
		CustomerImpl customer = new CustomerImpl();
		customer.interceptAfterPreExecute(null, null, null, (WebContext) null);
	}

	@Test
	void testInterceptBeforePreRerenderWithNoInterceptors() throws Exception {
		CustomerImpl customer = new CustomerImpl();
		assertFalse(customer.interceptBeforePreRerender(null, null, (WebContext) null));
	}

	@Test
	void testInterceptAfterPreRerenderWithNoInterceptors() throws Exception {
		CustomerImpl customer = new CustomerImpl();
		customer.interceptAfterPreRerender(null, null, (WebContext) null);
	}

	@Test
	void testInterceptBeforeServerSideActionWithNoInterceptors() throws Exception {
		CustomerImpl customer = new CustomerImpl();
		assertFalse(customer.interceptBeforeServerSideAction(null, null, null, (WebContext) null));
	}

	@Test
	void testInterceptAfterServerSideActionWithNoInterceptors() throws Exception {
		CustomerImpl customer = new CustomerImpl();
		customer.interceptAfterServerSideAction(null, null, null, (WebContext) null);
	}

	@Test
	void testInterceptBeforeDownloadActionWithNoInterceptors() throws Exception {
		CustomerImpl customer = new CustomerImpl();
		assertFalse(customer.interceptBeforeDownloadAction(null, null, null, (WebContext) null));
	}

	@Test
	void testInterceptAfterDownloadActionWithNoInterceptors() throws Exception {
		CustomerImpl customer = new CustomerImpl();
		customer.interceptAfterDownloadAction(null, null, null, null, (WebContext) null);
	}

	@Test
	void testInterceptBeforeUploadActionWithNoInterceptors() throws Exception {
		CustomerImpl customer = new CustomerImpl();
		assertFalse(customer.interceptBeforeUploadAction(null, null, null, null, (WebContext) null));
	}

	@Test
	void testInterceptAfterUploadActionWithNoInterceptors() throws Exception {
		CustomerImpl customer = new CustomerImpl();
		customer.interceptAfterUploadAction(null, null, null, null, (WebContext) null);
	}

	@Test
	void testInterceptBeforeBizImportActionWithNoInterceptors() throws Exception {
		CustomerImpl customer = new CustomerImpl();
		assertFalse(customer.interceptBeforeBizImportAction(null, null, null, null));
	}

	@Test
	void testInterceptAfterBizImportActionWithNoInterceptors() throws Exception {
		CustomerImpl customer = new CustomerImpl();
		customer.interceptAfterBizImportAction(null, null, null, null);
	}

	@Test
	void testInterceptBeforeBizExportActionWithNoInterceptors() throws Exception {
		CustomerImpl customer = new CustomerImpl();
		assertFalse(customer.interceptBeforeBizExportAction(null, null, (WebContext) null));
	}

	@Test
	void testInterceptAfterBizExportActionWithNoInterceptors() throws Exception {
		CustomerImpl customer = new CustomerImpl();
		customer.interceptAfterBizExportAction(null, null, null, (WebContext) null);
	}

	@Test
	void testInterceptBeforePostRenderWithNoInterceptors() {
		CustomerImpl customer = new CustomerImpl();
		assertFalse(customer.interceptBeforePostRender(null, (WebContext) null));
	}

	@Test
	void testInterceptAfterPostRenderWithNoInterceptors() {
		CustomerImpl customer = new CustomerImpl();
		customer.interceptAfterPostRender(null, (WebContext) null);
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
                customer.notifyStartup();
        }

        @Test
        void testNotifyShutdownWithNoObserversDoesNotThrow() {
                CustomerImpl customer = new CustomerImpl();
                customer.notifyShutdown();
        }

        @Test
        void testNotifyBeforeBackupWithNoObserversDoesNotThrow() {
                CustomerImpl customer = new CustomerImpl();
                customer.notifyBeforeBackup();
        }

        @Test
        void testNotifyAfterBackupWithNoObserversDoesNotThrow() {
                CustomerImpl customer = new CustomerImpl();
                customer.notifyAfterBackup();
        }

        @Test
        void testNotifyBeforeRestoreWithNoObserversDoesNotThrow() {
                CustomerImpl customer = new CustomerImpl();
                customer.notifyBeforeRestore();
        }

        @Test
        void testNotifyAfterRestoreWithNoObserversDoesNotThrow() {
                CustomerImpl customer = new CustomerImpl();
                customer.notifyAfterRestore();
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
                customer.setHomeModuleName("admin");
                // getHomeModuleName is not public but setHomeModuleName must not throw
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
                customer.notifyLogin(user, session);
        }

        @Test
        void testNotifyLogoutWithNoObserversDoesNotThrow() {
                CustomerImpl customer = new CustomerImpl();
                org.skyve.metadata.user.User user = Mockito.mock(org.skyve.metadata.user.User.class);
                jakarta.servlet.http.HttpSession session = Mockito.mock(jakarta.servlet.http.HttpSession.class);
                customer.notifyLogout(user, session);
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
}