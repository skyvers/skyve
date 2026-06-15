package org.skyve.impl.cdi;

import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;
import java.util.Collection;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

import org.junit.Test;
import org.skyve.domain.Bean;
import org.skyve.domain.types.DateOnly;
import org.skyve.domain.types.DateTime;
import org.skyve.domain.types.TimeOnly;
import org.skyve.domain.types.Timestamp;
import org.skyve.domain.types.converters.Converter;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.customer.CustomerRole;
import org.skyve.metadata.customer.HTMLResources;
import org.skyve.metadata.customer.InterceptorMetaData;
import org.skyve.metadata.customer.LoginResources;
import org.skyve.metadata.customer.ObserverMetaData;
import org.skyve.metadata.customer.UIResources;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.DocumentPermissionScope;
import org.skyve.metadata.user.User;
import org.skyve.metadata.user.UserAccess;

@SuppressWarnings({ "static-method", "unchecked", "boxing" })
public class CoreInjectablesDelegateTest {
	@Test
	@SuppressWarnings("java:S5961")
	public void userInjectableDelegatesToCoreUser() throws Exception {
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		Map<String, Object> attributes = Map.of("k", "v");
		Set<String> modules = Set.of("admin");
		Locale locale = Locale.ENGLISH;
		Document document = mock(Document.class);
		DocumentPermissionScope scope = DocumentPermissionScope.customer;
		when(user.getName()).thenReturn("mike");
		when(user.getId()).thenReturn("u1");
		when(user.getSessionId()).thenReturn("s1");
		when(user.getLanguageTag()).thenReturn("en-AU");
		when(user.getLocale()).thenReturn(locale);
		when(user.getPasswordHash()).thenReturn("hash");
		when(user.isPasswordChangeRequired()).thenReturn(Boolean.TRUE);
		when(user.getContactId()).thenReturn("c1");
		when(user.getContactName()).thenReturn("Mike");
		when(user.getContactImageId()).thenReturn("img");
		when(user.getContactImageUrl(64, 64)).thenReturn("url");
		when(user.getContactAvatarInitials()).thenReturn("MC");
		when(user.getCustomer()).thenReturn(customer);
		when(user.getCustomerName()).thenReturn("bizhub");
		when(user.getDataGroupId()).thenReturn("dg");
		when(user.getHomeModuleName()).thenReturn("admin");
		when(user.getAccessibleModuleNames()).thenReturn(modules);
		when(user.isInRole("admin", "RoleA")).thenReturn(Boolean.TRUE);
		when(user.getScope("admin", "DocA")).thenReturn(scope);
		when(user.canReadBean("1", "m", "d", "c", "g", "u")).thenReturn(Boolean.TRUE);
		when(user.canAccessContent("1", "m", "d", "c", "g", "u", "att")).thenReturn(Boolean.TRUE);
		when(user.canAccessDocument(document)).thenReturn(Boolean.TRUE);
		when(user.canCreateDocument(document)).thenReturn(Boolean.TRUE);
		when(user.canFlag()).thenReturn(Boolean.TRUE);
		when(user.canReadDocument(document)).thenReturn(Boolean.TRUE);
		when(user.canTextSearch()).thenReturn(Boolean.TRUE);
		when(user.canSwitchMode()).thenReturn(Boolean.TRUE);
		when(user.canUpdateDocument(document)).thenReturn(Boolean.TRUE);
		when(user.canDeleteDocument(document)).thenReturn(Boolean.TRUE);
		when(user.canExecuteAction(document, "go")).thenReturn(Boolean.TRUE);
		UserAccess access = mock(UserAccess.class);
		when(user.canAccess(access, "ux")).thenReturn(Boolean.TRUE);
		when(user.getAttributes()).thenReturn(attributes);

		withThreadLocalPersistence(user, () -> {
			UserInjectable injectable = new UserInjectable();
			assertSame("mike", injectable.getName());
			assertSame("u1", injectable.getId());
			injectable.setId("u2");
			assertSame("s1", injectable.getSessionId());
			assertSame("en-AU", injectable.getLanguageTag());
			assertSame(locale, injectable.getLocale());
			assertSame("hash", injectable.getPasswordHash());
			assertTrue(injectable.isPasswordChangeRequired());
			assertSame("c1", injectable.getContactId());
			assertSame("Mike", injectable.getContactName());
			assertSame("img", injectable.getContactImageId());
			assertSame("url", injectable.getContactImageUrl(64, 64));
			assertSame("MC", injectable.getContactAvatarInitials());
			assertSame(customer, injectable.getCustomer());
			assertSame("bizhub", injectable.getCustomerName());
			injectable.setCustomerName("newCust");
			assertSame("dg", injectable.getDataGroupId());
			injectable.setDataGroupId("newDg");
			assertSame("admin", injectable.getHomeModuleName());
			assertSame(modules, injectable.getAccessibleModuleNames());
			assertTrue(injectable.isInRole("admin", "RoleA"));
			assertSame(scope, injectable.getScope("admin", "DocA"));
			assertTrue(injectable.canReadBean("1", "m", "d", "c", "g", "u"));
			assertTrue(injectable.canAccessContent("1", "m", "d", "c", "g", "u", "att"));
			assertTrue(injectable.canAccessDocument(document));
			assertTrue(injectable.canCreateDocument(document));
			assertTrue(injectable.canFlag());
			assertTrue(injectable.canReadDocument(document));
			assertTrue(injectable.canTextSearch());
			assertTrue(injectable.canSwitchMode());
			assertTrue(injectable.canUpdateDocument(document));
			assertTrue(injectable.canDeleteDocument(document));
			assertTrue(injectable.canExecuteAction(document, "go"));
			assertTrue(injectable.canAccess(access, "ux"));
			assertSame(attributes, injectable.getAttributes());
		});

		verify(user).setId("u2");
		verify(user).setCustomerName("newCust");
		verify(user).setDataGroupId("newDg");
	}

	@Test
	@SuppressWarnings("java:S5961")
	public void customerInjectableDelegatesToCoreCustomer() throws Exception {
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		List<Module> modules = List.of(module);
		Collection<CustomerRole> roles = List.of(mock(CustomerRole.class));
		Collection<InterceptorMetaData> interceptors = List.of(mock(InterceptorMetaData.class));
		Collection<ObserverMetaData> observers = List.of(mock(ObserverMetaData.class));
		UIResources ui = mock(UIResources.class);
		HTMLResources html = mock(HTMLResources.class);
		LoginResources login = mock(LoginResources.class);
		Bizlet<Bean> bizlet = mock(Bizlet.class);
		Attribute attribute = mock(Attribute.class);
		List<DomainValue> values = List.of(mock(DomainValue.class));
		Converter<DateOnly> dateConverter = mock(Converter.class);
		Converter<DateTime> dateTimeConverter = mock(Converter.class);
		Converter<TimeOnly> timeConverter = mock(Converter.class);
		Converter<Timestamp> timestampConverter = mock(Converter.class);
		when(user.getCustomer()).thenReturn(customer);
		when(customer.getName()).thenReturn("bizhub");
		when(customer.getLastModifiedMillis()).thenReturn(1L);
		when(customer.getLastCheckedMillis()).thenReturn(2L);
		when(customer.getLanguageTag()).thenReturn("en-AU");
		when(customer.getDefaultDateConverter()).thenReturn(dateConverter);
		when(customer.getDefaultDateTimeConverter()).thenReturn(dateTimeConverter);
		when(customer.getDefaultTimeConverter()).thenReturn(timeConverter);
		when(customer.getDefaultTimestampConverter()).thenReturn(timestampConverter);
		when(customer.getHomeModule()).thenReturn(module);
		when(customer.getModule("admin")).thenReturn(module);
		when(customer.getModules()).thenReturn(modules);
		when(customer.getRoles()).thenReturn(roles);
		when(customer.getRole("RoleA")).thenReturn(mock(CustomerRole.class));
		when(customer.isAllowModuleRoles()).thenReturn(Boolean.TRUE);
		when(customer.getInterceptors()).thenReturn(interceptors);
		when(customer.getObservers()).thenReturn(observers);
		when(customer.getUiResources()).thenReturn(ui);
		when(customer.getHtmlResources()).thenReturn(html);
		when(customer.getLoginResources()).thenReturn(login);
		when(customer.getJFreeChartPostProcessorClassName()).thenReturn("JFree");
		when(customer.getPrimeFacesChartPostProcessorClassName()).thenReturn("PrimeFaces");
		when(customer.getConstantDomainValues(bizlet, "m", "d", attribute)).thenReturn(values);

		withThreadLocalPersistence(user, () -> {
			CustomerInjectable injectable = new CustomerInjectable();
			assertSame("bizhub", injectable.getName());
			assertSame(Long.valueOf(1L), Long.valueOf(injectable.getLastModifiedMillis()));
			assertSame(Long.valueOf(2L), Long.valueOf(injectable.getLastCheckedMillis()));
			injectable.setLastCheckedMillis(3L);
			assertSame("en-AU", injectable.getLanguageTag());
			assertSame(dateConverter, injectable.getDefaultDateConverter());
			assertSame(dateTimeConverter, injectable.getDefaultDateTimeConverter());
			assertSame(timeConverter, injectable.getDefaultTimeConverter());
			assertSame(timestampConverter, injectable.getDefaultTimestampConverter());
			assertSame(module, injectable.getHomeModule());
			assertSame(module, injectable.getModule("admin"));
			assertSame(modules, injectable.getModules());
			assertSame(roles, injectable.getRoles());
			assertSame(customer.getRole("RoleA"), injectable.getRole("RoleA"));
			assertTrue(injectable.isAllowModuleRoles());
			assertSame(interceptors, injectable.getInterceptors());
			assertSame(observers, injectable.getObservers());
			assertSame(ui, injectable.getUiResources());
			assertSame(html, injectable.getHtmlResources());
			assertSame(login, injectable.getLoginResources());
			assertSame("JFree", injectable.getJFreeChartPostProcessorClassName());
			assertSame("PrimeFaces", injectable.getPrimeFacesChartPostProcessorClassName());
			assertSame(values, injectable.getConstantDomainValues(bizlet, "m", "d", attribute));
			injectable.determineDependencies();
		});

		verify(customer).setLastCheckedMillis(3L);
		verify(customer).determineDependencies();
	}

	private interface ThrowingRunnable {
		void run() throws Exception;
	}

	private static void withThreadLocalPersistence(User user, ThrowingRunnable runnable) throws Exception {
		ThreadLocal<AbstractPersistence> threadLocal = getThreadLocalPersistence();
		AbstractPersistence original = threadLocal.get();
		AbstractPersistence persistence = mock(AbstractPersistence.class);
		when(persistence.getUser()).thenReturn(user);
		try {
			threadLocal.set(persistence);
			runnable.run();
		}
		finally {
			if (original == null) {
				threadLocal.remove();
			}
			else {
				threadLocal.set(original);
			}
		}
	}

	private static ThreadLocal<AbstractPersistence> getThreadLocalPersistence() throws Exception {
		Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
		field.setAccessible(true);
		return (ThreadLocal<AbstractPersistence>) field.get(null);
	}
}
