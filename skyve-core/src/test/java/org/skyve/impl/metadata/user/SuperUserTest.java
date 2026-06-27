package org.skyve.impl.metadata.user;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.Locale;
import java.util.Map;
import java.util.TreeMap;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.view.container.form.FormLabelLayout;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.user.DocumentPermissionScope;
import org.skyve.metadata.user.User;

@ExtendWith(MockitoExtension.class)
class SuperUserTest {

	@Mock
	private Document document;

	@Test
	@SuppressWarnings("static-method")
	void defaultConstructorSetsLocale() {
		SuperUser user = new SuperUser();
		assertNotNull(user.getLocale());
	}

	@Test
	@SuppressWarnings("static-method")
	void isInRoleAlwaysReturnsTrue() {
		SuperUser user = new SuperUser();
		assertTrue(user.isInRole("admin", "BasicUser"));
	}

	@Test
	@SuppressWarnings("static-method")
	void canReadBeanAlwaysReturnsTrue() {
		SuperUser user = new SuperUser();
		assertTrue(user.canReadBean("bizId", "admin", "User", "acme", null, null));
	}

	@Test
	void canAccessDocumentAlwaysReturnsTrue() {
		SuperUser user = new SuperUser();
		assertTrue(user.canAccessDocument(document));
	}

	@Test
	void canCreateDocumentAlwaysReturnsTrue() {
		SuperUser user = new SuperUser();
		assertTrue(user.canCreateDocument(document));
	}

	@Test
	void canDeleteDocumentAlwaysReturnsTrue() {
		SuperUser user = new SuperUser();
		assertTrue(user.canDeleteDocument(document));
	}

	@Test
	void canReadDocumentAlwaysReturnsTrue() {
		SuperUser user = new SuperUser();
		assertTrue(user.canReadDocument(document));
	}

	@Test
	void canUpdateDocumentAlwaysReturnsTrue() {
		SuperUser user = new SuperUser();
		assertTrue(user.canUpdateDocument(document));
	}

	@Test
	void canExecuteActionAlwaysReturnsTrue() {
		SuperUser user = new SuperUser();
		assertTrue(user.canExecuteAction(document, "Edit"));
	}

	@Test
	@SuppressWarnings("static-method")
	void canFlagAlwaysReturnsTrue() {
		SuperUser user = new SuperUser();
		assertTrue(user.canFlag());
	}

	@Test
	@SuppressWarnings("static-method")
	void canTextSearchAlwaysReturnsTrue() {
		SuperUser user = new SuperUser();
		assertTrue(user.canTextSearch());
	}

	@Test
	@SuppressWarnings("static-method")
	void canSwitchModeAlwaysReturnsTrue() {
		SuperUser user = new SuperUser();
		assertTrue(user.canSwitchMode());
	}

	@Test
	@SuppressWarnings("static-method")
	void copyingConstructorCopiesContactId() {
		User mockUser = Mockito.mock(User.class);
		Mockito.when(mockUser.getContactId()).thenReturn("contact-123");
		Mockito.when(mockUser.getContactName()).thenReturn("Test User");
		Mockito.when(mockUser.getCustomerName()).thenReturn("test");
		Mockito.when(mockUser.getDataGroupId()).thenReturn(null);
		Mockito.when(mockUser.getHomeModuleName()).thenReturn("admin");
		Mockito.when(mockUser.getId()).thenReturn("user-id-1");
		Mockito.when(mockUser.getName()).thenReturn("testuser");
		Mockito.when(mockUser.getPasswordHash()).thenReturn("hash");
		Mockito.doReturn(Boolean.FALSE).when(mockUser).isPasswordChangeRequired();
		Mockito.when(mockUser.getLocale()).thenReturn(Locale.ENGLISH);
		Mockito.when(mockUser.getLanguageTag()).thenReturn("en");

		SuperUser superUser = new SuperUser(mockUser);

		assertThat(superUser.getContactId(), is("contact-123"));
		assertThat(superUser.getContactName(), is("Test User"));
		assertNotNull(superUser.getLocale());
	}

	@Test
	@SuppressWarnings("static-method")
	void copyingConstructorWithNullLocaleDefaultsToEnglish() {
		User mockUser = Mockito.mock(User.class);
		Mockito.when(mockUser.getLocale()).thenReturn(null);
		// Must stub all properties called by copying constructor
		Mockito.when(mockUser.getContactId()).thenReturn(null);
		Mockito.when(mockUser.getContactName()).thenReturn(null);
		Mockito.when(mockUser.getCustomerName()).thenReturn(null);
		Mockito.when(mockUser.getDataGroupId()).thenReturn(null);
		Mockito.when(mockUser.getHomeModuleName()).thenReturn(null);
		Mockito.when(mockUser.getId()).thenReturn(null);
		Mockito.when(mockUser.getName()).thenReturn(null);
		Mockito.when(mockUser.getPasswordHash()).thenReturn(null);
		Mockito.doReturn(Boolean.FALSE).when(mockUser).isPasswordChangeRequired();
		Mockito.when(mockUser.getLanguageTag()).thenReturn(null);

		SuperUser superUser = new SuperUser(mockUser);

		// locale defaults to ENGLISH when null
		assertThat(superUser.getLocale(), is(Locale.ENGLISH));
	}

	@Test
	@SuppressWarnings("static-method")
	void getScopeReturnsCustomerWhenNoPermissionsSet() {
		SuperUser superUser = new SuperUser();
		// When super.getScope() returns 'none', SuperUser upgrades it to 'customer'
		DocumentPermissionScope scope = superUser.getScope("anyModule", "anyDocument");
		assertThat(scope, is(DocumentPermissionScope.customer));
	}

	@Test
	@SuppressWarnings("static-method")
	void getAccessibleModuleNamesReturnsSortedCustomerModuleNames() {
		CustomerImpl customer = Mockito.mock(CustomerImpl.class);
		Map<String, FormLabelLayout> modules = new TreeMap<>();
		modules.put("sales", FormLabelLayout.top);
		modules.put("admin", FormLabelLayout.side);
		Mockito.when(customer.getModuleEntries()).thenReturn(modules);

		SuperUser superUser = new TestSuperUser(customer);

		assertThat(superUser.getAccessibleModuleNames(), is(new java.util.TreeSet<>(modules.keySet())));
	}

	private static final class TestSuperUser extends SuperUser {
		private static final long serialVersionUID = 1L;

		private final Customer customer;

		private TestSuperUser(Customer customer) {
			this.customer = customer;
		}

		@Override
		public Customer getCustomer() {
			return customer;
		}
	}
}
