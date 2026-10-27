package org.skyve.impl.metadata.user;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.Locale;
import java.util.Map;
import java.util.TreeMap;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
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
		User mockUser = mock(User.class);
		when(mockUser.getContactId()).thenReturn("contact-123");
		when(mockUser.getContactName()).thenReturn("Test User");
		when(mockUser.getCustomerName()).thenReturn("test");
		when(mockUser.getDataGroupId()).thenReturn(null);
		when(mockUser.getHomeModuleName()).thenReturn("admin");
		when(mockUser.getId()).thenReturn("user-id-1");
		when(mockUser.getName()).thenReturn("testuser");
		when(mockUser.getPasswordHash()).thenReturn("hash");
		doReturn(Boolean.FALSE).when(mockUser).isPasswordChangeRequired();
		when(mockUser.getLocale()).thenReturn(Locale.ENGLISH);
		when(mockUser.getLanguageTag()).thenReturn("en");

		SuperUser superUser = new SuperUser(mockUser);

		assertThat(superUser.getContactId(), is("contact-123"));
		assertThat(superUser.getContactName(), is("Test User"));
		assertNotNull(superUser.getLocale());
	}

	@Test
	@SuppressWarnings("static-method")
	void copyingConstructorWithNullLocaleDefaultsToEnglish() {
		User mockUser = mock(User.class);
		when(mockUser.getLocale()).thenReturn(null);
		// Must stub all properties called by copying constructor
		when(mockUser.getContactId()).thenReturn(null);
		when(mockUser.getContactName()).thenReturn(null);
		when(mockUser.getCustomerName()).thenReturn(null);
		when(mockUser.getDataGroupId()).thenReturn(null);
		when(mockUser.getHomeModuleName()).thenReturn(null);
		when(mockUser.getId()).thenReturn(null);
		when(mockUser.getName()).thenReturn(null);
		when(mockUser.getPasswordHash()).thenReturn(null);
		doReturn(Boolean.FALSE).when(mockUser).isPasswordChangeRequired();
		when(mockUser.getLanguageTag()).thenReturn(null);

		SuperUser superUser = new SuperUser(mockUser);

		// locale defaults to ENGLISH when null
		assertThat(superUser.getLocale(), is(Locale.ENGLISH));
	}

	@Test
	@SuppressWarnings("static-method")
	void copyingConstructorCopiesLegacyId() {
		User mockUser = mock(User.class);
		when(mockUser.getLegacyId()).thenReturn("BHM-42");

		SuperUser superUser = new SuperUser(mockUser);

		assertThat(superUser.getLegacyId(), is("BHM-42"));
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
		CustomerImpl customer = mock(CustomerImpl.class);
		Map<String, FormLabelLayout> modules = new TreeMap<>();
		modules.put("sales", FormLabelLayout.top);
		modules.put("admin", FormLabelLayout.side);
		when(customer.getModuleEntries()).thenReturn(modules);

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
