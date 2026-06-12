package org.skyve.impl.metadata.user;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.ArrayList;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.menu.Menu;
import org.skyve.metadata.module.menu.MenuItem;
import org.skyve.metadata.user.DocumentPermission;
import org.skyve.metadata.user.DocumentPermissionScope;

class TestUserImpl {
	@Test
	@SuppressWarnings("static-method")
	void testSetLanguageTag() {
		UserImpl user = new UserImpl();
		assertNull(user.getLocale());
		assertNull(user.getLanguageTag());
		user.setLanguageTag(Locale.FRENCH.getLanguage());
		assertEquals(Locale.FRENCH.getLanguage(), user.getLanguageTag());
		assertEquals(Locale.FRENCH, user.getLocale());
	}
	
	@Test
	@SuppressWarnings("static-method")
	void testSetWebLocaleWithoutLanguageTag() {
		UserImpl user = new UserImpl();
		assertNull(user.getLocale());
		assertNull(user.getLanguageTag());
		user.setWebLocale(Locale.CHINESE);
		assertNull(user.getLanguageTag());
		assertEquals(Locale.CHINESE, user.getLocale());
	}
	
	@Test
	@SuppressWarnings("static-method")
	void testSetWebLocaleWithLanguageTag() {
		UserImpl user = new UserImpl();
		assertNull(user.getLocale());
		assertNull(user.getLanguageTag());
		user.setLanguageTag(Locale.FRENCH.getLanguage());
		assertEquals(Locale.FRENCH.getLanguage(), user.getLanguageTag());
		user.setWebLocale(Locale.CHINESE);
		assertEquals(Locale.FRENCH.getLanguage(), user.getLanguageTag());
		assertEquals(Locale.FRENCH, user.getLocale());
	}
	
	@Test
	@SuppressWarnings("static-method")
	void testLocaleSerialization() {
		UserImpl user = new UserImpl();
		assertNull(user.getLocale());
		assertNull(user.getLanguageTag());
		user = UtilImpl.cloneBySerialization(user);
		assertNull(user.getLocale());
		assertNull(user.getLanguageTag());
		user.setWebLocale(Locale.CHINESE);
		user = UtilImpl.cloneBySerialization(user);
		assertNull(user.getLocale());
		assertNull(user.getLanguageTag());
		user.setLanguageTag(Locale.FRENCH.getLanguage());
		user = UtilImpl.cloneBySerialization(user);
		assertEquals(Locale.FRENCH.getLanguage(), user.getLanguageTag());
		assertEquals(Locale.FRENCH, user.getLocale());
	}

	@Test
	@SuppressWarnings("static-method")
	void testSetAndGetId() {
		UserImpl user = new UserImpl();
		user.setId("test-id-123");
		assertEquals("test-id-123", user.getId());
	}

	@Test
	@SuppressWarnings("static-method")
	void testSetAndGetName() {
		UserImpl user = new UserImpl();
		assertNull(user.getName());
		user.setName("testuser");
		assertEquals("testuser", user.getName());
	}

	@Test
	@SuppressWarnings("static-method")
	void testSetAndGetPasswordHash() {
		UserImpl user = new UserImpl();
		assertNull(user.getPasswordHash());
		user.setPasswordHash("hashedpw");
		assertEquals("hashedpw", user.getPasswordHash());
	}

	@Test
	@SuppressWarnings("static-method")
	void testSetAndGetPasswordChangeRequired() {
		UserImpl user = new UserImpl();
		assertFalse(user.isPasswordChangeRequired());
		user.setPasswordChangeRequired(true);
		assertTrue(user.isPasswordChangeRequired());
	}

	@Test
	@SuppressWarnings("static-method")
	void testSetAndGetContactId() {
		UserImpl user = new UserImpl();
		assertNull(user.getContactId());
		user.setContactId("contact-123");
		assertEquals("contact-123", user.getContactId());
	}

	@Test
	@SuppressWarnings("static-method")
	void testSetAndGetContactName() {
		UserImpl user = new UserImpl();
		assertNull(user.getContactName());
		user.setContactName("John Doe");
		assertEquals("John Doe", user.getContactName());
	}

	@Test
	@SuppressWarnings("static-method")
	void testSetAndGetContactImageId() {
		UserImpl user = new UserImpl();
		assertNull(user.getContactImageId());
		user.setContactImageId("image-abc");
		assertEquals("image-abc", user.getContactImageId());
	}

	@Test
	@SuppressWarnings("static-method")
	void testSetAndGetCustomerName() {
		UserImpl user = new UserImpl();
		assertNull(user.getCustomerName());
		user.setCustomerName("acme");
		assertEquals("acme", user.getCustomerName());
	}

	@Test
	@SuppressWarnings("static-method")
	void testSetAndGetDataGroupId() {
		UserImpl user = new UserImpl();
		assertNull(user.getDataGroupId());
		user.setDataGroupId("group-1");
		assertEquals("group-1", user.getDataGroupId());
	}

	@Test
	@SuppressWarnings("static-method")
	void testSetAndGetHomeModuleName() {
		UserImpl user = new UserImpl();
		assertNull(user.getHomeModuleName());
		user.setHomeModuleName("admin");
		assertEquals("admin", user.getHomeModuleName());
	}

	@Test
	@SuppressWarnings("static-method")
	void testGetAttributesReturnsEmptyMapByDefault() {
		UserImpl user = new UserImpl();
		Map<String, Object> attributes = user.getAttributes();
		assertNotNull(attributes);
		assertTrue(attributes.isEmpty());
	}

	@Test
	@SuppressWarnings("static-method")
	void testSetSessionIdAndGetSessionId() {
		UserImpl user = new UserImpl();
		user.setSessionId("session-xyz");
		assertEquals("session-xyz", user.getSessionId());
	}

	@Test
	@SuppressWarnings("static-method")
	void testGetContactAvatarInitialsWithFirstAndLastName() {
		UserImpl user = new UserImpl();
		user.setContactName("John Doe");
		String initials = user.getContactAvatarInitials();
		assertNotNull(initials);
		assertEquals("JD", initials);
	}

	@Test
	@SuppressWarnings("static-method")
	void testGetContactAvatarInitialsWithSingleName() {
		UserImpl user = new UserImpl();
		user.setContactName("John");
		String initials = user.getContactAvatarInitials();
		assertNotNull(initials);
		assertEquals("J", initials);
	}

	@Test
	@SuppressWarnings("static-method")
	void testGetContactAvatarInitialsWithNullName() {
		UserImpl user = new UserImpl();
		String initials = user.getContactAvatarInitials();
		assertNotNull(initials);
		assertEquals("??", initials);
	}

	@Test
	@SuppressWarnings("static-method")
	void testGetContactImageUrlNullWhenNoImageId() {
		UserImpl user = new UserImpl();
		assertNull(user.getContactImageUrl(64, 64));
	}

	@Test
	@SuppressWarnings("static-method")
	void testGetContactImageUrlContainsImageId() {
		UserImpl user = new UserImpl();
		user.setContactImageId("img-123");
		String url = user.getContactImageUrl(100, 200);
		assertNotNull(url);
		assertTrue(url.contains("img-123"));
		assertTrue(url.contains("100"));
		assertTrue(url.contains("200"));
	}

	@Test
	@SuppressWarnings("static-method")
	void testGetFullyQualifiedDocumentNamesEmptyByDefault() {
		UserImpl user = new UserImpl();
		Set<String> names = user.getFullyQualifiedDocumentNames();
		assertNotNull(names);
		assertTrue(names.isEmpty());
	}

	@Test
	@SuppressWarnings("static-method")
	void testGetModuleMenuNullWhenNotSet() {
		UserImpl user = new UserImpl();
		assertNull(user.getModuleMenu("admin"));
	}

	@Test
	@SuppressWarnings("static-method")
	void testPutAndGetModuleMenu() {
		UserImpl user = new UserImpl();
		Module module = Mockito.mock(Module.class);
		Mockito.when(module.getName()).thenReturn("admin");
		Menu menu = Mockito.mock(Menu.class);
		Mockito.when(menu.getItems()).thenReturn(new ArrayList<>());
		user.putModuleMenu(module, menu);
		assertEquals(menu, user.getModuleMenu("admin"));
	}

	@Test
	@SuppressWarnings("static-method")
	void testClearModuleMenus() {
		UserImpl user = new UserImpl();
		Module module = Mockito.mock(Module.class);
		Mockito.when(module.getName()).thenReturn("admin");
		Menu menu = Mockito.mock(Menu.class);
		user.putModuleMenu(module, menu);
		user.clearModuleMenus();
		assertNull(user.getModuleMenu("admin"));
	}

	@Test
	@SuppressWarnings("static-method")
	void testGetAccessibleModuleNamesEmptyWhenNoMenus() {
		UserImpl user = new UserImpl();
		Set<String> accessible = user.getAccessibleModuleNames();
		assertNotNull(accessible);
		assertTrue(accessible.isEmpty());
	}

	@Test
	@SuppressWarnings("static-method")
	void testGetAccessibleModuleNamesIncludesModulesWithMenuItems() {
		UserImpl user = new UserImpl();
		Module module = Mockito.mock(Module.class);
		Mockito.when(module.getName()).thenReturn("admin");
		Menu menu = Mockito.mock(Menu.class);
		MenuItem item = Mockito.mock(MenuItem.class);
		java.util.List<MenuItem> items = new ArrayList<>();
		items.add(item);
		Mockito.when(menu.getItems()).thenReturn(items);
		user.putModuleMenu(module, menu);
		Set<String> accessible = user.getAccessibleModuleNames();
		assertTrue(accessible.contains("admin"));
	}

	@Test
	@SuppressWarnings("static-method")
	void testIsInRoleReturnsFalseWhenNoRoles() {
		UserImpl user = new UserImpl();
		assertFalse(user.isInRole("admin", "SomeRole"));
	}

	// --- addRole / putDocumentPermission ---

	private static RoleImpl buildRole(Module module, String roleName, String documentName, DocumentPermission permission) {
		DocumentPrivilege priv = new DocumentPrivilege();
		priv.setName(documentName);
		priv.setPermission(permission);

		RoleImpl role = new RoleImpl();
		role.setOwningModule(module);
		role.setName(roleName);
		role.getPrivileges().add(priv);
		return role;
	}

	@Test
	@SuppressWarnings("static-method")
	void testAddRoleAddsRoleName() {
		Module module = Mockito.mock(Module.class);
		Mockito.when(module.getName()).thenReturn("admin");

		RoleImpl role = buildRole(module, "BasicUser", "User", DocumentPermission._R__G);

		UserImpl user = new UserImpl();
		user.addRole(role);
		assertTrue(user.isInRole("admin", "BasicUser"));
	}

	@Test
	@SuppressWarnings("static-method")
	void testAddRoleIdempotent() {
		Module module = Mockito.mock(Module.class);
		Mockito.when(module.getName()).thenReturn("admin");

		RoleImpl role = buildRole(module, "BasicUser", "User", DocumentPermission._R__G);

		UserImpl user = new UserImpl();
		user.addRole(role);
		user.addRole(role); // second add should be a no-op
		// scope should still be global (not doubled)
		assertEquals(DocumentPermissionScope.global, user.getScope("admin", "User"));
	}

	@Test
	@SuppressWarnings("static-method")
	void testGetScopeNoneWhenNoPermission() {
		UserImpl user = new UserImpl();
		assertEquals(DocumentPermissionScope.none, user.getScope("admin", "User"));
	}

	@Test
	@SuppressWarnings("static-method")
	void testGetScopeReflectsAddedRole() {
		Module module = Mockito.mock(Module.class);
		Mockito.when(module.getName()).thenReturn("myModule");

		RoleImpl role = buildRole(module, "Editor", "MyDoc", DocumentPermission.CRUDC);
		UserImpl user = new UserImpl();
		user.addRole(role);
		assertEquals(DocumentPermissionScope.customer, user.getScope("myModule", "MyDoc"));
	}

	@Test
	@SuppressWarnings("static-method")
	void testCanAccessDocumentAfterAddRole() {
		Module module = Mockito.mock(Module.class);
		Mockito.when(module.getName()).thenReturn("admin");

		RoleImpl role = buildRole(module, "Viewer", "Report", DocumentPermission._R__G);
		UserImpl user = new UserImpl();
		user.addRole(role);

		Document doc = Mockito.mock(Document.class);
		Mockito.when(doc.getOwningModuleName()).thenReturn("admin");
		Mockito.when(doc.getName()).thenReturn("Report");

		assertTrue(user.canAccessDocument(doc));
	}

	@Test
	@SuppressWarnings("static-method")
	void testCanAccessDocumentReturnsFalseWhenNoPermission() {
		UserImpl user = new UserImpl();

		Document doc = Mockito.mock(Document.class);
		Mockito.when(doc.getOwningModuleName()).thenReturn("admin");
		Mockito.when(doc.getName()).thenReturn("Report");

		assertFalse(user.canAccessDocument(doc));
	}

	@Test
	@SuppressWarnings("static-method")
	void testCanCreateDocumentWithCRUDG() {
		Module module = Mockito.mock(Module.class);
		Mockito.when(module.getName()).thenReturn("admin");

		RoleImpl role = buildRole(module, "Admin", "Config", DocumentPermission.CRUDG);
		UserImpl user = new UserImpl();
		user.addRole(role);

		Document doc = Mockito.mock(Document.class);
		Mockito.when(doc.getOwningModuleName()).thenReturn("admin");
		Mockito.when(doc.getName()).thenReturn("Config");

		assertTrue(user.canCreateDocument(doc));
	}

	@Test
	@SuppressWarnings("static-method")
	void testCanCreateDocumentReturnsFalseForReadOnly() {
		Module module = Mockito.mock(Module.class);
		Mockito.when(module.getName()).thenReturn("admin");

		RoleImpl role = buildRole(module, "ReadOnly", "Config", DocumentPermission._R__G);
		UserImpl user = new UserImpl();
		user.addRole(role);

		Document doc = Mockito.mock(Document.class);
		Mockito.when(doc.getOwningModuleName()).thenReturn("admin");
		Mockito.when(doc.getName()).thenReturn("Config");

		assertFalse(user.canCreateDocument(doc));
	}

	@Test
	@SuppressWarnings("static-method")
	void testCanReadDocument() {
		Module module = Mockito.mock(Module.class);
		Mockito.when(module.getName()).thenReturn("mod");

		RoleImpl role = buildRole(module, "Role1", "Doc1", DocumentPermission._R__U);
		UserImpl user = new UserImpl();
		user.addRole(role);

		Document doc = Mockito.mock(Document.class);
		Mockito.when(doc.getOwningModuleName()).thenReturn("mod");
		Mockito.when(doc.getName()).thenReturn("Doc1");

		assertTrue(user.canReadDocument(doc));
	}

	@Test
	@SuppressWarnings("static-method")
	void testCanUpdateDocument() {
		Module module = Mockito.mock(Module.class);
		Mockito.when(module.getName()).thenReturn("mod");

		RoleImpl role = buildRole(module, "Updater", "Doc1", DocumentPermission.CRU_G);
		UserImpl user = new UserImpl();
		user.addRole(role);

		Document doc = Mockito.mock(Document.class);
		Mockito.when(doc.getOwningModuleName()).thenReturn("mod");
		Mockito.when(doc.getName()).thenReturn("Doc1");

		assertTrue(user.canUpdateDocument(doc));
	}

	@Test
	@SuppressWarnings("static-method")
	void testCanDeleteDocument() {
		Module module = Mockito.mock(Module.class);
		Mockito.when(module.getName()).thenReturn("mod");

		RoleImpl role = buildRole(module, "Deleter", "Doc2", DocumentPermission.CRUDD);
		UserImpl user = new UserImpl();
		user.addRole(role);

		Document doc = Mockito.mock(Document.class);
		Mockito.when(doc.getOwningModuleName()).thenReturn("mod");
		Mockito.when(doc.getName()).thenReturn("Doc2");

		assertTrue(user.canDeleteDocument(doc));
	}

	@Test
	@SuppressWarnings("static-method")
	void testCanExecuteActionAfterActionPrivilege() {
		ActionPrivilege actionPriv = new ActionPrivilege();
		actionPriv.setDocumentName("Task");
		actionPriv.setName("Execute");

		Module module = Mockito.mock(Module.class);
		Mockito.when(module.getName()).thenReturn("tasks");

		RoleImpl role = new RoleImpl();
		role.setOwningModule(module);
		role.setName("Executor");
		role.getPrivileges().add(actionPriv);

		UserImpl user = new UserImpl();
		user.addRole(role);

		Document doc = Mockito.mock(Document.class);
		Mockito.when(doc.getOwningModuleName()).thenReturn("tasks");
		Mockito.when(doc.getName()).thenReturn("Task");

		assertTrue(user.canExecuteAction(doc, "Execute"));
		assertFalse(user.canExecuteAction(doc, "Other"));
	}

	@Test
	@SuppressWarnings("static-method")
	void testClearAllPermissionsAndMenus() {
		Module module = Mockito.mock(Module.class);
		Mockito.when(module.getName()).thenReturn("admin");

		RoleImpl role = buildRole(module, "Admin", "User", DocumentPermission.CRUDG);
		UserImpl user = new UserImpl();
		user.addRole(role);

		// sanity check
		assertTrue(user.isInRole("admin", "Admin"));

		user.clearAllPermissionsAndMenus();
		assertFalse(user.isInRole("admin", "Admin"));
		assertEquals(DocumentPermissionScope.none, user.getScope("admin", "User"));
	}

	@Test
	@SuppressWarnings("static-method")
	void testCanReadBeanGlobalScope() {
		Module module = Mockito.mock(Module.class);
		Mockito.when(module.getName()).thenReturn("admin");

		RoleImpl role = buildRole(module, "Admin", "User", DocumentPermission._R__G);
		UserImpl user = new UserImpl();
		user.setCustomerName("myCustomer");
		user.addRole(role);

		assertTrue(user.canReadBean("someId", "admin", "User", "anyCustomer", null, null));
	}

	@Test
	@SuppressWarnings("static-method")
	void testCanReadBeanCustomerScope() {
		Module module = Mockito.mock(Module.class);
		Mockito.when(module.getName()).thenReturn("admin");

		RoleImpl role = buildRole(module, "Admin", "User", DocumentPermission._R__C);
		UserImpl user = new UserImpl();
		user.setCustomerName("myCustomer");
		user.addRole(role);

		assertTrue(user.canReadBean("someId", "admin", "User", "myCustomer", null, null));
		assertFalse(user.canReadBean("someId", "admin", "User", "otherCustomer", null, null));
	}

	@Test
	@SuppressWarnings("static-method")
	void testCanReadBeanDataGroupScope() {
		Module module = Mockito.mock(Module.class);
		Mockito.when(module.getName()).thenReturn("admin");

		RoleImpl role = buildRole(module, "Admin", "User", DocumentPermission._R__D);
		UserImpl user = new UserImpl();
		user.setCustomerName("myCustomer");
		user.setDataGroupId("group1");
		user.addRole(role);

		// Same customer and data group — allowed
		assertTrue(user.canReadBean("someId", "admin", "User", "myCustomer", "group1", null));
		// Different customer — denied
		assertFalse(user.canReadBean("someId", "admin", "User", "otherCustomer", "group1", null));
		// Same customer, different data group — denied
		assertFalse(user.canReadBean("someId", "admin", "User", "myCustomer", "group2", null));
	}

	@Test
	@SuppressWarnings("static-method")
	void testCanReadBeanDataGroupScopeWithNullDataGroup() {
		Module module = Mockito.mock(Module.class);
		Mockito.when(module.getName()).thenReturn("admin");

		RoleImpl role = buildRole(module, "Admin", "User", DocumentPermission._R__D);
		UserImpl user = new UserImpl();
		user.setCustomerName("myCustomer");
		// dataGroupId is null on user — means user sees all groups in their customer
		user.addRole(role);

		assertTrue(user.canReadBean("someId", "admin", "User", "myCustomer", "anyGroup", null));
		assertFalse(user.canReadBean("someId", "admin", "User", "otherCustomer", "anyGroup", null));
	}

	@Test
	@SuppressWarnings("static-method")
	void testCanReadBeanUserScope() {
		Module module = Mockito.mock(Module.class);
		Mockito.when(module.getName()).thenReturn("admin");

		RoleImpl role = buildRole(module, "Admin", "User", DocumentPermission._R__U);
		UserImpl user = new UserImpl();
		user.setCustomerName("myCustomer");
		user.setDataGroupId("group1");
		user.setId("userId1");
		user.addRole(role);

		// Same customer, group and user — allowed
		assertTrue(user.canReadBean("someId", "admin", "User", "myCustomer", "group1", "userId1"));
		// Different user — denied
		assertFalse(user.canReadBean("someId", "admin", "User", "myCustomer", "group1", "userId2"));
		// Different customer — denied
		assertFalse(user.canReadBean("someId", "admin", "User", "otherCustomer", "group1", "userId1"));
	}

	@Test
	@SuppressWarnings("static-method")
	void testCanReadBeanNoReadPermission() {
		Module module = Mockito.mock(Module.class);
		Mockito.when(module.getName()).thenReturn("admin");

		// _____: no permissions at all (no read)
		RoleImpl role = buildRole(module, "Admin", "User", DocumentPermission._____);
		UserImpl user = new UserImpl();
		user.setCustomerName("myCustomer");
		user.addRole(role);

		assertFalse(user.canReadBean("someId", "admin", "User", "myCustomer", null, null));
	}

	@Test
	@SuppressWarnings("static-method")
	void testToStringContainsNameAndCustomer() {
		UserImpl user = new UserImpl();
		user.setName("jdoe");
		user.setCustomerName("acme");
		user.setId("abc123");
		String s = user.toString();
		assertTrue(s.contains("jdoe"));
		assertTrue(s.contains("acme"));
		assertTrue(s.contains("abc123"));
	}

	@Test
	@SuppressWarnings("static-method")
	void testCanAccessDocumentReturnsTrueWhenPermissionSet() {
		Module module = Mockito.mock(Module.class);
		Mockito.when(module.getName()).thenReturn("myModule");

		RoleImpl role = buildRole(module, "MyRole", "MyDoc", DocumentPermission._R__G);
		UserImpl user = new UserImpl();
		user.addRole(role);

		Document doc = Mockito.mock(Document.class);
		Mockito.when(doc.getOwningModuleName()).thenReturn("myModule");
		Mockito.when(doc.getName()).thenReturn("MyDoc");

		assertTrue(user.canAccessDocument(doc));
	}

	@Test
	@SuppressWarnings("static-method")
	void testCanAccessDocumentReturnsFalseWhenNoPermissionSet() {
		UserImpl user = new UserImpl();

		Document doc = Mockito.mock(Document.class);
		Mockito.when(doc.getOwningModuleName()).thenReturn("myModule");
		Mockito.when(doc.getName()).thenReturn("MyDoc");

		assertFalse(user.canAccessDocument(doc));
	}

	@Test
	@SuppressWarnings("static-method")
	void testCanAccessContentReturnsFalseWhenRestricted() {
		ExposedUserImpl user = new ExposedUserImpl();
		user.setCustomerName("myCustomer");
		user.setId("userId1");

		org.skyve.impl.metadata.repository.module.ContentRestriction restriction =
				new org.skyve.impl.metadata.repository.module.ContentRestriction();
		restriction.setDocumentName("Contact");
		restriction.setAttributeName("photo");
		user.callAddContentRestriction("myModule", restriction);

		assertFalse(user.canAccessContent("bizId", "myModule", "Contact", "myCustomer", null, "userId1", "photo"));
	}

	@Test
	@SuppressWarnings("static-method")
	void testCanAccessContentReturnsTrueWhenExplicitlyPermitted() {
		Module module = Mockito.mock(Module.class);
		Mockito.when(module.getName()).thenReturn("myModule");

		// Give the user permission to read the document
		RoleImpl role = buildRole(module, "MyRole", "Contact", DocumentPermission._R__G);
		ExposedUserImpl user = new ExposedUserImpl();
		user.setCustomerName("myCustomer");
		user.setId("userId1");
		user.addRole(role);

		org.skyve.impl.metadata.repository.module.ContentPermission permission =
				new org.skyve.impl.metadata.repository.module.ContentPermission();
		permission.setDocumentName("Contact");
		permission.setAttributeName("photo");
		user.callAddContentPermission("myModule", permission);

		assertTrue(user.canAccessContent("bizId", "myModule", "Contact", "myCustomer", null, "userId1", "photo"));
	}

	/** Helper subclass to expose protected methods for testing. */
	private static class ExposedUserImpl extends UserImpl {
		private static final long serialVersionUID = 1L;
		void callAddContentRestriction(String moduleName, org.skyve.impl.metadata.repository.module.ContentRestriction restriction) {
			addContentRestriction(moduleName, restriction);
		}
		void callAddContentPermission(String moduleName, org.skyve.impl.metadata.repository.module.ContentPermission permission) {
			addContentPermission(moduleName, permission);
		}
		void callPutDocumentPermission(String moduleName, String documentName, DocumentPermission permission) {
			putDocumentPermission(moduleName, documentName, permission);
		}
	}

	@Test
	@SuppressWarnings("static-method")
	void testCreateClientUserPopulatesAllFields() {
		ExposedUserImpl user = new ExposedUserImpl();
		user.setId("user-1");
		user.setName("jdoe");
		user.setContactId("contact-1");
		user.setContactName("John Doe");
		user.setCustomerName("myCustomer");
		user.setDataGroupId("dg-1");
		user.callPutDocumentPermission("myModule", "MyDoc", DocumentPermission.CRUDG);

		org.skyve.impl.metadata.user.ClientUserData clientUser = user.createClientUser();

		assertNotNull(clientUser);
		assertEquals("user-1", clientUser.getId());
		assertEquals("jdoe", clientUser.getName());
		assertEquals("contact-1", clientUser.getContactId());
		assertEquals("John Doe", clientUser.getContactName());
		assertEquals("myCustomer", clientUser.getCustomerName());
		assertEquals("dg-1", clientUser.getDataGroupId());

		Map<String, Map<String, Boolean>> permissions = clientUser.getDocumentPermissions();
		assertNotNull(permissions);
		assertTrue(permissions.containsKey("myModule.MyDoc"));
		Map<String, Boolean> docPerms = permissions.get("myModule.MyDoc");
		assertEquals(Boolean.TRUE, docPerms.get("create"));
		assertEquals(Boolean.TRUE, docPerms.get("read"));
		assertEquals(Boolean.TRUE, docPerms.get("update"));
		assertEquals(Boolean.TRUE, docPerms.get("delete"));
	}
}
