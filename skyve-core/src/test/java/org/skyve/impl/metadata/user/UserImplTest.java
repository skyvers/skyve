package org.skyve.impl.metadata.user;

import java.util.ArrayList;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

import org.junit.Assert;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;

import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.menu.Menu;
import org.skyve.metadata.module.menu.MenuItem;
import org.skyve.metadata.user.DocumentPermission;
import org.skyve.metadata.user.DocumentPermissionScope;

class UserImplTest {
	@Test
	@SuppressWarnings("static-method")
	void testSetLanguageTag() {
		UserImpl user = new UserImpl();
		Assert.assertNull(user.getLocale());
		Assert.assertNull(user.getLanguageTag());
		user.setLanguageTag(Locale.FRENCH.getLanguage());
		Assert.assertEquals(Locale.FRENCH.getLanguage(), user.getLanguageTag());
		Assert.assertEquals(Locale.FRENCH, user.getLocale());
	}
	
	@Test
	@SuppressWarnings("static-method")
	void testSetWebLocaleWithoutLanguageTag() {
		UserImpl user = new UserImpl();
		Assert.assertNull(user.getLocale());
		Assert.assertNull(user.getLanguageTag());
		user.setWebLocale(Locale.CHINESE);
		Assert.assertNull(user.getLanguageTag());
		Assert.assertEquals(Locale.CHINESE, user.getLocale());
	}
	
	@Test
	@SuppressWarnings("static-method")
	void testSetWebLocaleWithLanguageTag() {
		UserImpl user = new UserImpl();
		Assert.assertNull(user.getLocale());
		Assert.assertNull(user.getLanguageTag());
		user.setLanguageTag(Locale.FRENCH.getLanguage());
		Assert.assertEquals(Locale.FRENCH.getLanguage(), user.getLanguageTag());
		user.setWebLocale(Locale.CHINESE);
		Assert.assertEquals(Locale.FRENCH.getLanguage(), user.getLanguageTag());
		Assert.assertEquals(Locale.FRENCH, user.getLocale());
	}
	
	@Test
	@SuppressWarnings("static-method")
	void testLocaleSerialization() {
		UserImpl user = new UserImpl();
		Assert.assertNull(user.getLocale());
		Assert.assertNull(user.getLanguageTag());
		user = UtilImpl.cloneBySerialization(user);
		Assert.assertNull(user.getLocale());
		Assert.assertNull(user.getLanguageTag());
		user.setWebLocale(Locale.CHINESE);
		user = UtilImpl.cloneBySerialization(user);
		Assert.assertNull(user.getLocale());
		Assert.assertNull(user.getLanguageTag());
		user.setLanguageTag(Locale.FRENCH.getLanguage());
		user = UtilImpl.cloneBySerialization(user);
		Assert.assertEquals(Locale.FRENCH.getLanguage(), user.getLanguageTag());
		Assert.assertEquals(Locale.FRENCH, user.getLocale());
	}

	@Test
	@SuppressWarnings("static-method")
	void testSetAndGetId() {
		UserImpl user = new UserImpl();
		user.setId("test-id-123");
		Assert.assertEquals("test-id-123", user.getId());
	}

	@Test
	@SuppressWarnings("static-method")
	void testSetAndGetName() {
		UserImpl user = new UserImpl();
		Assert.assertNull(user.getName());
		user.setName("testuser");
		Assert.assertEquals("testuser", user.getName());
	}

	@Test
	@SuppressWarnings("static-method")
	void testSetAndGetPasswordHash() {
		UserImpl user = new UserImpl();
		Assert.assertNull(user.getPasswordHash());
		user.setPasswordHash("hashedpw");
		Assert.assertEquals("hashedpw", user.getPasswordHash());
	}

	@Test
	@SuppressWarnings("static-method")
	void testSetAndGetPasswordChangeRequired() {
		UserImpl user = new UserImpl();
		Assert.assertFalse(user.isPasswordChangeRequired());
		user.setPasswordChangeRequired(true);
		Assert.assertTrue(user.isPasswordChangeRequired());
	}

	@Test
	@SuppressWarnings("static-method")
	void testSetAndGetContactId() {
		UserImpl user = new UserImpl();
		Assert.assertNull(user.getContactId());
		user.setContactId("contact-123");
		Assert.assertEquals("contact-123", user.getContactId());
	}

	@Test
	@SuppressWarnings("static-method")
	void testSetAndGetContactName() {
		UserImpl user = new UserImpl();
		Assert.assertNull(user.getContactName());
		user.setContactName("John Doe");
		Assert.assertEquals("John Doe", user.getContactName());
	}

	@Test
	@SuppressWarnings("static-method")
	void testSetAndGetContactImageId() {
		UserImpl user = new UserImpl();
		Assert.assertNull(user.getContactImageId());
		user.setContactImageId("image-abc");
		Assert.assertEquals("image-abc", user.getContactImageId());
	}

	@Test
	@SuppressWarnings("static-method")
	void testSetAndGetCustomerName() {
		UserImpl user = new UserImpl();
		Assert.assertNull(user.getCustomerName());
		user.setCustomerName("acme");
		Assert.assertEquals("acme", user.getCustomerName());
	}

	@Test
	@SuppressWarnings("static-method")
	void testSetAndGetDataGroupId() {
		UserImpl user = new UserImpl();
		Assert.assertNull(user.getDataGroupId());
		user.setDataGroupId("group-1");
		Assert.assertEquals("group-1", user.getDataGroupId());
	}

	@Test
	@SuppressWarnings("static-method")
	void testSetAndGetHomeModuleName() {
		UserImpl user = new UserImpl();
		Assert.assertNull(user.getHomeModuleName());
		user.setHomeModuleName("admin");
		Assert.assertEquals("admin", user.getHomeModuleName());
	}

	@Test
	@SuppressWarnings("static-method")
	void testGetAttributesReturnsEmptyMapByDefault() {
		UserImpl user = new UserImpl();
		Map<String, Object> attributes = user.getAttributes();
		Assert.assertNotNull(attributes);
		Assert.assertTrue(attributes.isEmpty());
	}

	@Test
	@SuppressWarnings("static-method")
	void testSetSessionIdAndGetSessionId() {
		UserImpl user = new UserImpl();
		user.setSessionId("session-xyz");
		Assert.assertEquals("session-xyz", user.getSessionId());
	}

	@Test
	@SuppressWarnings("static-method")
	void testGetContactAvatarInitialsWithFirstAndLastName() {
		UserImpl user = new UserImpl();
		user.setContactName("John Doe");
		String initials = user.getContactAvatarInitials();
		Assert.assertNotNull(initials);
		Assert.assertEquals("JD", initials);
	}

	@Test
	@SuppressWarnings("static-method")
	void testGetContactAvatarInitialsWithSingleName() {
		UserImpl user = new UserImpl();
		user.setContactName("John");
		String initials = user.getContactAvatarInitials();
		Assert.assertNotNull(initials);
		Assert.assertEquals("J", initials);
	}

	@Test
	@SuppressWarnings("static-method")
	void testGetContactAvatarInitialsWithNullName() {
		UserImpl user = new UserImpl();
		String initials = user.getContactAvatarInitials();
		Assert.assertNotNull(initials);
		Assert.assertEquals("??", initials);
	}

	@Test
	@SuppressWarnings("static-method")
	void testGetContactImageUrlNullWhenNoImageId() {
		UserImpl user = new UserImpl();
		Assert.assertNull(user.getContactImageUrl(64, 64));
	}

	@Test
	@SuppressWarnings("static-method")
	void testGetContactImageUrlContainsImageId() {
		UserImpl user = new UserImpl();
		user.setContactImageId("img-123");
		String url = user.getContactImageUrl(100, 200);
		Assert.assertNotNull(url);
		Assert.assertTrue(url.contains("img-123"));
		Assert.assertTrue(url.contains("100"));
		Assert.assertTrue(url.contains("200"));
	}

	@Test
	@SuppressWarnings("static-method")
	void testGetFullyQualifiedDocumentNamesEmptyByDefault() {
		UserImpl user = new UserImpl();
		Set<String> names = user.getFullyQualifiedDocumentNames();
		Assert.assertNotNull(names);
		Assert.assertTrue(names.isEmpty());
	}

	@Test
	@SuppressWarnings("static-method")
	void testGetModuleMenuNullWhenNotSet() {
		UserImpl user = new UserImpl();
		Assert.assertNull(user.getModuleMenu("admin"));
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
		Assert.assertEquals(menu, user.getModuleMenu("admin"));
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
		Assert.assertNull(user.getModuleMenu("admin"));
	}

	@Test
	@SuppressWarnings("static-method")
	void testGetAccessibleModuleNamesEmptyWhenNoMenus() {
		UserImpl user = new UserImpl();
		Set<String> accessible = user.getAccessibleModuleNames();
		Assert.assertNotNull(accessible);
		Assert.assertTrue(accessible.isEmpty());
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
		Assert.assertTrue(accessible.contains("admin"));
	}

	@Test
	@SuppressWarnings("static-method")
	void testIsInRoleReturnsFalseWhenNoRoles() {
		UserImpl user = new UserImpl();
		Assert.assertFalse(user.isInRole("admin", "SomeRole"));
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
		Assert.assertTrue(user.isInRole("admin", "BasicUser"));
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
		Assert.assertEquals(DocumentPermissionScope.global, user.getScope("admin", "User"));
	}

	@Test
	@SuppressWarnings("static-method")
	void testGetScopeNoneWhenNoPermission() {
		UserImpl user = new UserImpl();
		Assert.assertEquals(DocumentPermissionScope.none, user.getScope("admin", "User"));
	}

	@Test
	@SuppressWarnings("static-method")
	void testGetScopeReflectsAddedRole() {
		Module module = Mockito.mock(Module.class);
		Mockito.when(module.getName()).thenReturn("myModule");

		RoleImpl role = buildRole(module, "Editor", "MyDoc", DocumentPermission.CRUDC);
		UserImpl user = new UserImpl();
		user.addRole(role);
		Assert.assertEquals(DocumentPermissionScope.customer, user.getScope("myModule", "MyDoc"));
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

		Assert.assertTrue(user.canAccessDocument(doc));
	}

	@Test
	@SuppressWarnings("static-method")
	void testCanAccessDocumentReturnsFalseWhenNoPermission() {
		UserImpl user = new UserImpl();

		Document doc = Mockito.mock(Document.class);
		Mockito.when(doc.getOwningModuleName()).thenReturn("admin");
		Mockito.when(doc.getName()).thenReturn("Report");

		Assert.assertFalse(user.canAccessDocument(doc));
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

		Assert.assertTrue(user.canCreateDocument(doc));
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

		Assert.assertFalse(user.canCreateDocument(doc));
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

		Assert.assertTrue(user.canReadDocument(doc));
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

		Assert.assertTrue(user.canUpdateDocument(doc));
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

		Assert.assertTrue(user.canDeleteDocument(doc));
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

		Assert.assertTrue(user.canExecuteAction(doc, "Execute"));
		Assert.assertFalse(user.canExecuteAction(doc, "Other"));
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
		Assert.assertTrue(user.isInRole("admin", "Admin"));

		user.clearAllPermissionsAndMenus();
		Assert.assertFalse(user.isInRole("admin", "Admin"));
		Assert.assertEquals(DocumentPermissionScope.none, user.getScope("admin", "User"));
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

		Assert.assertTrue(user.canReadBean("someId", "admin", "User", "anyCustomer", null, null));
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

		Assert.assertTrue(user.canReadBean("someId", "admin", "User", "myCustomer", null, null));
		Assert.assertFalse(user.canReadBean("someId", "admin", "User", "otherCustomer", null, null));
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
		Assert.assertTrue(user.canReadBean("someId", "admin", "User", "myCustomer", "group1", null));
		// Different customer — denied
		Assert.assertFalse(user.canReadBean("someId", "admin", "User", "otherCustomer", "group1", null));
		// Same customer, different data group — denied
		Assert.assertFalse(user.canReadBean("someId", "admin", "User", "myCustomer", "group2", null));
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

		Assert.assertTrue(user.canReadBean("someId", "admin", "User", "myCustomer", "anyGroup", null));
		Assert.assertFalse(user.canReadBean("someId", "admin", "User", "otherCustomer", "anyGroup", null));
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
		Assert.assertTrue(user.canReadBean("someId", "admin", "User", "myCustomer", "group1", "userId1"));
		// Different user — denied
		Assert.assertFalse(user.canReadBean("someId", "admin", "User", "myCustomer", "group1", "userId2"));
		// Different customer — denied
		Assert.assertFalse(user.canReadBean("someId", "admin", "User", "otherCustomer", "group1", "userId1"));
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

		Assert.assertFalse(user.canReadBean("someId", "admin", "User", "myCustomer", null, null));
	}

	@Test
	@SuppressWarnings("static-method")
	void testToStringContainsNameAndCustomer() {
		UserImpl user = new UserImpl();
		user.setName("jdoe");
		user.setCustomerName("acme");
		user.setId("abc123");
		String s = user.toString();
		Assert.assertTrue(s.contains("jdoe"));
		Assert.assertTrue(s.contains("acme"));
		Assert.assertTrue(s.contains("abc123"));
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

		Assert.assertTrue(user.canAccessDocument(doc));
	}

	@Test
	@SuppressWarnings("static-method")
	void testCanAccessDocumentReturnsFalseWhenNoPermissionSet() {
		UserImpl user = new UserImpl();

		Document doc = Mockito.mock(Document.class);
		Mockito.when(doc.getOwningModuleName()).thenReturn("myModule");
		Mockito.when(doc.getName()).thenReturn("MyDoc");

		Assert.assertFalse(user.canAccessDocument(doc));
	}

	@Test
	@SuppressWarnings("static-method")
	void testCanAccessContentReturnsFalseWhenRestricted() {
		TestUserImpl user = new TestUserImpl();
		user.setCustomerName("myCustomer");
		user.setId("userId1");

		org.skyve.impl.metadata.repository.module.ContentRestriction restriction =
				new org.skyve.impl.metadata.repository.module.ContentRestriction();
		restriction.setDocumentName("Contact");
		restriction.setAttributeName("photo");
		user.callAddContentRestriction("myModule", restriction);

		Assert.assertFalse(user.canAccessContent("bizId", "myModule", "Contact", "myCustomer", null, "userId1", "photo"));
	}

	@Test
	@SuppressWarnings("static-method")
	void testCanAccessContentReturnsTrueWhenExplicitlyPermitted() {
		Module module = Mockito.mock(Module.class);
		Mockito.when(module.getName()).thenReturn("myModule");

		// Give the user permission to read the document
		RoleImpl role = buildRole(module, "MyRole", "Contact", DocumentPermission._R__G);
		TestUserImpl user = new TestUserImpl();
		user.setCustomerName("myCustomer");
		user.setId("userId1");
		user.addRole(role);

		org.skyve.impl.metadata.repository.module.ContentPermission permission =
				new org.skyve.impl.metadata.repository.module.ContentPermission();
		permission.setDocumentName("Contact");
		permission.setAttributeName("photo");
		user.callAddContentPermission("myModule", permission);

		Assert.assertTrue(user.canAccessContent("bizId", "myModule", "Contact", "myCustomer", null, "userId1", "photo"));
	}

	/** Helper subclass to expose protected methods for testing. */
	private static class TestUserImpl extends UserImpl {
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
		TestUserImpl user = new TestUserImpl();
		user.setId("user-1");
		user.setName("jdoe");
		user.setContactId("contact-1");
		user.setContactName("John Doe");
		user.setCustomerName("myCustomer");
		user.setDataGroupId("dg-1");
		user.callPutDocumentPermission("myModule", "MyDoc", DocumentPermission.CRUDG);

		org.skyve.impl.metadata.user.ClientUserData clientUser = user.createClientUser();

		Assert.assertNotNull(clientUser);
		Assert.assertEquals("user-1", clientUser.getId());
		Assert.assertEquals("jdoe", clientUser.getName());
		Assert.assertEquals("contact-1", clientUser.getContactId());
		Assert.assertEquals("John Doe", clientUser.getContactName());
		Assert.assertEquals("myCustomer", clientUser.getCustomerName());
		Assert.assertEquals("dg-1", clientUser.getDataGroupId());

		Map<String, Map<String, Boolean>> permissions = clientUser.getDocumentPermissions();
		Assert.assertNotNull(permissions);
		Assert.assertTrue(permissions.containsKey("myModule.MyDoc"));
		Map<String, Boolean> docPerms = permissions.get("myModule.MyDoc");
		Assert.assertEquals(Boolean.TRUE, docPerms.get("create"));
		Assert.assertEquals(Boolean.TRUE, docPerms.get("read"));
		Assert.assertEquals(Boolean.TRUE, docPerms.get("update"));
		Assert.assertEquals(Boolean.TRUE, docPerms.get("delete"));
	}

	// ---- isInRole DATA_ADMINISTRATOR_ROLE branch ----

	@Test
	@SuppressWarnings("static-method")
	void testIsInRoleDataAdminGrantedToSecurityAdminWithNullDataGroupId() {
		Module module = Mockito.mock(Module.class);
		Mockito.when(module.getName()).thenReturn("admin");

		RoleImpl role = new RoleImpl();
		role.setOwningModule(module);
		role.setName("SecurityAdministrator");

		UserImpl user = new UserImpl();
		// dataGroupId is null by default
		user.addRole(role);
		Assert.assertTrue(user.isInRole("admin", "DataAdministrator"));
	}

	@Test
	@SuppressWarnings("static-method")
	void testIsInRoleDataAdminNotGrantedToSecurityAdminWhenDataGroupIdSet() {
		Module module = Mockito.mock(Module.class);
		Mockito.when(module.getName()).thenReturn("admin");

		RoleImpl role = new RoleImpl();
		role.setOwningModule(module);
		role.setName("SecurityAdministrator");

		UserImpl user = new UserImpl();
		user.setDataGroupId("some-group");
		user.addRole(role);
		Assert.assertFalse(user.isInRole("admin", "DataAdministrator"));
	}

	// ---- addRole with ContentRestriction / ContentPermission ----

	@Test
	@SuppressWarnings("static-method")
	void testAddRoleWithContentRestrictionAddsRestrictionToUser() {
		Module module = Mockito.mock(Module.class);
		Mockito.when(module.getName()).thenReturn("admin");

		org.skyve.impl.metadata.repository.module.ContentRestriction restriction =
				new org.skyve.impl.metadata.repository.module.ContentRestriction();
		restriction.setDocumentName("User");
		restriction.setAttributeName("photo");

		RoleImpl role = new RoleImpl();
		role.setOwningModule(module);
		role.setName("Restricted");
		role.getContentRestrictions().add(restriction);

		UserImpl user = new UserImpl();
		user.addRole(role);
		// canAccessContent returns false when a restriction key matches
		Assert.assertFalse(user.canAccessContent("bizId", "admin", "User", "myCustomer", null, "userId1", "photo"));
	}

	@Test
	@SuppressWarnings("static-method")
	void testAddRoleWithContentPermissionAddsPermissionToUser() {
		Module module = Mockito.mock(Module.class);
		Mockito.when(module.getName()).thenReturn("admin");

		org.skyve.impl.metadata.repository.module.ContentPermission permission =
				new org.skyve.impl.metadata.repository.module.ContentPermission();
		permission.setDocumentName("User");
		permission.setAttributeName("photo");

		RoleImpl role = new RoleImpl();
		role.setOwningModule(module);
		role.setName("Permitted");
		role.getContentPermissions().add(permission);

		UserImpl user = new UserImpl();
		user.addRole(role);
		// canAccessContent returns true when a permission key matches
		Assert.assertTrue(user.canAccessContent("bizId", "admin", "User", "myCustomer", null, "userId1", "photo"));
	}

	@Test
	@SuppressWarnings("static-method")
	void testPutDocumentPermissionMergesWithExistingPermission() {
		TestUserImpl user = new TestUserImpl();
		// Put an initial permission
		user.callPutDocumentPermission("admin", "User", DocumentPermission._R__G);
		Assert.assertNotNull(user.getScope("admin", "User"));
		// Put a second permission for the same document – triggers the merge branch
		user.callPutDocumentPermission("admin", "User", DocumentPermission.CR__G);
		// After merging C____ with _R__G the user still has scope (merged permission)
		Assert.assertNotNull(user.getScope("admin", "User"));
	}
}
