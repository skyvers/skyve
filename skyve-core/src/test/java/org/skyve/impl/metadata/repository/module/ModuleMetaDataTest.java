package org.skyve.impl.metadata.repository.module;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.module.JobMetaDataImpl;
import org.skyve.impl.metadata.view.container.form.FormLabelLayout;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.user.DocumentPermission;
import org.skyve.metadata.view.View.ViewType;

@SuppressWarnings("static-method")
class ModuleMetaDataTest {

	@Test
	void testSetAndGetTitle() {
		ModuleMetaData module = new ModuleMetaData();
		assertNull(module.getTitle());
		module.setTitle("Test Module");
		assertEquals("Test Module", module.getTitle());
	}

	@Test
	void testSetAndGetPrototype() {
		ModuleMetaData module = new ModuleMetaData();
		assertNull(module.getPrototype());
		module.setPrototype(Boolean.TRUE);
		assertEquals(Boolean.TRUE, module.getPrototype());
	}

	@Test
	void testSetAndGetHomeDocument() {
		ModuleMetaData module = new ModuleMetaData();
		assertNull(module.getHomeDocument());
		module.setHomeDocument("Dashboard");
		assertEquals("Dashboard", module.getHomeDocument());
	}

	@Test
	void testSetAndGetHomeRef() {
		ModuleMetaData module = new ModuleMetaData();
		assertNull(module.getHomeRef());
		module.setHomeRef(ViewType.edit);
		assertEquals(ViewType.edit, module.getHomeRef());
	}

	@Test
	void testGetJobsNotNull() {
		ModuleMetaData module = new ModuleMetaData();
		assertNotNull(module.getJobs());
		assertTrue(module.getJobs().isEmpty());
	}

	@Test
	void testGetDocumentsNotNull() {
		ModuleMetaData module = new ModuleMetaData();
		assertNotNull(module.getDocuments());
		assertTrue(module.getDocuments().isEmpty());
	}

	@Test
	void testGetQueriesNotNull() {
		ModuleMetaData module = new ModuleMetaData();
		assertNotNull(module.getQueries());
		assertTrue(module.getQueries().isEmpty());
	}

	@Test
	void testGetRolesNotNull() {
		ModuleMetaData module = new ModuleMetaData();
		assertNotNull(module.getRoles());
		assertTrue(module.getRoles().isEmpty());
	}

	@Test
	void testSetAndGetMenu() {
		ModuleMetaData module = new ModuleMetaData();
		assertNull(module.getMenu());
		MenuMetaData menu = new MenuMetaData();
		module.setMenu(menu);
		assertEquals(menu, module.getMenu());
	}

	@Test
	void testSetAndGetDocumentation() {
		ModuleMetaData module = new ModuleMetaData();
		assertNull(module.getDocumentation());
		module.setDocumentation("Test documentation");
		assertEquals("Test documentation", module.getDocumentation());
	}

	@Test
	void testSetAndGetLastModifiedMillis() {
		ModuleMetaData module = new ModuleMetaData();
		module.setLastModifiedMillis(12345L);
		assertEquals(12345L, module.getLastModifiedMillis());
	}

	@Test
	void testGetPropertiesNotNull() {
		ModuleMetaData module = new ModuleMetaData();
		assertNotNull(module.getProperties());
		assertTrue(module.getProperties().isEmpty());
	}

	@Test
	void testSetAndGetFormLabelLayout() {
		ModuleMetaData module = new ModuleMetaData();
		assertNull(module.getFormLabelLayout());
		// FormLabelLayout is an enum — pick any value
		module.setFormLabelLayout(FormLabelLayout.top);
		assertEquals(FormLabelLayout.top, module.getFormLabelLayout());
	}

	// ── convert() ───────────────────────────────────────────────────────────

	private static ModuleMetaData createMinimalModuleMetaData() {
		ModuleMetaData module = new ModuleMetaData();
		module.setName("testModule");
		module.setTitle("Test Module");
		module.setHomeDocument("TestDocument");
		module.setMenu(new MenuMetaData());

		ModuleDocumentMetaData doc = new ModuleDocumentMetaData();
		doc.setRef("TestDocument");
		module.getDocuments().add(doc);

		return module;
	}

	@Test
	void convertThrowsWhenNameIsNull() {
		ModuleMetaData module = createMinimalModuleMetaData();
		module.setName(null);
		assertThrows(MetaDataException.class, () -> module.convert("test"));
	}

	@Test
	void convertThrowsWhenTitleIsNull() {
		ModuleMetaData module = createMinimalModuleMetaData();
		module.setTitle(null);
		assertThrows(MetaDataException.class, () -> module.convert("test"));
	}

	@Test
	void convertThrowsWhenHomeDocumentIsNull() {
		ModuleMetaData module = createMinimalModuleMetaData();
		module.setHomeDocument(null);
		assertThrows(MetaDataException.class, () -> module.convert("test"));
	}

	@Test
	void convertThrowsWhenHomeDocumentNotInDocumentList() {
		ModuleMetaData module = new ModuleMetaData();
		module.setName("testModule");
		module.setTitle("Test Module");
		module.setHomeDocument("NonExistent");

		ModuleDocumentMetaData doc = new ModuleDocumentMetaData();
		doc.setRef("SomeOtherDocument");
		module.getDocuments().add(doc);

		assertThrows(MetaDataException.class, () -> module.convert("test"));
	}

	@Test
	void convertThrowsWhenDocumentRefIsNull() {
		ModuleMetaData module = new ModuleMetaData();
		module.setName("testModule");
		module.setTitle("Test Module");
		module.setHomeDocument("TestDocument");

		ModuleDocumentMetaData doc = new ModuleDocumentMetaData();
		// ref is null
		module.getDocuments().add(doc);

		assertThrows(MetaDataException.class, () -> module.convert("test"));
	}

	@Test
	void convertThrowsWhenDuplicateDocumentRef() {
		ModuleMetaData module = new ModuleMetaData();
		module.setName("testModule");
		module.setTitle("Test Module");
		module.setHomeDocument("TestDocument");

		ModuleDocumentMetaData doc1 = new ModuleDocumentMetaData();
		doc1.setRef("TestDocument");
		ModuleDocumentMetaData doc2 = new ModuleDocumentMetaData();
		doc2.setRef("TestDocument");
		module.getDocuments().add(doc1);
		module.getDocuments().add(doc2);

		assertThrows(MetaDataException.class, () -> module.convert("test"));
	}

	@Test
	void convertSucceedsForMinimalModule() {
		assertNotNull(createMinimalModuleMetaData().convert("test"));
	}

	@Test
	void convertThrowsWhenJobNameIsNull() {
		ModuleMetaData module = createMinimalModuleMetaData();
		JobMetaDataImpl job = new JobMetaDataImpl();
		// name is null
		job.setDisplayName("My Job");
		module.getJobs().add(job);
		assertThrows(MetaDataException.class, () -> module.convert("test"));
	}

	@Test
	void convertThrowsWhenJobDisplayNameIsNull() {
		ModuleMetaData module = createMinimalModuleMetaData();
		JobMetaDataImpl job = new JobMetaDataImpl();
		job.setName("myJob");
		// displayName is null
		module.getJobs().add(job);
		assertThrows(MetaDataException.class, () -> module.convert("test"));
	}

	@Test
	void convertThrowsWhenDuplicateJobName() {
		ModuleMetaData module = createMinimalModuleMetaData();
		JobMetaDataImpl job1 = new JobMetaDataImpl();
		job1.setName("myJob");
		job1.setDisplayName("My Job");
		JobMetaDataImpl job2 = new JobMetaDataImpl();
		job2.setName("myJob");
		job2.setDisplayName("My Job Again");
		module.getJobs().add(job1);
		module.getJobs().add(job2);
		assertThrows(MetaDataException.class, () -> module.convert("test"));
	}

	@Test
	void convertThrowsWhenRoleNameIsNull() {
		ModuleMetaData module = createMinimalModuleMetaData();
		ModuleRoleMetaData role = new ModuleRoleMetaData();
		// name is null
		module.getRoles().add(role);
		assertThrows(MetaDataException.class, () -> module.convert("test"));
	}

	@Test
	void convertThrowsWhenDuplicateRoleName() {
		ModuleMetaData module = createMinimalModuleMetaData();
		ModuleRoleMetaData role1 = new ModuleRoleMetaData();
		role1.setName("viewer");
		ModuleRoleMetaData role2 = new ModuleRoleMetaData();
		role2.setName("viewer");
		module.getRoles().add(role1);
		module.getRoles().add(role2);
		assertThrows(MetaDataException.class, () -> module.convert("test"));
	}

	@Test
	void convertThrowsWhenRoleNameIsDocumentName() {
		ModuleMetaData module = createMinimalModuleMetaData();
		ModuleRoleMetaData role = new ModuleRoleMetaData();
		role.setName("TestDocument"); // clashes with the document ref
		module.getRoles().add(role);
		assertThrows(MetaDataException.class, () -> module.convert("test"));
	}

	@Test
	void convertThrowsWhenJobNameIsDocumentName() {
		ModuleMetaData module = createMinimalModuleMetaData();
		JobMetaDataImpl job = new JobMetaDataImpl();
		job.setName("TestDocument"); // clashes with the document ref
		job.setDisplayName("Test Document Job");
		module.getJobs().add(job);
		assertThrows(MetaDataException.class, () -> module.convert("test"));
	}

	@Test
	void convertThrowsWhenDocumentPrivilegeDocumentNameIsNull() {
		ModuleMetaData module = createMinimalModuleMetaData();
		ModuleRoleMetaData role = new ModuleRoleMetaData();
		role.setName("viewer");
		DocumentPrivilegeMetaData privilege = new DocumentPrivilegeMetaData();
		// documentName is null
		privilege.setPermission(DocumentPermission.CRUDG);
		role.getPrivileges().add(privilege);
		module.getRoles().add(role);
		assertThrows(MetaDataException.class, () -> module.convert("test"));
	}

	@Test
	void convertThrowsWhenDuplicateDocumentPrivilege() {
		ModuleMetaData module = createMinimalModuleMetaData();
		ModuleRoleMetaData role = new ModuleRoleMetaData();
		role.setName("viewer");
		DocumentPrivilegeMetaData privilege1 = new DocumentPrivilegeMetaData();
		privilege1.setDocumentName("TestDocument");
		privilege1.setPermission(DocumentPermission.CRUDG);
		DocumentPrivilegeMetaData privilege2 = new DocumentPrivilegeMetaData();
		privilege2.setDocumentName("TestDocument"); // duplicate
		privilege2.setPermission(DocumentPermission._R__G);
		role.getPrivileges().add(privilege1);
		role.getPrivileges().add(privilege2);
		module.getRoles().add(role);
		assertThrows(MetaDataException.class, () -> module.convert("test"));
	}

	@Test
	void convertThrowsWhenDocumentPrivilegeDocumentNotInModule() {
		ModuleMetaData module = createMinimalModuleMetaData();
		ModuleRoleMetaData role = new ModuleRoleMetaData();
		role.setName("viewer");
		DocumentPrivilegeMetaData privilege = new DocumentPrivilegeMetaData();
		privilege.setDocumentName("NonExistentDocument");
		privilege.setPermission(DocumentPermission.CRUDG);
		role.getPrivileges().add(privilege);
		module.getRoles().add(role);
		assertThrows(MetaDataException.class, () -> module.convert("test"));
	}

	@Test
	void convertThrowsWhenDocumentPrivilegePermissionIsNull() {
		ModuleMetaData module = createMinimalModuleMetaData();
		ModuleRoleMetaData role = new ModuleRoleMetaData();
		role.setName("viewer");
		DocumentPrivilegeMetaData privilege = new DocumentPrivilegeMetaData();
		privilege.setDocumentName("TestDocument");
		// permission is null
		role.getPrivileges().add(privilege);
		module.getRoles().add(role);
		assertThrows(MetaDataException.class, () -> module.convert("test"));
	}

	@Test
	void convertThrowsWhenActionPrivilegeNameIsNull() {
		ModuleMetaData module = createMinimalModuleMetaData();
		ModuleRoleMetaData role = new ModuleRoleMetaData();
		role.setName("viewer");
		DocumentPrivilegeMetaData privilege = new DocumentPrivilegeMetaData();
		privilege.setDocumentName("TestDocument");
		privilege.setPermission(DocumentPermission.CRUDG);
		ActionPrivilegeMetaData action = new ActionPrivilegeMetaData();
		// actionName is null
		privilege.getActions().add(action);
		role.getPrivileges().add(privilege);
		module.getRoles().add(role);
		assertThrows(MetaDataException.class, () -> module.convert("test"));
	}

	@Test
	void convertSucceedsWithRoleAndDocumentPrivilege() {
		ModuleMetaData module = createMinimalModuleMetaData();
		ModuleRoleMetaData role = new ModuleRoleMetaData();
		role.setName("viewer");
		DocumentPrivilegeMetaData privilege = new DocumentPrivilegeMetaData();
		privilege.setDocumentName("TestDocument");
		privilege.setPermission(DocumentPermission._R__G);
		role.getPrivileges().add(privilege);
		module.getRoles().add(role);
		assertNotNull(module.convert("test"));
	}

	@Test
	void convertSucceedsWithDocumentReferencedFromAnotherModule() {
		ModuleMetaData module = createMinimalModuleMetaData();
		// Add a cross-module document reference
		ModuleDocumentMetaData crossModuleDoc = new ModuleDocumentMetaData();
		crossModuleDoc.setRef("CrossDoc");
		crossModuleDoc.setModuleRef("otherModule");
		module.getDocuments().add(crossModuleDoc);
		assertNotNull(module.convert("test"));
	}
}
