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

	// ── Query section ────────────────────────────────────────────────────────

	private static SQLMetaData createSQLQuery(String name) {
		SQLMetaData q = new SQLMetaData();
		q.setName(name);
		q.setDescription("SQL query " + name);
		q.setQuery("SELECT 1");
		return q;
	}

	private static BizQLMetaData createBizQLQuery(String name) {
		BizQLMetaData q = new BizQLMetaData();
		q.setName(name);
		q.setDescription("BizQL query " + name);
		q.setQuery("SELECT bean FROM {testModule.TestDocument} AS bean");
		return q;
	}

	private static MetaDataQueryMetaData createMetaDataQuery(String name, String documentName) {
		MetaDataQueryMetaData q = new MetaDataQueryMetaData();
		q.setName(name);
		q.setDescription("Metadata query " + name);
		q.setDocumentName(documentName);
		return q;
	}

	@Test
	void convertSucceedsWithSQLQuery() {
		ModuleMetaData module = createMinimalModuleMetaData();
		module.getQueries().add(createSQLQuery("myQuery"));
		assertNotNull(module.convert("test"));
	}

	@Test
	void convertSucceedsWithBizQLQuery() {
		ModuleMetaData module = createMinimalModuleMetaData();
		module.getQueries().add(createBizQLQuery("myBizQL"));
		assertNotNull(module.convert("test"));
	}

	@Test
	void convertThrowsWhenQueryNameIsNull() {
		ModuleMetaData module = createMinimalModuleMetaData();
		SQLMetaData q = new SQLMetaData();
		q.setDescription("desc");
		q.setQuery("SELECT 1");
		module.getQueries().add(q);
		assertThrows(MetaDataException.class, () -> module.convert("test"));
	}

	@Test
	void convertThrowsWhenQueryDescriptionIsNull() {
		ModuleMetaData module = createMinimalModuleMetaData();
		SQLMetaData q = new SQLMetaData();
		q.setName("myQuery");
		q.setQuery("SELECT 1");
		module.getQueries().add(q);
		assertThrows(MetaDataException.class, () -> module.convert("test"));
	}

	@Test
	void convertThrowsWhenDuplicateQueryName() {
		ModuleMetaData module = createMinimalModuleMetaData();
		module.getQueries().add(createSQLQuery("myQuery"));
		module.getQueries().add(createBizQLQuery("myQuery"));
		assertThrows(MetaDataException.class, () -> module.convert("test"));
	}

	@Test
	void convertThrowsWhenQueryNameMatchesDocumentName() {
		ModuleMetaData module = createMinimalModuleMetaData();
		module.getQueries().add(createSQLQuery("TestDocument"));
		assertThrows(MetaDataException.class, () -> module.convert("test"));
	}

	@Test
	void convertThrowsWhenDefaultQueryNameDoesNotExist() {
		ModuleMetaData module = createMinimalModuleMetaData();
		module.getDocuments().get(0).setDefaultQueryName("nonExistentQuery");
		assertThrows(MetaDataException.class, () -> module.convert("test"));
	}

	@Test
	void convertSucceedsWhenDefaultQueryNameExists() {
		ModuleMetaData module = createMinimalModuleMetaData();
		module.getDocuments().get(0).setDefaultQueryName("myQuery");
		module.getQueries().add(createMetaDataQuery("myQuery", "TestDocument"));
		assertNotNull(module.convert("test"));
	}

	@Test
	void convertSucceedsWithMetaDataQueryAndProjectedColumn() {
		ModuleMetaData module = createMinimalModuleMetaData();
		MetaDataQueryMetaData q = createMetaDataQuery("myQuery", "TestDocument");
		MetaDataQueryProjectedColumnMetaData col = new MetaDataQueryProjectedColumnMetaData();
		col.setBinding("name");
		q.getColumns().add(col);
		module.getQueries().add(q);
		assertNotNull(module.convert("test"));
	}

	@Test
	void convertThrowsWhenProjectedColumnHasNeitherBindingNorExpression() {
		ModuleMetaData module = createMinimalModuleMetaData();
		MetaDataQueryMetaData q = createMetaDataQuery("myQuery", "TestDocument");
		MetaDataQueryProjectedColumnMetaData col = new MetaDataQueryProjectedColumnMetaData();
		q.getColumns().add(col);
		module.getQueries().add(q);
		assertThrows(MetaDataException.class, () -> module.convert("test"));
	}

	@Test
	void convertThrowsWhenProjectedColumnHasBothBindingAndExpression() {
		ModuleMetaData module = createMinimalModuleMetaData();
		MetaDataQueryMetaData q = createMetaDataQuery("myQuery", "TestDocument");
		MetaDataQueryProjectedColumnMetaData col = new MetaDataQueryProjectedColumnMetaData();
		col.setBinding("name");
		col.setExpression("bean.name");
		q.getColumns().add(col);
		module.getQueries().add(q);
		assertThrows(MetaDataException.class, () -> module.convert("test"));
	}

	@Test
	void convertThrowsWhenExpressionColumnNameIsNull() {
		ModuleMetaData module = createMinimalModuleMetaData();
		MetaDataQueryMetaData q = createMetaDataQuery("myQuery", "TestDocument");
		MetaDataQueryProjectedColumnMetaData col = new MetaDataQueryProjectedColumnMetaData();
		col.setExpression("bean.name");
		// name is null
		q.getColumns().add(col);
		module.getQueries().add(q);
		assertThrows(MetaDataException.class, () -> module.convert("test"));
	}

	@Test
	void convertThrowsWhenMetaDataQueryDocumentNameIsNull() {
		ModuleMetaData module = createMinimalModuleMetaData();
		MetaDataQueryMetaData q = new MetaDataQueryMetaData();
		q.setName("myQuery");
		q.setDescription("desc");
		// documentName is null
		module.getQueries().add(q);
		assertThrows(MetaDataException.class, () -> module.convert("test"));
	}

	@Test
	void convertThrowsWhenMetaDataQueryDocumentNotInModule() {
		ModuleMetaData module = createMinimalModuleMetaData();
		module.getQueries().add(createMetaDataQuery("myQuery", "NonExistentDoc"));
		assertThrows(MetaDataException.class, () -> module.convert("test"));
	}

	@Test
	void convertSucceedsWithQueryReference() {
		ModuleMetaData module = createMinimalModuleMetaData();
		module.getQueries().add(createMetaDataQuery("sourceQuery", "TestDocument"));
		MetaDataQueryReferenceMetaData ref = new MetaDataQueryReferenceMetaData();
		ref.setName("queryRef");
		ref.setRef("sourceQuery");
		module.getQueries().add(ref);
		assertNotNull(module.convert("test"));
	}

	@Test
	void convertSucceedsWithSQLQueryReference() {
		ModuleMetaData module = createMinimalModuleMetaData();
		module.getQueries().add(createSQLQuery("baseSQL"));
		SQLReferenceMetaData ref = new SQLReferenceMetaData();
		ref.setName("sqlRef");
		ref.setRef("baseSQL");
		module.getQueries().add(ref);
		assertNotNull(module.convert("test"));
	}

	@Test
	void convertSucceedsWithBizQLQueryReference() {
		ModuleMetaData module = createMinimalModuleMetaData();
		module.getQueries().add(createBizQLQuery("baseBizQL"));
		BizQLReferenceMetaData ref = new BizQLReferenceMetaData();
		ref.setName("bizqlRef");
		ref.setRef("baseBizQL");
		module.getQueries().add(ref);
		assertNotNull(module.convert("test"));
	}

	// ── Menu section ─────────────────────────────────────────────────────────

	/** Creates a module with one role named "viewer" for menu item tests. */
	private static ModuleMetaData createModuleWithRole() {
		ModuleMetaData module = createMinimalModuleMetaData();
		ModuleRoleMetaData role = new ModuleRoleMetaData();
		role.setName("viewer");
		module.getRoles().add(role);
		return module;
	}

	private static GrantedTo grantedTo(String roleName) {
		GrantedTo g = new GrantedTo();
		g.setRoleName(roleName);
		return g;
	}

	@Test
	void convertSucceedsWithEditMenuItem() {
		ModuleMetaData module = createModuleWithRole();
		EditItemMetaData item = new EditItemMetaData();
		item.setName("editTestDocument");
		item.setDocumentName("TestDocument");
		item.getRoles().add(grantedTo("viewer"));
		module.getMenu().getActions().add(item);
		assertNotNull(module.convert("test"));
	}

	@Test
	void convertSucceedsWithListMenuItemWithDocument() {
		ModuleMetaData module = createModuleWithRole();
		ListItemMetaData item = new ListItemMetaData();
		item.setName("listTestDocument");
		item.setDocumentName("TestDocument");
		item.getRoles().add(grantedTo("viewer"));
		module.getMenu().getActions().add(item);
		assertNotNull(module.convert("test"));
	}

	@Test
	void convertSucceedsWithListMenuItemWithQuery() {
		ModuleMetaData module = createModuleWithRole();
		module.getQueries().add(createMetaDataQuery("myQuery", "TestDocument"));
		ListItemMetaData item = new ListItemMetaData();
		item.setName("listByQuery");
		item.setQueryName("myQuery");
		item.getRoles().add(grantedTo("viewer"));
		module.getMenu().getActions().add(item);
		assertNotNull(module.convert("test"));
	}

	@Test
	void convertThrowsWhenMenuItemHasNoRoles() {
		ModuleMetaData module = createModuleWithRole();
		EditItemMetaData item = new EditItemMetaData();
		item.setName("editItem");
		item.setDocumentName("TestDocument");
		// no roles added
		module.getMenu().getActions().add(item);
		assertThrows(MetaDataException.class, () -> module.convert("test"));
	}

	@Test
	void convertThrowsWhenMenuItemRoleNameIsNull() {
		ModuleMetaData module = createModuleWithRole();
		EditItemMetaData item = new EditItemMetaData();
		item.setName("editItem");
		item.setDocumentName("TestDocument");
		GrantedTo g = new GrantedTo();
		// roleName is null
		item.getRoles().add(g);
		module.getMenu().getActions().add(item);
		assertThrows(MetaDataException.class, () -> module.convert("test"));
	}

	@Test
	void convertThrowsWhenMenuItemRoleNotDefinedInModule() {
		ModuleMetaData module = createModuleWithRole();
		EditItemMetaData item = new EditItemMetaData();
		item.setName("editItem");
		item.setDocumentName("TestDocument");
		item.getRoles().add(grantedTo("undefinedRole"));
		module.getMenu().getActions().add(item);
		assertThrows(MetaDataException.class, () -> module.convert("test"));
	}

	@Test
	void convertThrowsWhenEditMenuItemDocumentIsNull() {
		ModuleMetaData module = createModuleWithRole();
		EditItemMetaData item = new EditItemMetaData();
		item.setName("editItem");
		// documentName is null
		item.getRoles().add(grantedTo("viewer"));
		module.getMenu().getActions().add(item);
		assertThrows(MetaDataException.class, () -> module.convert("test"));
	}

	@Test
	void convertThrowsWhenListMenuItemHasNothingDefined() {
		ModuleMetaData module = createModuleWithRole();
		ListItemMetaData item = new ListItemMetaData();
		item.setName("listItem");
		// no document, query, or model
		item.getRoles().add(grantedTo("viewer"));
		module.getMenu().getActions().add(item);
		assertThrows(MetaDataException.class, () -> module.convert("test"));
	}

	@Test
	void convertThrowsWhenMenuItemNameIsNull() {
		ModuleMetaData module = createModuleWithRole();
		EditItemMetaData item = new EditItemMetaData();
		// name is null
		item.setDocumentName("TestDocument");
		item.getRoles().add(grantedTo("viewer"));
		module.getMenu().getActions().add(item);
		assertThrows(MetaDataException.class, () -> module.convert("test"));
	}

	@Test
	void convertThrowsWhenMenuItemHasDuplicateRoles() {
		ModuleMetaData module = createModuleWithRole();
		EditItemMetaData item = new EditItemMetaData();
		item.setName("editItem");
		item.setDocumentName("TestDocument");
		item.getRoles().add(grantedTo("viewer"));
		item.getRoles().add(grantedTo("viewer")); // duplicate
		module.getMenu().getActions().add(item);
		assertThrows(MetaDataException.class, () -> module.convert("test"));
	}

	// ── Role privilege tests ──────────────────────────────────────────────────

	@Test
	void convertSucceedsWithActionPrivilege() {
		ModuleMetaData module = createMinimalModuleMetaData();
		ModuleRoleMetaData role = new ModuleRoleMetaData();
		role.setName("viewer");
		DocumentPrivilegeMetaData privilege = new DocumentPrivilegeMetaData();
		privilege.setDocumentName("TestDocument");
		privilege.setPermission(DocumentPermission._R__G);
		ActionPrivilegeMetaData action = new ActionPrivilegeMetaData();
		action.setActionName("Edit");
		privilege.getActions().add(action);
		role.getPrivileges().add(privilege);
		module.getRoles().add(role);
		assertNotNull(module.convert("test"));
	}

	@Test
	void convertSucceedsWithContentRestriction() {
		ModuleMetaData module = createMinimalModuleMetaData();
		ModuleRoleMetaData role = new ModuleRoleMetaData();
		role.setName("viewer");
		DocumentPrivilegeMetaData privilege = new DocumentPrivilegeMetaData();
		privilege.setDocumentName("TestDocument");
		privilege.setPermission(DocumentPermission._R__G);
		ContentRestriction restriction = new ContentRestriction();
		restriction.setAttributeName("photo");
		privilege.getContentRestrictions().add(restriction);
		role.getPrivileges().add(privilege);
		module.getRoles().add(role);
		assertNotNull(module.convert("test"));
	}

	@Test
	void convertThrowsWhenContentRestrictionAttributeNameIsNull() {
		ModuleMetaData module = createMinimalModuleMetaData();
		ModuleRoleMetaData role = new ModuleRoleMetaData();
		role.setName("viewer");
		DocumentPrivilegeMetaData privilege = new DocumentPrivilegeMetaData();
		privilege.setDocumentName("TestDocument");
		privilege.setPermission(DocumentPermission._R__G);
		ContentRestriction restriction = new ContentRestriction();
		// attributeName is null
		privilege.getContentRestrictions().add(restriction);
		role.getPrivileges().add(privilege);
		module.getRoles().add(role);
		assertThrows(MetaDataException.class, () -> module.convert("test"));
	}

	@Test
	void convertSucceedsWithContentPermission() {
		ModuleMetaData module = createMinimalModuleMetaData();
		ModuleRoleMetaData role = new ModuleRoleMetaData();
		role.setName("viewer");
		DocumentPrivilegeMetaData privilege = new DocumentPrivilegeMetaData();
		privilege.setDocumentName("TestDocument");
		privilege.setPermission(DocumentPermission._R__G);
		ContentPermission permission = new ContentPermission();
		permission.setAttributeName("photo");
		privilege.getContentPermissions().add(permission);
		role.getPrivileges().add(privilege);
		module.getRoles().add(role);
		assertNotNull(module.convert("test"));
	}

	@Test
	void convertThrowsWhenContentPermissionAttributeNameIsNull() {
		ModuleMetaData module = createMinimalModuleMetaData();
		ModuleRoleMetaData role = new ModuleRoleMetaData();
		role.setName("viewer");
		DocumentPrivilegeMetaData privilege = new DocumentPrivilegeMetaData();
		privilege.setDocumentName("TestDocument");
		privilege.setPermission(DocumentPermission._R__G);
		ContentPermission permission = new ContentPermission();
		// attributeName is null
		privilege.getContentPermissions().add(permission);
		role.getPrivileges().add(privilege);
		module.getRoles().add(role);
		assertThrows(MetaDataException.class, () -> module.convert("test"));
	}

	@Test
	void convertThrowsWhenPrivilegeOnCrossModuleDocument() {
		ModuleMetaData module = createMinimalModuleMetaData();
		ModuleDocumentMetaData crossModuleDoc = new ModuleDocumentMetaData();
		crossModuleDoc.setRef("CrossDoc");
		crossModuleDoc.setModuleRef("otherModule");
		module.getDocuments().add(crossModuleDoc);

		ModuleRoleMetaData role = new ModuleRoleMetaData();
		role.setName("viewer");
		DocumentPrivilegeMetaData privilege = new DocumentPrivilegeMetaData();
		privilege.setDocumentName("CrossDoc");
		privilege.setPermission(DocumentPermission._R__G);
		role.getPrivileges().add(privilege);
		module.getRoles().add(role);
		assertThrows(MetaDataException.class, () -> module.convert("test"));
	}

	// ── Additional menu item type tests ──────────────────────────────────────

	@Test
	void convertSucceedsWithLinkMenuItem() {
		ModuleMetaData module = createModuleWithRole();
		LinkItemMetaData item = new LinkItemMetaData();
		item.setName("externalLink");
		item.setHref("https://example.com");
		item.getRoles().add(grantedTo("viewer"));
		module.getMenu().getActions().add(item);
		assertNotNull(module.convert("test"));
	}

	@Test
	void convertThrowsWhenLinkMenuItemHrefIsNull() {
		ModuleMetaData module = createModuleWithRole();
		LinkItemMetaData item = new LinkItemMetaData();
		item.setName("brokenLink");
		// href is null
		item.getRoles().add(grantedTo("viewer"));
		module.getMenu().getActions().add(item);
		assertThrows(MetaDataException.class, () -> module.convert("test"));
	}

	@Test
	void convertSucceedsWithGroupMenuItemContainingEditItem() {
		ModuleMetaData module = createModuleWithRole();
		GroupMetaData group = new GroupMetaData();
		group.setName("adminGroup");
		EditItemMetaData editItem = new EditItemMetaData();
		editItem.setName("editInGroup");
		editItem.setDocumentName("TestDocument");
		editItem.getRoles().add(grantedTo("viewer"));
		group.getActions().add(editItem);
		module.getMenu().getActions().add(group);
		assertNotNull(module.convert("test"));
	}

	@Test
	void convertThrowsWhenGroupMenuItemNameIsNull() {
		ModuleMetaData module = createModuleWithRole();
		GroupMetaData group = new GroupMetaData();
		// name is null
		module.getMenu().getActions().add(group);
		assertThrows(MetaDataException.class, () -> module.convert("test"));
	}

	@Test
	void convertSucceedsWithGroupMenuItemWithName() {
		ModuleMetaData module = createModuleWithRole();
		GroupMetaData group = new GroupMetaData();
		group.setName("myGroup");
		EditItemMetaData editItem = new EditItemMetaData();
		editItem.setName("editInGroup");
		editItem.setDocumentName("TestDocument");
		editItem.getRoles().add(grantedTo("viewer"));
		group.getActions().add(editItem);
		module.getMenu().getActions().add(group);
		assertNotNull(module.convert("test"));
	}

	@Test
	void convertSucceedsWithTreeMenuItemWithDocument() {
		ModuleMetaData module = createModuleWithRole();
		TreeItemMetaData item = new TreeItemMetaData();
		item.setName("treeItem");
		item.setDocumentName("TestDocument");
		item.getRoles().add(grantedTo("viewer"));
		module.getMenu().getActions().add(item);
		assertNotNull(module.convert("test"));
	}

	@Test
	void convertThrowsWhenTreeMenuItemHasNothingDefined() {
		ModuleMetaData module = createModuleWithRole();
		TreeItemMetaData item = new TreeItemMetaData();
		item.setName("treeItem");
		// no document, query, or model
		item.getRoles().add(grantedTo("viewer"));
		module.getMenu().getActions().add(item);
		assertThrows(MetaDataException.class, () -> module.convert("test"));
	}

	// ── Additional query column tests ────────────────────────────────────────

	@Test
	void convertSucceedsWithContentQueryColumn() {
		ModuleMetaData module = createMinimalModuleMetaData();
		MetaDataQueryMetaData q = createMetaDataQuery("myQuery", "TestDocument");
		MetaDataQueryContentColumnMetaData col = new MetaDataQueryContentColumnMetaData();
		col.setBinding("photo");
		col.setDisplay(MetaDataQueryContentColumnMetaData.DisplayType.link);
		q.getColumns().add(col);
		module.getQueries().add(q);
		assertNotNull(module.convert("test"));
	}

	@Test
	void convertThrowsWhenContentColumnBindingIsNull() {
		ModuleMetaData module = createMinimalModuleMetaData();
		MetaDataQueryMetaData q = createMetaDataQuery("myQuery", "TestDocument");
		MetaDataQueryContentColumnMetaData col = new MetaDataQueryContentColumnMetaData();
		col.setDisplay(MetaDataQueryContentColumnMetaData.DisplayType.link);
		// binding is null
		q.getColumns().add(col);
		module.getQueries().add(q);
		assertThrows(MetaDataException.class, () -> module.convert("test"));
	}

	@Test
	void convertThrowsWhenContentColumnHasEmptyThumbnailButNotThumbnailDisplay() {
		ModuleMetaData module = createMinimalModuleMetaData();
		MetaDataQueryMetaData q = createMetaDataQuery("myQuery", "TestDocument");
		MetaDataQueryContentColumnMetaData col = new MetaDataQueryContentColumnMetaData();
		col.setBinding("photo");
		col.setDisplay(MetaDataQueryContentColumnMetaData.DisplayType.link);
		col.setEmptyThumbnailRelativeFile("images/empty.png");
		q.getColumns().add(col);
		module.getQueries().add(q);
		assertThrows(MetaDataException.class, () -> module.convert("test"));
	}

	@Test
	void convertSucceedsWithContentColumnThumbnailAndEmptyFile() {
		ModuleMetaData module = createMinimalModuleMetaData();
		MetaDataQueryMetaData q = createMetaDataQuery("myQuery", "TestDocument");
		MetaDataQueryContentColumnMetaData col = new MetaDataQueryContentColumnMetaData();
		col.setBinding("photo");
		col.setDisplay(MetaDataQueryContentColumnMetaData.DisplayType.thumbnail);
		col.setEmptyThumbnailRelativeFile("images/empty.png");
		q.getColumns().add(col);
		module.getQueries().add(q);
		assertNotNull(module.convert("test"));
	}

	@Test
	void convertSucceedsWithProjectedColumnWithExpression() {
		ModuleMetaData module = createMinimalModuleMetaData();
		MetaDataQueryMetaData q = createMetaDataQuery("myQuery", "TestDocument");
		MetaDataQueryProjectedColumnMetaData col = new MetaDataQueryProjectedColumnMetaData();
		col.setExpression("bean.name");
		col.setName("nameExpr");
		q.getColumns().add(col);
		module.getQueries().add(q);
		assertNotNull(module.convert("test"));
	}

	@Test
	void convertThrowsWhenQueryColumnFilterOperatorRequiresExpressionButNoneProvided() {
		ModuleMetaData module = createMinimalModuleMetaData();
		MetaDataQueryMetaData q = createMetaDataQuery("myQuery", "TestDocument");
		MetaDataQueryProjectedColumnMetaData col = new MetaDataQueryProjectedColumnMetaData();
		col.setBinding("name");
		col.setFilterOperator(org.skyve.metadata.FilterOperator.equal);
		// filterExpression is null — should throw
		q.getColumns().add(col);
		module.getQueries().add(q);
		assertThrows(MetaDataException.class, () -> module.convert("test"));
	}

	@Test
	void convertSucceedsWithQueryColumnIsNullFilterOperatorAndNoExpression() {
		ModuleMetaData module = createMinimalModuleMetaData();
		MetaDataQueryMetaData q = createMetaDataQuery("myQuery", "TestDocument");
		MetaDataQueryProjectedColumnMetaData col = new MetaDataQueryProjectedColumnMetaData();
		col.setBinding("name");
		col.setFilterOperator(org.skyve.metadata.FilterOperator.isNull);
		// filterExpression not needed for isNull
		q.getColumns().add(col);
		module.getQueries().add(q);
		assertNotNull(module.convert("test"));
	}

	@Test
	void convertThrowsWhenQueryColumnHasFilterExpressionButNoOperator() {
		ModuleMetaData module = createMinimalModuleMetaData();
		MetaDataQueryMetaData q = createMetaDataQuery("myQuery", "TestDocument");
		MetaDataQueryProjectedColumnMetaData col = new MetaDataQueryProjectedColumnMetaData();
		col.setBinding("name");
		col.setFilterExpression("someValue");
		// filterOperator is null — should throw
		q.getColumns().add(col);
		module.getQueries().add(q);
		assertThrows(MetaDataException.class, () -> module.convert("test"));
	}
}
