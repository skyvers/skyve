package org.skyve.metadata.module.fluent;

import static org.hamcrest.CoreMatchers.hasItem;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.not;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import java.util.Collections;
import java.util.Set;
import java.util.TreeMap;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.repository.module.ContentPermission;
import org.skyve.impl.metadata.repository.module.ContentRestriction;
import org.skyve.impl.metadata.user.ActionPrivilege;
import org.skyve.impl.metadata.user.DocumentPrivilege;
import org.skyve.impl.metadata.user.RoleImpl;
import org.skyve.metadata.user.DocumentPermission;
import org.skyve.metadata.user.UserAccess;

class FluentModuleRoleTest {
	
	private FluentModuleRole fluent;
	
	@BeforeEach
	void setup() {
		fluent = new FluentModuleRole();
	}

	@Test
	void testAddSingluarAggregateAccess() {
		// setup the test data
		FluentModuleRoleSingularAccess access = new FluentModuleRoleSingularAccess();
		access.documentName("TestDocument");

		// validate the test data
		assertEquals(0, fluent.get().getAccesses().size());

		// call the method under test
		fluent.addSingularAccess(access);

		// verify the result
		assertEquals(1, fluent.get().getAccesses().size());
	}

	@Test
	void testAddDocumentAggregateAccess() {
		// setup the test data
		FluentModuleRoleDocumentAggregateAccess access = new FluentModuleRoleDocumentAggregateAccess();
		access.documentName("TestDocument");

		// validate the test data
		assertEquals(0, fluent.get().getAccesses().size());

		// call the method under test
		fluent.addDocumentAggregateAccess(access);

		// verify the result
		assertEquals(1, fluent.get().getAccesses().size());
	}

	@Test
	void testAddQueryAggregateAccess() {
		// setup the test data
		FluentModuleRoleQueryAggregateAccess access = new FluentModuleRoleQueryAggregateAccess();
		access.queryName("TestQuery");

		// validate the test data
		assertEquals(0, fluent.get().getAccesses().size());

		// call the method under test
		fluent.addQueryAggregateAccess(access);

		// verify the result
		assertEquals(1, fluent.get().getAccesses().size());
	}

	@Test
	void testAddModelAggregateAccess() {
		// setup the test data
		FluentModuleRoleModelAggregateAccess access = new FluentModuleRoleModelAggregateAccess();
		access.modelName("TestModel");

		// validate the test data
		assertEquals(0, fluent.get().getAccesses().size());

		// call the method under test
		fluent.addModelAggregateAccess(access);

		// verify the result
		assertEquals(1, fluent.get().getAccesses().size());
	}

	@Test
	void testAddPreviousCompleteAccess() {
		// setup the test data
		FluentModuleRolePreviousCompleteAccess access = new FluentModuleRolePreviousCompleteAccess();
		access.documentName("TestDocument");

		// validate the test data
		assertEquals(0, fluent.get().getAccesses().size());

		// call the method under test
		fluent.addPreviousCompleteAccess(access);

		// verify the result
		assertEquals(1, fluent.get().getAccesses().size());
	}

	@Test
	void testAddReportAccess() {
		// setup the test data
		FluentModuleRoleReportAccess access = new FluentModuleRoleReportAccess();
		access.moduleName("TestModule");
		access.documentName("TestDocument");
		access.reportName("TestReport");
		
		// validate the test data
		assertEquals(0, fluent.get().getAccesses().size());

		// call the method under test
		fluent.addReportAccess(access);

		// verify the result
		assertEquals(1, fluent.get().getAccesses().size());
	}

	@Test
	void testAddDynamicImageAccess() {
		// setup the test data
		FluentModuleRoleDynamicImageAccess access = new FluentModuleRoleDynamicImageAccess();
		access.documentName("TestDocument");
		access.imageName("TestImage");
		
		// validate the test data
		assertEquals(0, fluent.get().getAccesses().size());

		// call the method under test
		fluent.addDynamicImageAccess(access);

		// verify the result
		assertEquals(1, fluent.get().getAccesses().size());
	}

	@Test
	void testAddContentAccess() {
		// setup the test data
		FluentModuleRoleContentAccess access = new FluentModuleRoleContentAccess();
		access.documentName("TestDocument");
		access.binding("TestContent");
		
		// validate the test data
		assertEquals(0, fluent.get().getAccesses().size());

		// call the method under test
		fluent.addContentAccess(access);

		// verify the result
		assertEquals(1, fluent.get().getAccesses().size());
	}

	@Test
	void testClearAccesses() {
		// setup the test data
		FluentModuleRoleDocumentAggregateAccess access1 = new FluentModuleRoleDocumentAggregateAccess();
		access1.documentName("TestDocument1");

		FluentModuleRoleSingularAccess access2 = new FluentModuleRoleSingularAccess();
		access2.documentName("TestDocument2");

		fluent.addDocumentAggregateAccess(access1);
		fluent.addSingularAccess(access2);

		// validate the test data
		assertEquals(2, fluent.get().getAccesses().size());

		// call the method under test
		fluent.clearAccesses();

		// verify the result
		assertEquals(0, fluent.get().getAccesses().size());
	}

	@Test
	void testFindSingularAccess() {
		// setup the test data
		FluentModuleRoleSingularAccess access1 = new FluentModuleRoleSingularAccess();
		access1.documentName("TestDocument1");

		FluentModuleRoleSingularAccess access2 = new FluentModuleRoleSingularAccess();
		access2.documentName("TestDocument2");

		fluent.addSingularAccess(access1);
		fluent.addSingularAccess(access2);

		// validate the test data
		assertEquals(2, fluent.get().getAccesses().size());

		// call the method under test
		FluentModuleRoleSingularAccess result = fluent.findSingularAccess("TestDocument1");

		// verify the result
		assertThat(result, is(notNullValue()));
		assertThat(result.get().getDocumentName(), is("TestDocument1"));
	}

	@Test
	void testFindDocumentAggregateAccess() {
		// setup the test data
		FluentModuleRoleDocumentAggregateAccess access1 = new FluentModuleRoleDocumentAggregateAccess();
		access1.documentName("TestDocument1");

		FluentModuleRoleDocumentAggregateAccess access2 = new FluentModuleRoleDocumentAggregateAccess();
		access2.documentName("TestDocument2");

		fluent.addDocumentAggregateAccess(access1);
		fluent.addDocumentAggregateAccess(access2);

		// validate the test data
		assertEquals(2, fluent.get().getAccesses().size());

		// call the method under test
		FluentModuleRoleDocumentAggregateAccess result = fluent.findDocumentAggregateAccess("TestDocument1");

		// verify the result
		assertThat(result, is(notNullValue()));
		assertThat(result.get().getDocumentName(), is("TestDocument1"));
	}

	@Test
	void testFindQueryAggregateAccess() {
		// setup the test data
		FluentModuleRoleQueryAggregateAccess access1 = new FluentModuleRoleQueryAggregateAccess();
		access1.queryName("TestQuery1");

		FluentModuleRoleQueryAggregateAccess access2 = new FluentModuleRoleQueryAggregateAccess();
		access2.queryName("TestQuery2");

		fluent.addQueryAggregateAccess(access1);
		fluent.addQueryAggregateAccess(access2);

		// validate the test data
		assertEquals(2, fluent.get().getAccesses().size());

		// call the method under test
		FluentModuleRoleQueryAggregateAccess result = fluent.findQueryAggregateAccess("TestQuery1");

		// verify the result
		assertThat(result, is(notNullValue()));
		assertThat(result.get().getQueryName(), is("TestQuery1"));
	}

	@Test
	void testFindModelAggregateAccess() {
		// setup the test data
		FluentModuleRoleModelAggregateAccess access1 = new FluentModuleRoleModelAggregateAccess();
		access1.documentName("TestDocument").modelName("TestModel1");

		FluentModuleRoleModelAggregateAccess access2 = new FluentModuleRoleModelAggregateAccess();
		access2.documentName("TestDocument").modelName("TestModel2");

		fluent.addModelAggregateAccess(access1);
		fluent.addModelAggregateAccess(access2);

		// validate the test data
		assertEquals(2, fluent.get().getAccesses().size());

		// call the method under test
		FluentModuleRoleModelAggregateAccess result = fluent.findModelAggregateAccess("TestDocument", "TestModel1");

		// verify the result
		assertThat(result, is(notNullValue()));
		assertThat(result.get().getDocumentName(), is("TestDocument"));
		assertThat(result.get().getModelName(), is("TestModel1"));
	}

	@Test
	void testFindPreviousCompleteAccess() {
		// setup the test data
		FluentModuleRolePreviousCompleteAccess access1 = new FluentModuleRolePreviousCompleteAccess();
		access1.documentName("TestDocument").binding("binding1");

		FluentModuleRolePreviousCompleteAccess access2 = new FluentModuleRolePreviousCompleteAccess();
		access2.documentName("TestDocument").binding("binding2");

		fluent.addPreviousCompleteAccess(access1);
		fluent.addPreviousCompleteAccess(access2);

		// validate the test data
		assertEquals(2, fluent.get().getAccesses().size());

		// call the method under test
		FluentModuleRolePreviousCompleteAccess result = fluent.findPreviousCompleteAccess("TestDocument", "binding1");

		// verify the result
		assertThat(result, is(notNullValue()));
		assertThat(result.get().getDocumentName(), is("TestDocument"));
		assertThat(result.get().getBinding(), is("binding1"));
	}

	@Test
	void testFindReportAccess() {
		// setup the test data
		FluentModuleRoleReportAccess access1 = new FluentModuleRoleReportAccess();
		access1.moduleName("TestModule").documentName("TestDocument").reportName("TestReport1");

		FluentModuleRoleReportAccess access2 = new FluentModuleRoleReportAccess();
		access2.moduleName("TestModule").documentName("TestDocument").reportName("TestReport2");

		fluent.addReportAccess(access1);
		fluent.addReportAccess(access2);

		// validate the test data
		assertEquals(2, fluent.get().getAccesses().size());

		// call the method under test
		FluentModuleRoleReportAccess result = fluent.findReportAccess("TestModule", "TestDocument", "TestReport1");

		// verify the result
		assertThat(result, is(notNullValue()));
		assertThat(result.get().getModuleName(), is("TestModule"));
		assertThat(result.get().getDocumentName(), is("TestDocument"));
		assertThat(result.get().getReportName(), is("TestReport1"));
	}

	@Test
	void testFindDynamicImageAccess() {
		// setup the test data
		FluentModuleRoleDynamicImageAccess access1 = new FluentModuleRoleDynamicImageAccess();
		access1.documentName("TestDocument").imageName("TestImage1");

		FluentModuleRoleDynamicImageAccess access2 = new FluentModuleRoleDynamicImageAccess();
		access2.documentName("TestDocument").imageName("TestImage2");

		fluent.addDynamicImageAccess(access1);
		fluent.addDynamicImageAccess(access2);

		// validate the test data
		assertEquals(2, fluent.get().getAccesses().size());

		// call the method under test
		FluentModuleRoleDynamicImageAccess result = fluent.findDynamicImageAccess("TestDocument", "TestImage1");

		// verify the result
		assertThat(result, is(notNullValue()));
		assertThat(result.get().getDocumentName(), is("TestDocument"));
		assertThat(result.get().getImageName(), is("TestImage1"));
	}

	@Test
	void testFindContentAccess() {
		// setup the test data
		FluentModuleRoleContentAccess access1 = new FluentModuleRoleContentAccess();
		access1.documentName("TestDocument").binding("binding1");

		FluentModuleRoleContentAccess access2 = new FluentModuleRoleContentAccess();
		access2.documentName("TestDocument").binding("binding2");

		fluent.addContentAccess(access1);
		fluent.addContentAccess(access2);

		// validate the test data
		assertEquals(2, fluent.get().getAccesses().size());

		// call the method under test
		FluentModuleRoleContentAccess result = fluent.findContentAccess("TestDocument", "binding1");

		// verify the result
		assertThat(result, is(notNullValue()));
		assertThat(result.get().getDocumentName(), is("TestDocument"));
		assertThat(result.get().getBinding(), is("binding1"));
	}

	@Test
	void testRemoveSingularAccess() {
		// setup the test data
		FluentModuleRoleSingularAccess access1 = new FluentModuleRoleSingularAccess();
		access1.documentName("TestDocument1");

		FluentModuleRoleSingularAccess access2 = new FluentModuleRoleSingularAccess();
		access2.documentName("TestDocument2");

		fluent.addSingularAccess(access1);
		fluent.addSingularAccess(access2);

		// validate the test data
		assertEquals(2, fluent.get().getAccesses().size());

		// call the method under test
		fluent.removeSingularAccess("TestDocument1");

		// verify the result
		assertEquals(1, fluent.get().getAccesses().size());
		assertThat(fluent.get().getAccesses(), not(hasItem(access1.get())));
		assertThat(fluent.get().getAccesses(), hasItem(access2.get()));
	}

	@Test
	void testRemoveDocumentAggregateAccess() {
		// setup the test data
		FluentModuleRoleDocumentAggregateAccess access1 = new FluentModuleRoleDocumentAggregateAccess();
		access1.documentName("TestDocument1");

		FluentModuleRoleDocumentAggregateAccess access2 = new FluentModuleRoleDocumentAggregateAccess();
		access2.documentName("TestDocument2");

		fluent.addDocumentAggregateAccess(access1);
		fluent.addDocumentAggregateAccess(access2);

		// validate the test data
		assertEquals(2, fluent.get().getAccesses().size());

		// call the method under test
		fluent.removeDocumentAggregateAccess("TestDocument1");

		// verify the result
		assertEquals(1, fluent.get().getAccesses().size());
		assertThat(fluent.get().getAccesses(), not(hasItem(access1.get())));
		assertThat(fluent.get().getAccesses(), hasItem(access2.get()));
	}

	@Test
	void testRemoveQueryAggregateAccess() {
		// setup the test data
		FluentModuleRoleQueryAggregateAccess access1 = new FluentModuleRoleQueryAggregateAccess();
		access1.queryName("TestQuery1");

		FluentModuleRoleQueryAggregateAccess access2 = new FluentModuleRoleQueryAggregateAccess();
		access2.queryName("TestQuery2");

		fluent.addQueryAggregateAccess(access1);
		fluent.addQueryAggregateAccess(access2);

		// validate the test data
		assertEquals(2, fluent.get().getAccesses().size());

		// call the method under test
		fluent.removeQueryAggregateAccess("TestQuery1");

		// verify the result
		assertEquals(1, fluent.get().getAccesses().size());
		assertThat(fluent.get().getAccesses(), not(hasItem(access1.get())));
		assertThat(fluent.get().getAccesses(), hasItem(access2.get()));
	}

	@Test
	void testRemoveModelAggregateAccess() {
		// setup the test data
		FluentModuleRoleModelAggregateAccess access1 = new FluentModuleRoleModelAggregateAccess();
		access1.documentName("TestDocument").modelName("TestModel1");

		FluentModuleRoleModelAggregateAccess access2 = new FluentModuleRoleModelAggregateAccess();
		access2.documentName("TestDocument").modelName("TestModel2");

		fluent.addModelAggregateAccess(access1);
		fluent.addModelAggregateAccess(access2);

		// validate the test data
		assertEquals(2, fluent.get().getAccesses().size());

		// call the method under test
		fluent.removeModelAggregateAccess("TestDocument", "TestModel1");

		// verify the result
		assertEquals(1, fluent.get().getAccesses().size());
		assertThat(fluent.get().getAccesses(), not(hasItem(access1.get())));
		assertThat(fluent.get().getAccesses(), hasItem(access2.get()));
	}

	@Test
	void testRemovePreviousCompleteAccess() {
		// setup the test data
		FluentModuleRolePreviousCompleteAccess access1 = new FluentModuleRolePreviousCompleteAccess();
		access1.documentName("TestDocument1");
		access1.binding("binding1");

		FluentModuleRolePreviousCompleteAccess access2 = new FluentModuleRolePreviousCompleteAccess();
		access2.documentName("TestDocument2");
		access2.binding("binding2");

		fluent.addPreviousCompleteAccess(access1);
		fluent.addPreviousCompleteAccess(access2);

		// validate the test data
		assertEquals(2, fluent.get().getAccesses().size());

		// call the method under test
		fluent.removePreviousCompleteAccess("TestDocument1", "binding1");

		// verify the result
		assertEquals(1, fluent.get().getAccesses().size());
		assertThat(fluent.get().getAccesses(), not(hasItem(access1.get())));
		assertThat(fluent.get().getAccesses(), hasItem(access2.get()));
	}

	@Test
	void testRemoveReportAccess() {
		// setup the test data
		FluentModuleRoleReportAccess access1 = new FluentModuleRoleReportAccess();
		access1.moduleName("TestModule1");
		access1.documentName("TestDocument1");
		access1.reportName("TestReport1");

		FluentModuleRoleReportAccess access2 = new FluentModuleRoleReportAccess();
		access2.moduleName("TestModule2");
		access2.documentName("TestDocument2");
		access2.reportName("TestReport2");

		fluent.addReportAccess(access1);
		fluent.addReportAccess(access2);

		// validate the test data
		assertEquals(2, fluent.get().getAccesses().size());

		// call the method under test
		fluent.removeReportAccess("TestModule1", "TestDocument1", "TestReport1");

		// verify the result
		assertEquals(1, fluent.get().getAccesses().size());
		assertThat(fluent.get().getAccesses(), not(hasItem(access1.get())));
		assertThat(fluent.get().getAccesses(), hasItem(access2.get()));
	}

	@Test
	void testRemoveDynamicImageAccess() {
		// setup the test data
		FluentModuleRoleDynamicImageAccess access1 = new FluentModuleRoleDynamicImageAccess();
		access1.documentName("TestDocument1");
		access1.imageName("TestImage1");

		FluentModuleRoleDynamicImageAccess access2 = new FluentModuleRoleDynamicImageAccess();
		access2.documentName("TestDocument2");
		access2.imageName("TestImage2");

		fluent.addDynamicImageAccess(access1);
		fluent.addDynamicImageAccess(access2);

		// validate the test data
		assertEquals(2, fluent.get().getAccesses().size());

		// call the method under test
		fluent.removeDynamicImageAccess("TestDocument1", "TestImage1");

		// verify the result
		assertEquals(1, fluent.get().getAccesses().size());
		assertThat(fluent.get().getAccesses(), not(hasItem(access1.get())));
		assertThat(fluent.get().getAccesses(), hasItem(access2.get()));
	}
	
	@Test
	void testRemoveContentAccess() {
		// setup the test data
		FluentModuleRoleContentAccess access1 = new FluentModuleRoleContentAccess();
		access1.documentName("TestDocument1");
		access1.binding("binding1");

		FluentModuleRoleContentAccess access2 = new FluentModuleRoleContentAccess();
		access2.documentName("TestDocument2");
		access2.binding("binding2");

		fluent.addContentAccess(access1);
		fluent.addContentAccess(access2);

		// validate the test data
		assertEquals(2, fluent.get().getAccesses().size());

		// call the method under test
		fluent.removeContentAccess("TestDocument1", "binding1");

		// verify the result
		assertEquals(1, fluent.get().getAccesses().size());
		assertThat(fluent.get().getAccesses(), not(hasItem(access1.get())));
		assertThat(fluent.get().getAccesses(), hasItem(access2.get()));
	}

	// ---- name / description / documentation ----

	@Test
	void testNameSetsValue() {
		fluent.name("AdminRole");
		assertThat(fluent.get().getName(), is("AdminRole"));
	}

	@Test
	void testDescriptionSetsValue() {
		fluent.description("Administrator role");
		assertThat(fluent.get().getDescription(), is("Administrator role"));
	}

	@Test
	void testDocumentationSetsValue() {
		fluent.documentation("See admin guide");
		assertThat(fluent.get().getDocumentation(), is("See admin guide"));
	}

	// ---- wrapping constructor ----

	@Test
	@SuppressWarnings("static-method")
	void testWrappingConstructorPreservesInstance() {
		org.skyve.impl.metadata.repository.module.ModuleRoleMetaData meta =
				new org.skyve.impl.metadata.repository.module.ModuleRoleMetaData();
		meta.setName("existing");
		FluentModuleRole wrapped = new FluentModuleRole(meta);
		assertThat(wrapped.get(), is(meta));
		assertThat(wrapped.get().getName(), is("existing"));
	}

	// ---- addPrivilege / findPrivilege / removePrivilege / clearPrivileges ----

	@Test
	void testAddPrivilegeAddsEntry() {
		FluentDocumentPrivilege privilege = new FluentDocumentPrivilege().documentName("Contact");
		fluent.addPrivilege(privilege);
		assertEquals(1, fluent.get().getPrivileges().size());
	}

	@Test
	void testFindPrivilegeReturnsMatch() {
		FluentDocumentPrivilege privilege = new FluentDocumentPrivilege().documentName("Contact");
		fluent.addPrivilege(privilege);
		assertThat(fluent.findPrivilege("Contact"), is(notNullValue()));
	}

	@Test
	void testFindPrivilegeReturnsNullWhenNotFound() {
		assertThat(fluent.findPrivilege("Missing"), is(org.hamcrest.CoreMatchers.nullValue()));
	}

	@Test
	void testRemovePrivilegeRemovesEntry() {
		FluentDocumentPrivilege privilege = new FluentDocumentPrivilege().documentName("Contact");
		fluent.addPrivilege(privilege);
		fluent.removePrivilege("Contact");
		assertEquals(0, fluent.get().getPrivileges().size());
	}

	@Test
	void testClearPrivilegesRemovesAll() {
		fluent.addPrivilege(new FluentDocumentPrivilege().documentName("Contact"));
		fluent.addPrivilege(new FluentDocumentPrivilege().documentName("User"));
		fluent.clearPrivileges();
		assertEquals(0, fluent.get().getPrivileges().size());
	}

	@Test
	@SuppressWarnings("static-method")
	void testFromCopiesNameDescriptionDocumentation() {
		RoleImpl role = new RoleImpl();
		role.setName("TestRole");
		role.setDescription("Test Description");
		role.setDocumentation("Test Documentation");

		FluentModuleRole result = new FluentModuleRole().from(role);

		assertEquals("TestRole", result.get().getName());
		assertEquals("Test Description", result.get().getDescription());
		assertEquals("Test Documentation", result.get().getDocumentation());
	}

	@Test
	@SuppressWarnings("static-method")
	void testFromCopiesDocumentPrivilegeWithPermission() {
		RoleImpl role = new RoleImpl();
		role.setName("TestRole");

		DocumentPrivilege dp = new DocumentPrivilege();
		dp.setName("Contact");
		dp.setPermission(DocumentPermission.CRUDC);
		role.getPrivileges().add(dp);

		FluentModuleRole result = new FluentModuleRole().from(role);

		assertNotNull(result.findPrivilege("Contact"));
	}

	@Test
	@SuppressWarnings("static-method")
	void testFromCopiesDocumentAndActionPrivileges() {
		RoleImpl role = new RoleImpl();
		role.setName("TestRole");

		DocumentPrivilege dp = new DocumentPrivilege();
		dp.setName("Contact");
		dp.setPermission(DocumentPermission.CRUDC);
		role.getPrivileges().add(dp);

		ActionPrivilege ap = new ActionPrivilege();
		ap.setName("save");
		ap.setDocumentName("Contact");
		role.getPrivileges().add(ap);

		FluentModuleRole result = new FluentModuleRole().from(role);

		assertNotNull(result.findPrivilege("Contact"));
	}

	@Test
	@SuppressWarnings("static-method")
	void testFromCopiesSingularAccess() {
		RoleImpl role = new RoleImpl();
		role.setName("TestRole");

		java.util.Map<UserAccess, Set<String>> accesses = new TreeMap<>();
		accesses.put(UserAccess.singular("testModule", "Contact"), Collections.singleton("uxui"));
		role.getAccesses().putAll(accesses);

		FluentModuleRole result = new FluentModuleRole().from(role);

		assertNotNull(result.findSingularAccess("Contact"));
	}

	@Test
	@SuppressWarnings("static-method")
	void testFromCopiesQueryAggregateAccess() {
		RoleImpl role = new RoleImpl();
		role.setName("TestRole");

		role.getAccesses().put(UserAccess.queryAggregate("testModule", "ContactQuery"), Collections.singleton("uxui"));

		FluentModuleRole result = new FluentModuleRole().from(role);

		assertNotNull(result.findQueryAggregateAccess("ContactQuery"));
	}

	@Test
	@SuppressWarnings("static-method")
	void testFromCopiesDocumentAggregateAccess() {
		RoleImpl role = new RoleImpl();
		role.setName("TestRole");

		role.getAccesses().put(UserAccess.documentAggregate("testModule", "Contact"), Collections.singleton("uxui"));

		FluentModuleRole result = new FluentModuleRole().from(role);

		assertNotNull(result.findDocumentAggregateAccess("Contact"));
	}

	@Test
	@SuppressWarnings("static-method")
	void testFromCopiesModelAggregateAccess() {
		RoleImpl role = new RoleImpl();
		role.setName("TestRole");

		role.getAccesses().put(UserAccess.modelAggregate("testModule", "Contact", "myModel"), Collections.singleton("uxui"));

		FluentModuleRole result = new FluentModuleRole().from(role);

		assertNotNull(result.findModelAggregateAccess("Contact", "myModel"));
	}

	@Test
	@SuppressWarnings("static-method")
	void testFromCopiesPreviousCompleteAccess() {
		RoleImpl role = new RoleImpl();
		role.setName("TestRole");

		role.getAccesses().put(UserAccess.previousComplete("testModule", "Contact", "status"), Collections.singleton("uxui"));

		FluentModuleRole result = new FluentModuleRole().from(role);

		assertNotNull(result.findPreviousCompleteAccess("Contact", "status"));
	}

	@Test
	@SuppressWarnings("static-method")
	void testFromCopiesReportAccess() {
		RoleImpl role = new RoleImpl();
		role.setName("TestRole");

		role.getAccesses().put(UserAccess.report("testModule", "Contact", "ContactReport"), Collections.singleton("uxui"));

		FluentModuleRole result = new FluentModuleRole().from(role);

		assertNotNull(result.findReportAccess("testModule", "Contact", "ContactReport"));
	}

	@Test
	@SuppressWarnings("static-method")
	void testFromCopiesDynamicImageAccess() {
		RoleImpl role = new RoleImpl();
		role.setName("TestRole");

		role.getAccesses().put(UserAccess.dynamicImage("testModule", "Contact", "myImage"), Collections.singleton("uxui"));

		FluentModuleRole result = new FluentModuleRole().from(role);

		assertNotNull(result.findDynamicImageAccess("Contact", "myImage"));
	}

	@Test
	@SuppressWarnings("static-method")
	void testFromCopiesContentAccess() {
		RoleImpl role = new RoleImpl();
		role.setName("TestRole");

		role.getAccesses().put(UserAccess.content("testModule", "Contact", "photo"), Collections.singleton("uxui"));

		FluentModuleRole result = new FluentModuleRole().from(role);

		assertNotNull(result.findContentAccess("Contact", "photo"));
	}

	@Test
	void findSingularAccessReturnsNullWhenMissing() {
		assertNull(fluent.findSingularAccess("Missing"));
	}

	@Test
	void findDocumentAggregateAccessReturnsNullWhenMissing() {
		assertNull(fluent.findDocumentAggregateAccess("Missing"));
	}

	@Test
	void findQueryAggregateAccessReturnsNullWhenMissing() {
		assertNull(fluent.findQueryAggregateAccess("Missing"));
	}

	@Test
	void findModelAggregateAccessReturnsNullWhenMissing() {
		assertNull(fluent.findModelAggregateAccess("Missing", "MissingModel"));
	}

	@Test
	void findPreviousCompleteAccessReturnsNullWhenMissing() {
		assertNull(fluent.findPreviousCompleteAccess("Missing", "binding"));
	}

	@Test
	void findReportAccessReturnsNullWhenMissing() {
		assertNull(fluent.findReportAccess("mod", "Missing", "report"));
	}

	@Test
	void findDynamicImageAccessReturnsNullWhenMissing() {
		assertNull(fluent.findDynamicImageAccess("Missing", "image"));
	}

	@Test
	void findContentAccessReturnsNullWhenMissing() {
		assertNull(fluent.findContentAccess("Missing", "binding"));
	}

	@Test
	@SuppressWarnings("static-method")
	void testFromCopiesContentPermission() {
		RoleImpl role = new RoleImpl();
		role.setName("TestRole");

		DocumentPrivilege docPriv = new DocumentPrivilege();
		docPriv.setName("Contact");
		docPriv.setPermission(org.skyve.metadata.user.DocumentPermission.CRUDG);
		role.getPrivileges().add(docPriv);

		ContentPermission perm = new ContentPermission();
		perm.setDocumentName("Contact");
		perm.setAttributeName("photo");
		role.getContentPermissions().add(perm);

		FluentModuleRole result = new FluentModuleRole().from(role);

		assertNotNull(result.findPrivilege("Contact"));
		assertFalse(result.findPrivilege("Contact").get().getContentPermissions().isEmpty());
	}

	@Test
	@SuppressWarnings("static-method")
	void testFromCopiesContentRestriction() {
		RoleImpl role = new RoleImpl();
		role.setName("TestRole");

		DocumentPrivilege docPriv = new DocumentPrivilege();
		docPriv.setName("Contact");
		docPriv.setPermission(org.skyve.metadata.user.DocumentPermission.CRUDG);
		role.getPrivileges().add(docPriv);

		ContentRestriction restr = new ContentRestriction();
		restr.setDocumentName("Contact");
		restr.setAttributeName("photo");
		role.getContentRestrictions().add(restr);

		FluentModuleRole result = new FluentModuleRole().from(role);

		assertNotNull(result.findPrivilege("Contact"));
		assertFalse(result.findPrivilege("Contact").get().getContentRestrictions().isEmpty());
	}
}
