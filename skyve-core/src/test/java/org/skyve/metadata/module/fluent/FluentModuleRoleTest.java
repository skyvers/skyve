package org.skyve.metadata.module.fluent;

import static org.hamcrest.CoreMatchers.hasItem;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.not;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

public class FluentModuleRoleTest {
	
	private FluentModuleRole fluent;
	
	@BeforeEach
	public void setup() throws Exception {
		fluent = new FluentModuleRole();
	}

	@Test
	@SuppressWarnings("boxing")
	public void testAddSingluarAggregateAccess() {
		// setup the test data
		FluentModuleRoleSingularAccess access = new FluentModuleRoleSingularAccess();
		access.documentName("TestDocument");

		// validate the test data
		assertThat(fluent.get().getAccesses().size(), is(0));

		// call the method under test
		fluent.addSingularAccess(access);

		// verify the result
		assertThat(fluent.get().getAccesses().size(), is(1));
	}

	@Test
	@SuppressWarnings("boxing")
	public void testAddDocumentAggregateAccess() {
		// setup the test data
		FluentModuleRoleDocumentAggregateAccess access = new FluentModuleRoleDocumentAggregateAccess();
		access.documentName("TestDocument");

		// validate the test data
		assertThat(fluent.get().getAccesses().size(), is(0));

		// call the method under test
		fluent.addDocumentAggregateAccess(access);

		// verify the result
		assertThat(fluent.get().getAccesses().size(), is(1));
	}

	@Test
	@SuppressWarnings("boxing")
	public void testAddQueryAggregateAccess() {
		// setup the test data
		FluentModuleRoleQueryAggregateAccess access = new FluentModuleRoleQueryAggregateAccess();
		access.queryName("TestQuery");

		// validate the test data
		assertThat(fluent.get().getAccesses().size(), is(0));

		// call the method under test
		fluent.addQueryAggregateAccess(access);

		// verify the result
		assertThat(fluent.get().getAccesses().size(), is(1));
	}

	@Test
	@SuppressWarnings("boxing")
	public void testAddModelAggregateAccess() {
		// setup the test data
		FluentModuleRoleModelAggregateAccess access = new FluentModuleRoleModelAggregateAccess();
		access.modelName("TestModel");

		// validate the test data
		assertThat(fluent.get().getAccesses().size(), is(0));

		// call the method under test
		fluent.addModelAggregateAccess(access);

		// verify the result
		assertThat(fluent.get().getAccesses().size(), is(1));
	}

	@Test
	@SuppressWarnings("boxing")
	public void testAddPreviousCompleteAccess() {
		// setup the test data
		FluentModuleRolePreviousCompleteAccess access = new FluentModuleRolePreviousCompleteAccess();
		access.documentName("TestDocument");

		// validate the test data
		assertThat(fluent.get().getAccesses().size(), is(0));

		// call the method under test
		fluent.addPreviousCompleteAccess(access);

		// verify the result
		assertThat(fluent.get().getAccesses().size(), is(1));
	}

	@Test
	@SuppressWarnings("boxing")
	public void testAddReportAccess() {
		// setup the test data
		FluentModuleRoleReportAccess access = new FluentModuleRoleReportAccess();
		access.moduleName("TestModule");
		access.documentName("TestDocument");
		access.reportName("TestReport");
		
		// validate the test data
		assertThat(fluent.get().getAccesses().size(), is(0));

		// call the method under test
		fluent.addReportAccess(access);

		// verify the result
		assertThat(fluent.get().getAccesses().size(), is(1));
	}

	@Test
	@SuppressWarnings("boxing")
	public void testAddDynamicImageAccess() {
		// setup the test data
		FluentModuleRoleDynamicImageAccess access = new FluentModuleRoleDynamicImageAccess();
		access.documentName("TestDocument");
		access.imageName("TestImage");
		
		// validate the test data
		assertThat(fluent.get().getAccesses().size(), is(0));

		// call the method under test
		fluent.addDynamicImageAccess(access);

		// verify the result
		assertThat(fluent.get().getAccesses().size(), is(1));
	}

	@Test
	@SuppressWarnings("boxing")
	public void testAddContentAccess() {
		// setup the test data
		FluentModuleRoleContentAccess access = new FluentModuleRoleContentAccess();
		access.documentName("TestDocument");
		access.binding("TestContent");
		
		// validate the test data
		assertThat(fluent.get().getAccesses().size(), is(0));

		// call the method under test
		fluent.addContentAccess(access);

		// verify the result
		assertThat(fluent.get().getAccesses().size(), is(1));
	}

	@Test
	@SuppressWarnings("boxing")
	public void testClearAccesses() {
		// setup the test data
		FluentModuleRoleDocumentAggregateAccess access1 = new FluentModuleRoleDocumentAggregateAccess();
		access1.documentName("TestDocument1");

		FluentModuleRoleSingularAccess access2 = new FluentModuleRoleSingularAccess();
		access2.documentName("TestDocument2");

		fluent.addDocumentAggregateAccess(access1);
		fluent.addSingularAccess(access2);

		// validate the test data
		assertThat(fluent.get().getAccesses().size(), is(2));

		// call the method under test
		fluent.clearAccesses();

		// verify the result
		assertThat(fluent.get().getAccesses().size(), is(0));
	}

	@Test
	@SuppressWarnings("boxing")
	public void testFindSingularAccess() {
		// setup the test data
		FluentModuleRoleSingularAccess access1 = new FluentModuleRoleSingularAccess();
		access1.documentName("TestDocument1");

		FluentModuleRoleSingularAccess access2 = new FluentModuleRoleSingularAccess();
		access2.documentName("TestDocument2");

		fluent.addSingularAccess(access1);
		fluent.addSingularAccess(access2);

		// validate the test data
		assertThat(fluent.get().getAccesses().size(), is(2));

		// call the method under test
		FluentModuleRoleSingularAccess result = fluent.findSingularAccess("TestDocument1");

		// verify the result
		assertThat(result, is(notNullValue()));
		assertThat(result.get().getDocumentName(), is("TestDocument1"));
	}

	@Test
	@SuppressWarnings("boxing")
	public void testFindDocumentAggregateAccess() {
		// setup the test data
		FluentModuleRoleDocumentAggregateAccess access1 = new FluentModuleRoleDocumentAggregateAccess();
		access1.documentName("TestDocument1");

		FluentModuleRoleDocumentAggregateAccess access2 = new FluentModuleRoleDocumentAggregateAccess();
		access2.documentName("TestDocument2");

		fluent.addDocumentAggregateAccess(access1);
		fluent.addDocumentAggregateAccess(access2);

		// validate the test data
		assertThat(fluent.get().getAccesses().size(), is(2));

		// call the method under test
		FluentModuleRoleDocumentAggregateAccess result = fluent.findDocumentAggregateAccess("TestDocument1");

		// verify the result
		assertThat(result, is(notNullValue()));
		assertThat(result.get().getDocumentName(), is("TestDocument1"));
	}

	@Test
	@SuppressWarnings("boxing")
	public void testFindQueryAggregateAccess() {
		// setup the test data
		FluentModuleRoleQueryAggregateAccess access1 = new FluentModuleRoleQueryAggregateAccess();
		access1.queryName("TestQuery1");

		FluentModuleRoleQueryAggregateAccess access2 = new FluentModuleRoleQueryAggregateAccess();
		access2.queryName("TestQuery2");

		fluent.addQueryAggregateAccess(access1);
		fluent.addQueryAggregateAccess(access2);

		// validate the test data
		assertThat(fluent.get().getAccesses().size(), is(2));

		// call the method under test
		FluentModuleRoleQueryAggregateAccess result = fluent.findQueryAggregateAccess("TestQuery1");

		// verify the result
		assertThat(result, is(notNullValue()));
		assertThat(result.get().getQueryName(), is("TestQuery1"));
	}

	@Test
	@SuppressWarnings("boxing")
	public void testFindModelAggregateAccess() {
		// setup the test data
		FluentModuleRoleModelAggregateAccess access1 = new FluentModuleRoleModelAggregateAccess();
		access1.documentName("TestDocument").modelName("TestModel1");

		FluentModuleRoleModelAggregateAccess access2 = new FluentModuleRoleModelAggregateAccess();
		access2.documentName("TestDocument").modelName("TestModel2");

		fluent.addModelAggregateAccess(access1);
		fluent.addModelAggregateAccess(access2);

		// validate the test data
		assertThat(fluent.get().getAccesses().size(), is(2));

		// call the method under test
		FluentModuleRoleModelAggregateAccess result = fluent.findModelAggregateAccess("TestDocument", "TestModel1");

		// verify the result
		assertThat(result, is(notNullValue()));
		assertThat(result.get().getDocumentName(), is("TestDocument"));
		assertThat(result.get().getModelName(), is("TestModel1"));
	}

	@Test
	@SuppressWarnings("boxing")
	public void testFindPreviousCompleteAccess() {
		// setup the test data
		FluentModuleRolePreviousCompleteAccess access1 = new FluentModuleRolePreviousCompleteAccess();
		access1.documentName("TestDocument").binding("binding1");

		FluentModuleRolePreviousCompleteAccess access2 = new FluentModuleRolePreviousCompleteAccess();
		access2.documentName("TestDocument").binding("binding2");

		fluent.addPreviousCompleteAccess(access1);
		fluent.addPreviousCompleteAccess(access2);

		// validate the test data
		assertThat(fluent.get().getAccesses().size(), is(2));

		// call the method under test
		FluentModuleRolePreviousCompleteAccess result = fluent.findPreviousCompleteAccess("TestDocument", "binding1");

		// verify the result
		assertThat(result, is(notNullValue()));
		assertThat(result.get().getDocumentName(), is("TestDocument"));
		assertThat(result.get().getBinding(), is("binding1"));
	}

	@Test
	@SuppressWarnings("boxing")
	public void testFindReportAccess() {
		// setup the test data
		FluentModuleRoleReportAccess access1 = new FluentModuleRoleReportAccess();
		access1.moduleName("TestModule").documentName("TestDocument").reportName("TestReport1");

		FluentModuleRoleReportAccess access2 = new FluentModuleRoleReportAccess();
		access2.moduleName("TestModule").documentName("TestDocument").reportName("TestReport2");

		fluent.addReportAccess(access1);
		fluent.addReportAccess(access2);

		// validate the test data
		assertThat(fluent.get().getAccesses().size(), is(2));

		// call the method under test
		FluentModuleRoleReportAccess result = fluent.findReportAccess("TestModule", "TestDocument", "TestReport1");

		// verify the result
		assertThat(result, is(notNullValue()));
		assertThat(result.get().getModuleName(), is("TestModule"));
		assertThat(result.get().getDocumentName(), is("TestDocument"));
		assertThat(result.get().getReportName(), is("TestReport1"));
	}

	@Test
	@SuppressWarnings("boxing")
	public void testFindDynamicImageAccess() {
		// setup the test data
		FluentModuleRoleDynamicImageAccess access1 = new FluentModuleRoleDynamicImageAccess();
		access1.documentName("TestDocument").imageName("TestImage1");

		FluentModuleRoleDynamicImageAccess access2 = new FluentModuleRoleDynamicImageAccess();
		access2.documentName("TestDocument").imageName("TestImage2");

		fluent.addDynamicImageAccess(access1);
		fluent.addDynamicImageAccess(access2);

		// validate the test data
		assertThat(fluent.get().getAccesses().size(), is(2));

		// call the method under test
		FluentModuleRoleDynamicImageAccess result = fluent.findDynamicImageAccess("TestDocument", "TestImage1");

		// verify the result
		assertThat(result, is(notNullValue()));
		assertThat(result.get().getDocumentName(), is("TestDocument"));
		assertThat(result.get().getImageName(), is("TestImage1"));
	}

	@Test
	@SuppressWarnings("boxing")
	public void testFindContentAccess() {
		// setup the test data
		FluentModuleRoleContentAccess access1 = new FluentModuleRoleContentAccess();
		access1.documentName("TestDocument").binding("binding1");

		FluentModuleRoleContentAccess access2 = new FluentModuleRoleContentAccess();
		access2.documentName("TestDocument").binding("binding2");

		fluent.addContentAccess(access1);
		fluent.addContentAccess(access2);

		// validate the test data
		assertThat(fluent.get().getAccesses().size(), is(2));

		// call the method under test
		FluentModuleRoleContentAccess result = fluent.findContentAccess("TestDocument", "binding1");

		// verify the result
		assertThat(result, is(notNullValue()));
		assertThat(result.get().getDocumentName(), is("TestDocument"));
		assertThat(result.get().getBinding(), is("binding1"));
	}

	@Test
	@SuppressWarnings("boxing")
	public void testRemoveSingularAccess() {
		// setup the test data
		FluentModuleRoleSingularAccess access1 = new FluentModuleRoleSingularAccess();
		access1.documentName("TestDocument1");

		FluentModuleRoleSingularAccess access2 = new FluentModuleRoleSingularAccess();
		access2.documentName("TestDocument2");

		fluent.addSingularAccess(access1);
		fluent.addSingularAccess(access2);

		// validate the test data
		assertThat(fluent.get().getAccesses().size(), is(2));

		// call the method under test
		fluent.removeSingularAccess("TestDocument1");

		// verify the result
		assertThat(fluent.get().getAccesses().size(), is(1));
		assertThat(fluent.get().getAccesses(), not(hasItem(access1.get())));
		assertThat(fluent.get().getAccesses(), hasItem(access2.get()));
	}

	@Test
	@SuppressWarnings("boxing")
	public void testRemoveDocumentAggregateAccess() {
		// setup the test data
		FluentModuleRoleDocumentAggregateAccess access1 = new FluentModuleRoleDocumentAggregateAccess();
		access1.documentName("TestDocument1");

		FluentModuleRoleDocumentAggregateAccess access2 = new FluentModuleRoleDocumentAggregateAccess();
		access2.documentName("TestDocument2");

		fluent.addDocumentAggregateAccess(access1);
		fluent.addDocumentAggregateAccess(access2);

		// validate the test data
		assertThat(fluent.get().getAccesses().size(), is(2));

		// call the method under test
		fluent.removeDocumentAggregateAccess("TestDocument1");

		// verify the result
		assertThat(fluent.get().getAccesses().size(), is(1));
		assertThat(fluent.get().getAccesses(), not(hasItem(access1.get())));
		assertThat(fluent.get().getAccesses(), hasItem(access2.get()));
	}

	@Test
	@SuppressWarnings("boxing")
	public void testRemoveQueryAggregateAccess() {
		// setup the test data
		FluentModuleRoleQueryAggregateAccess access1 = new FluentModuleRoleQueryAggregateAccess();
		access1.queryName("TestQuery1");

		FluentModuleRoleQueryAggregateAccess access2 = new FluentModuleRoleQueryAggregateAccess();
		access2.queryName("TestQuery2");

		fluent.addQueryAggregateAccess(access1);
		fluent.addQueryAggregateAccess(access2);

		// validate the test data
		assertThat(fluent.get().getAccesses().size(), is(2));

		// call the method under test
		fluent.removeQueryAggregateAccess("TestQuery1");

		// verify the result
		assertThat(fluent.get().getAccesses().size(), is(1));
		assertThat(fluent.get().getAccesses(), not(hasItem(access1.get())));
		assertThat(fluent.get().getAccesses(), hasItem(access2.get()));
	}

	@Test
	@SuppressWarnings("boxing")
	public void testRemoveModelAggregateAccess() {
		// setup the test data
		FluentModuleRoleModelAggregateAccess access1 = new FluentModuleRoleModelAggregateAccess();
		access1.documentName("TestDocument").modelName("TestModel1");

		FluentModuleRoleModelAggregateAccess access2 = new FluentModuleRoleModelAggregateAccess();
		access2.documentName("TestDocument").modelName("TestModel2");

		fluent.addModelAggregateAccess(access1);
		fluent.addModelAggregateAccess(access2);

		// validate the test data
		assertThat(fluent.get().getAccesses().size(), is(2));

		// call the method under test
		fluent.removeModelAggregateAccess("TestDocument", "TestModel1");

		// verify the result
		assertThat(fluent.get().getAccesses().size(), is(1));
		assertThat(fluent.get().getAccesses(), not(hasItem(access1.get())));
		assertThat(fluent.get().getAccesses(), hasItem(access2.get()));
	}

	@Test
	@SuppressWarnings("boxing")
	public void testRemovePreviousCompleteAccess() {
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
		assertThat(fluent.get().getAccesses().size(), is(2));

		// call the method under test
		fluent.removePreviousCompleteAccess("TestDocument1", "binding1");

		// verify the result
		assertThat(fluent.get().getAccesses().size(), is(1));
		assertThat(fluent.get().getAccesses(), not(hasItem(access1.get())));
		assertThat(fluent.get().getAccesses(), hasItem(access2.get()));
	}

	@Test
	@SuppressWarnings("boxing")
	public void testRemoveReportAccess() {
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
		assertThat(fluent.get().getAccesses().size(), is(2));

		// call the method under test
		fluent.removeReportAccess("TestModule1", "TestDocument1", "TestReport1");

		// verify the result
		assertThat(fluent.get().getAccesses().size(), is(1));
		assertThat(fluent.get().getAccesses(), not(hasItem(access1.get())));
		assertThat(fluent.get().getAccesses(), hasItem(access2.get()));
	}

	@Test
	@SuppressWarnings("boxing")
	public void testRemoveDynamicImageAccess() {
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
		assertThat(fluent.get().getAccesses().size(), is(2));

		// call the method under test
		fluent.removeDynamicImageAccess("TestDocument1", "TestImage1");

		// verify the result
		assertThat(fluent.get().getAccesses().size(), is(1));
		assertThat(fluent.get().getAccesses(), not(hasItem(access1.get())));
		assertThat(fluent.get().getAccesses(), hasItem(access2.get()));
	}
	
	@Test
	@SuppressWarnings("boxing")
	public void testRemoveContentAccess() {
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
		assertThat(fluent.get().getAccesses().size(), is(2));

		// call the method under test
		fluent.removeContentAccess("TestDocument1", "binding1");

		// verify the result
		assertThat(fluent.get().getAccesses().size(), is(1));
		assertThat(fluent.get().getAccesses(), not(hasItem(access1.get())));
		assertThat(fluent.get().getAccesses(), hasItem(access2.get()));
	}
}
