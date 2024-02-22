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

		FluentModuleRolePreviousCompleteAccess access2 = new FluentModuleRolePreviousCompleteAccess();
		access2.documentName("TestDocument2");

		fluent.addPreviousCompleteAccess(access1);
		fluent.addPreviousCompleteAccess(access2);

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
}
