package org.skyve.metadata.module.fluent;

import static org.hamcrest.CoreMatchers.hasItem;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.not;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import org.junit.Before;
import org.junit.Test;
import org.skyve.impl.metadata.repository.view.access.ViewUserAccessesMetaData;
import org.skyve.metadata.view.fluent.FluentView;
import org.skyve.metadata.view.fluent.FluentViewDocumentAggregateAccess;
import org.skyve.metadata.view.fluent.FluentViewModelAggregateAccess;
import org.skyve.metadata.view.fluent.FluentViewPreviousCompleteAccess;
import org.skyve.metadata.view.fluent.FluentViewQueryAggregateAccess;
import org.skyve.metadata.view.fluent.FluentViewSingularAccess;

public class FluentViewTest {

	private FluentView fluent;

	@Before
	public void setup() throws Exception {
		fluent = new FluentView();
		fluent.get().setAccesses(new ViewUserAccessesMetaData());
	}
	
	@SuppressWarnings("boxing")
	@Test
	public void testAddDocumentAggregateAccess() {
		// setup the test data
		FluentViewDocumentAggregateAccess access = new FluentViewDocumentAggregateAccess();
		access.documentName("TestDocument1");

		// validate the test data
		assertThat(fluent.get().getAccesses().getAccesses().size(), is(0));
		assertThat(fluent.get().getAccesses().getAccesses().size(), is(0));
		// call the method under test
		fluent.addDocumentAggregateAccess(access);

		// verify the result
		assertThat(fluent.get().getAccesses().getAccesses().size(), is(1));
	}
	
	@SuppressWarnings("boxing")
	@Test
	public void testAddQueryAggregateAccess() {
		// setup the test data
		FluentViewQueryAggregateAccess access = new FluentViewQueryAggregateAccess();
		access.queryName("TestQuery1");

		// validate the test data
		assertThat(fluent.get().getAccesses().getAccesses().size(), is(0));

		// call the method under test
		fluent.addQueryAggregateAccess(access);

		// verify the result
		assertThat(fluent.get().getAccesses().getAccesses().size(), is(1));
	}

	@SuppressWarnings("boxing")
	@Test
	public void testAddModelAggregateAccess() {
		// setup the test data
		FluentViewModelAggregateAccess access = new FluentViewModelAggregateAccess();
		access.modelName("TestModel1");

		// validate the test data
		assertThat(fluent.get().getAccesses().getAccesses().size(), is(0));

		// call the method under test
		fluent.addModelAggregateAccess(access);

		// verify the result
		assertThat(fluent.get().getAccesses().getAccesses().size(), is(1));
	}

	@SuppressWarnings("boxing")
	@Test
	public void testAddSingularAccess() {
		// setup the test data
		FluentViewSingularAccess access = new FluentViewSingularAccess();
		access.documentName("TestDocument1");

		// validate the test data
		assertThat(fluent.get().getAccesses().getAccesses().size(), is(0));

		// call the method under test
		fluent.addSingularAccess(access);

		// verify the result
		assertThat(fluent.get().getAccesses().getAccesses().size(), is(1));
	}

	@SuppressWarnings("boxing")
	@Test
	public void testAddPreviousCompleteAccess() {
		// setup the test data
		FluentViewPreviousCompleteAccess access = new FluentViewPreviousCompleteAccess();
		access.binding("Binding1");

		// validate the test data
		assertThat(fluent.get().getAccesses().getAccesses().size(), is(0));

		// call the method under test
		fluent.addPreviousCompleteAccess(access);

		// verify the result
		assertThat(fluent.get().getAccesses().getAccesses().size(), is(1));
	}

	@SuppressWarnings("boxing")
	@Test
	public void testClearAccesses() {
		// setup the test data
		FluentViewDocumentAggregateAccess access1 = new FluentViewDocumentAggregateAccess();
		access1.documentName("TestDocument1");

		FluentViewModelAggregateAccess access2 = new FluentViewModelAggregateAccess();
		access2.modelName("TestModel1");

		fluent.addDocumentAggregateAccess(access1);
		fluent.addModelAggregateAccess(access2);

		// validate the test data
		assertThat(fluent.get().getAccesses().getAccesses().size(), is(2));

		// call the method under test
		fluent.clearAccesses();

		// verify the result
		assertThat(fluent.get().getAccesses().getAccesses().size(), is(0));
	}

	@SuppressWarnings("boxing")
	@Test
	public void testFindDocumentAggregateAccess() {
		// setup the test data
		FluentViewDocumentAggregateAccess access1 = new FluentViewDocumentAggregateAccess();
		access1.documentName("TestDocument1");

		FluentViewDocumentAggregateAccess access2 = new FluentViewDocumentAggregateAccess();
		access2.documentName("TestDocument2");
		
		fluent.addDocumentAggregateAccess(access1);
		fluent.addDocumentAggregateAccess(access2);

		// validate the test data
		assertThat(fluent.get().getAccesses().getAccesses().size(), is(2));

		// call the method under test
		FluentViewDocumentAggregateAccess result = fluent.findDocumentAggregateAccess("TestDocument1");

		// verify the result
		assertThat(result, is(notNullValue()));
		assertThat(result.get().getDocumentName(), is("TestDocument1"));
	}

	@SuppressWarnings("boxing")
	@Test
	public void testFindModelAggregateAccess() {
		// setup the test data
		FluentViewModelAggregateAccess access1 = new FluentViewModelAggregateAccess();
		access1.modelName("TestModel1");

		FluentViewModelAggregateAccess access2 = new FluentViewModelAggregateAccess();
		access2.modelName("TestModel2");

		fluent.addModelAggregateAccess(access1);
		fluent.addModelAggregateAccess(access2);

		// validate the test data
		assertThat(fluent.get().getAccesses().getAccesses().size(), is(2));

		// call the method under test
		FluentViewModelAggregateAccess result = fluent.findModelAggregateAccess("TestModel1");

		// verify the result
		assertThat(result, is(notNullValue()));
		assertThat(result.get().getModelName(), is("TestModel1"));
	}

	@SuppressWarnings("boxing")
	@Test
	public void testFindPreviousCompleteAccess() {
		// setup the test data
		FluentViewPreviousCompleteAccess access1 = new FluentViewPreviousCompleteAccess();
		access1.binding("TestBinding1");

		FluentViewPreviousCompleteAccess access2 = new FluentViewPreviousCompleteAccess();
		access2.binding("TestBinding2");

		fluent.addPreviousCompleteAccess(access1);
		fluent.addPreviousCompleteAccess(access2);

		// validate the test data
		assertThat(fluent.get().getAccesses().getAccesses().size(), is(2));

		// call the method under test
		FluentViewPreviousCompleteAccess result = fluent.findPreviousCompleteAccess("TestBinding1");

		// verify the result
		assertThat(result, is(notNullValue()));
		assertThat(result.get().getBinding(), is("TestBinding1"));
	}

	@SuppressWarnings("boxing")
	@Test
	public void testFindQueryAggregateAccess() {
		// setup the test data
		FluentViewQueryAggregateAccess access1 = new FluentViewQueryAggregateAccess();
		access1.queryName("TestQuery1");

		FluentViewQueryAggregateAccess access2 = new FluentViewQueryAggregateAccess();
		access2.queryName("TestQuery2");

		fluent.addQueryAggregateAccess(access1);
		fluent.addQueryAggregateAccess(access2);

		// validate the test data
		assertThat(fluent.get().getAccesses().getAccesses().size(), is(2));

		// call the method under test
		FluentViewQueryAggregateAccess result = fluent.findQueryAggregateAccess("TestQuery1");

		// verify the result
		assertThat(result, is(notNullValue()));
		assertThat(result.get().getQueryName(), is("TestQuery1"));
	}

	@SuppressWarnings("boxing")
	@Test
	public void testFindSingularAccess() {
		// setup the test data
		FluentViewSingularAccess access1 = new FluentViewSingularAccess();
		access1.documentName("TestDocument1");

		FluentViewSingularAccess access2 = new FluentViewSingularAccess();
		access2.documentName("TestDocument2");

		fluent.addSingularAccess(access1);
		fluent.addSingularAccess(access2);

		// validate the test data
		assertThat(fluent.get().getAccesses().getAccesses().size(), is(2));

		// call the method under test
		FluentViewSingularAccess result = fluent.findSingularAccess("TestDocument1");

		// verify the result
		assertThat(result, is(notNullValue()));
		assertThat(result.get().getDocumentName(), is("TestDocument1"));
	}

	@SuppressWarnings("boxing")
	@Test
	public void testRemoveDocumentAggregateAccess() {
		// setup the test data
		FluentViewDocumentAggregateAccess access1 = new FluentViewDocumentAggregateAccess();
		access1.documentName("TestDocument1");

		FluentViewDocumentAggregateAccess access2 = new FluentViewDocumentAggregateAccess();
		access2.documentName("TestDocument2");

		fluent.addDocumentAggregateAccess(access1);
		fluent.addDocumentAggregateAccess(access2);

		// validate the test data
		assertThat(fluent.get().getAccesses().getAccesses().size(), is(2));

		// call the method under test
		fluent.removeDocumentAggregateAccess("TestDocument1");

		// verify the result
		assertThat(fluent.get().getAccesses().getAccesses().size(), is(1));
		assertThat(fluent.get().getAccesses().getAccesses(), not(hasItem(access1.get())));
		assertThat(fluent.get().getAccesses().getAccesses(), hasItem(access2.get()));
	}

	@SuppressWarnings("boxing")
	@Test
	public void testRemoveModelAggregateAccess() {
		// setup the test data
		FluentViewModelAggregateAccess access1 = new FluentViewModelAggregateAccess();
		access1.modelName("TestModel1");

		FluentViewModelAggregateAccess access2 = new FluentViewModelAggregateAccess();
		access2.modelName("TestModel2");

		fluent.addModelAggregateAccess(access1);
		fluent.addModelAggregateAccess(access2);

		// validate the test data
		assertThat(fluent.get().getAccesses().getAccesses().size(), is(2));

		// call the method under test
		fluent.removeModelAggregateAccess("TestModel1");

		// verify the result
		assertThat(fluent.get().getAccesses().getAccesses().size(), is(1));
		assertThat(fluent.get().getAccesses().getAccesses(), not(hasItem(access1.get())));
		assertThat(fluent.get().getAccesses().getAccesses(), hasItem(access2.get()));
	}

	@SuppressWarnings("boxing")
	@Test
	public void testRemovePreviousCompleteAccess() {
		// setup the test data
		FluentViewPreviousCompleteAccess access1 = new FluentViewPreviousCompleteAccess();
		access1.binding("TestBinding1");

		FluentViewPreviousCompleteAccess access2 = new FluentViewPreviousCompleteAccess();
		access2.binding("TestBinding2");

		fluent.addPreviousCompleteAccess(access1);
		fluent.addPreviousCompleteAccess(access2);

		// validate the test data
		assertThat(fluent.get().getAccesses().getAccesses().size(), is(2));

		// call the method under test
		fluent.removePreviousCompleteAccess("TestBinding1");

		// verify the result
		assertThat(fluent.get().getAccesses().getAccesses().size(), is(1));
		assertThat(fluent.get().getAccesses().getAccesses(), not(hasItem(access1.get())));
		assertThat(fluent.get().getAccesses().getAccesses(), hasItem(access2.get()));
	}

	@SuppressWarnings("boxing")
	@Test
	public void testRemoveQueryAggregateAccess() {
		// setup the test data
		FluentViewQueryAggregateAccess access1 = new FluentViewQueryAggregateAccess();
		access1.queryName("TestQuery1");

		FluentViewQueryAggregateAccess access2 = new FluentViewQueryAggregateAccess();
		access2.queryName("TestQuery2");

		fluent.addQueryAggregateAccess(access1);
		fluent.addQueryAggregateAccess(access2);

		// validate the test data
		assertThat(fluent.get().getAccesses().getAccesses().size(), is(2));

		// call the method under test
		fluent.removeQueryAggregateAccess("TestQuery1");

		// verify the result
		assertThat(fluent.get().getAccesses().getAccesses().size(), is(1));
		assertThat(fluent.get().getAccesses().getAccesses(), not(hasItem(access1.get())));
		assertThat(fluent.get().getAccesses().getAccesses(), hasItem(access2.get()));
	}

	@SuppressWarnings("boxing")
	@Test
	public void testRemoveSingularAccess() {
		// setup the test data
		FluentViewSingularAccess access1 = new FluentViewSingularAccess();
		access1.documentName("TestDocument1");

		FluentViewSingularAccess access2 = new FluentViewSingularAccess();
		access2.documentName("TestDocument2");

		fluent.addSingularAccess(access1);
		fluent.addSingularAccess(access2);

		// validate the test data
		assertThat(fluent.get().getAccesses().getAccesses().size(), is(2));

		// call the method under test
		fluent.removeSingularAccess("TestDocument1");

		// verify the result
		assertThat(fluent.get().getAccesses().getAccesses().size(), is(1));
		assertThat(fluent.get().getAccesses().getAccesses(), not(hasItem(access1.get())));
		assertThat(fluent.get().getAccesses().getAccesses(), hasItem(access2.get()));
	}
}
