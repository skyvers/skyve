package org.skyve.metadata.module.fluent;

import static org.hamcrest.CoreMatchers.hasItem;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.not;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.repository.view.access.ViewUserAccessesMetaData;
import org.skyve.metadata.view.fluent.FluentView;
import org.skyve.metadata.view.fluent.FluentViewContentAccess;
import org.skyve.metadata.view.fluent.FluentViewDocumentAggregateAccess;
import org.skyve.metadata.view.fluent.FluentViewDynamicImageAccess;
import org.skyve.metadata.view.fluent.FluentViewModelAggregateAccess;
import org.skyve.metadata.view.fluent.FluentViewPreviousCompleteAccess;
import org.skyve.metadata.view.fluent.FluentViewQueryAggregateAccess;
import org.skyve.metadata.view.fluent.FluentViewReportAccess;
import org.skyve.metadata.view.fluent.FluentViewSingularAccess;

public class FluentViewTest {

	private FluentView fluent;

	@BeforeEach
	public void setup() throws Exception {
		fluent = new FluentView();
		fluent.get().setAccesses(new ViewUserAccessesMetaData());
	}
	
	@Test
	@SuppressWarnings("boxing")
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

	@Test
	@SuppressWarnings("boxing")
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
	
	@Test
	@SuppressWarnings("boxing")
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

	@Test
	@SuppressWarnings("boxing")
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

	@Test
	@SuppressWarnings("boxing")
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

	@Test
	@SuppressWarnings("boxing")
	public void testAddReportAccess() {
		// setup the test data
		FluentViewReportAccess access = new FluentViewReportAccess();
		access.moduleName("TestModule");
		access.documentName("TestDocument");
		access.reportName("TestReport");

		// validate the test data
		assertThat(fluent.get().getAccesses().getAccesses().size(), is(0));

		// call the method under test
		fluent.addReportAccess(access);

		// verify the result
		assertThat(fluent.get().getAccesses().getAccesses().size(), is(1));
	}

	@Test
	@SuppressWarnings("boxing")
	public void testAddDynamicImageAccess() {
		// setup the test data
		FluentViewDynamicImageAccess access = new FluentViewDynamicImageAccess();
		access.imageName("TestImage");

		// validate the test data
		assertThat(fluent.get().getAccesses().getAccesses().size(), is(0));

		// call the method under test
		fluent.addDynamicImageAccess(access);

		// verify the result
		assertThat(fluent.get().getAccesses().getAccesses().size(), is(1));
	}

	@Test
	@SuppressWarnings("boxing")
	public void testAddContentAccess() {
		// setup the test data
		FluentViewContentAccess access = new FluentViewContentAccess();
		access.binding("binding");

		// validate the test data
		assertThat(fluent.get().getAccesses().getAccesses().size(), is(0));

		// call the method under test
		fluent.addContentAccess(access);

		// verify the result
		assertThat(fluent.get().getAccesses().getAccesses().size(), is(1));
	}

	@Test
	@SuppressWarnings("boxing")
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

	@Test
	@SuppressWarnings("boxing")
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

	@Test
	@SuppressWarnings("boxing")
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

	@Test
	@SuppressWarnings("boxing")
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

	@Test
	@SuppressWarnings("boxing")
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

	@Test
	@SuppressWarnings("boxing")
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

	@Test
	@SuppressWarnings("boxing")
	public void testFindReportAccess() {
		// setup the test data
		FluentViewReportAccess access1 = new FluentViewReportAccess();
		access1.moduleName("TestModule1");
		access1.documentName("TestDocument1");
		access1.reportName("TestReport1");

		FluentViewReportAccess access2 = new FluentViewReportAccess();
		access2.moduleName("TestModule2");
		access2.documentName("TestDocument2");
		access2.reportName("TestReport2");

		fluent.addReportAccess(access1);
		fluent.addReportAccess(access2);

		// validate the test data
		assertThat(fluent.get().getAccesses().getAccesses().size(), is(2));

		// call the method under test
		FluentViewReportAccess result = fluent.findReportAccess("TestModule1", "TestDocument1", "TestReport1");

		// verify the result
		assertThat(result, is(notNullValue()));
		assertThat(result.get().getModuleName(), is("TestModule1"));
		assertThat(result.get().getDocumentName(), is("TestDocument1"));
		assertThat(result.get().getReportName(), is("TestReport1"));
	}

	@Test
	@SuppressWarnings("boxing")
	public void testFindDynamicImageAccess() {
		// setup the test data
		FluentViewDynamicImageAccess access1 = new FluentViewDynamicImageAccess();
		access1.imageName("TestImage1");

		FluentViewDynamicImageAccess access2 = new FluentViewDynamicImageAccess();
		access2.imageName("TestImage2");

		fluent.addDynamicImageAccess(access1);
		fluent.addDynamicImageAccess(access2);

		// validate the test data
		assertThat(fluent.get().getAccesses().getAccesses().size(), is(2));

		// call the method under test
		FluentViewDynamicImageAccess result = fluent.findDynamicImageAccess("TestImage1");

		// verify the result
		assertThat(result, is(notNullValue()));
		assertThat(result.get().getImageName(), is("TestImage1"));
	}

	@Test
	@SuppressWarnings("boxing")
	public void testFindContentAccess() {
		// setup the test data
		FluentViewContentAccess access1 = new FluentViewContentAccess();
		access1.binding("binding1");

		FluentViewContentAccess access2 = new FluentViewContentAccess();
		access2.binding("binding2");

		fluent.addContentAccess(access1);
		fluent.addContentAccess(access2);

		// validate the test data
		assertThat(fluent.get().getAccesses().getAccesses().size(), is(2));

		// call the method under test
		FluentViewContentAccess result = fluent.findContentAccess("binding1");

		// verify the result
		assertThat(result, is(notNullValue()));
		assertThat(result.get().getBinding(), is("binding1"));
	}

	@Test
	@SuppressWarnings("boxing")
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

	@Test
	@SuppressWarnings("boxing")
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

	@Test
	@SuppressWarnings("boxing")
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

	@Test
	@SuppressWarnings("boxing")
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

	@Test
	@SuppressWarnings("boxing")
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

	@Test
	@SuppressWarnings("boxing")
	public void testRemoveReportAccess() {
		// setup the test data
		FluentViewReportAccess access1 = new FluentViewReportAccess();
		access1.moduleName("TestModule1");
		access1.documentName("TestDocument1");
		access1.reportName("TestReport1");

		FluentViewReportAccess access2 = new FluentViewReportAccess();
		access2.moduleName("TestModule2");
		access2.documentName("TestDocument2");
		access2.reportName("TestReport2");

		fluent.addReportAccess(access1);
		fluent.addReportAccess(access2);

		// validate the test data
		assertThat(fluent.get().getAccesses().getAccesses().size(), is(2));

		// call the method under test
		fluent.removeReportAccess("TestModule1", "TestDocument1", "TestReport1");

		// verify the result
		assertThat(fluent.get().getAccesses().getAccesses().size(), is(1));
		assertThat(fluent.get().getAccesses().getAccesses(), not(hasItem(access1.get())));
		assertThat(fluent.get().getAccesses().getAccesses(), hasItem(access2.get()));
	}

	@Test
	@SuppressWarnings("boxing")
	public void testRemoveDynamicImageAccess() {
		// setup the test data
		FluentViewDynamicImageAccess access1 = new FluentViewDynamicImageAccess();
		access1.imageName("TestImage1");

		FluentViewDynamicImageAccess access2 = new FluentViewDynamicImageAccess();
		access2.imageName("TestImage2");

		fluent.addDynamicImageAccess(access1);
		fluent.addDynamicImageAccess(access2);

		// validate the test data
		assertThat(fluent.get().getAccesses().getAccesses().size(), is(2));

		// call the method under test
		fluent.removeDynamicImageAccess("TestImage1");

		// verify the result
		assertThat(fluent.get().getAccesses().getAccesses().size(), is(1));
		assertThat(fluent.get().getAccesses().getAccesses(), not(hasItem(access1.get())));
		assertThat(fluent.get().getAccesses().getAccesses(), hasItem(access2.get()));
	}

	@Test
	@SuppressWarnings("boxing")
	public void testRemoveContentAccess() {
		// setup the test data
		FluentViewContentAccess access1 = new FluentViewContentAccess();
		access1.binding("binding1");

		FluentViewContentAccess access2 = new FluentViewContentAccess();
		access2.binding("binding2");

		fluent.addContentAccess(access1);
		fluent.addContentAccess(access2);

		// validate the test data
		assertThat(fluent.get().getAccesses().getAccesses().size(), is(2));

		// call the method under test
		fluent.removeContentAccess("binding1");

		// verify the result
		assertThat(fluent.get().getAccesses().getAccesses().size(), is(1));
		assertThat(fluent.get().getAccesses().getAccesses(), not(hasItem(access1.get())));
		assertThat(fluent.get().getAccesses().getAccesses(), hasItem(access2.get()));
	}
}
