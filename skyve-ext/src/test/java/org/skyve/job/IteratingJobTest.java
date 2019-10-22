package org.skyve.job;

import org.junit.Before;
import org.junit.Test;
import org.mockito.*;
import org.skyve.persistence.Persistence;

import javax.inject.Inject;
import java.util.Arrays;

import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.fail;

@SuppressWarnings("unchecked")
public class IteratingJobTest {

	@Mock
	private Persistence persistence;

	@Spy
	@Inject
	@InjectMocks
	private TestJob testJob;

	@Before
	public void before() {
		MockitoAnnotations.initMocks(this);
	}

	@Test
	public void testNoErrors() throws Exception {
		testJob.setElements(Arrays.asList("element1", "element2", "element3"));
		testJob.execute();

		Mockito.verifyNoMoreInteractions(persistence);
		assertThat(testJob.getNumProcessedElements(), is(3));
		assertThat(testJob.getNumSuccessfulElements(), is(3));
		assertThat(testJob.getNumFailedElements(), is(0));
		assertThat(testJob.getPercentComplete(), is(100));
	}

	/**
	 * Tests that no elements are processed after a failure.
	 */
	@Test
	public void testErrorOnFirstElementWithoutContinue() {
		testJob.setElements(Arrays.asList("exception", "element2", "element3"));

		try {
			testJob.execute();
			fail();
		} catch (Exception e) {
			// We are expecting an exception.
		}

		Mockito.verifyNoMoreInteractions(persistence);
		assertThat(testJob.getNumProcessedElements(), is(1));
		assertThat(testJob.getNumSuccessfulElements(), is(0));
		assertThat(testJob.getNumFailedElements(), is(1));
		assertThat(testJob.getPercentComplete(), is(0));
	}

	/**
	 * Tests that no elements are processed after a failure.
	 */
	@Test
	public void testErrorOnSecondElementWithoutContinue() {
		testJob.setElements(Arrays.asList("element1", "exception", "element3", "element4"));

		try {
			testJob.execute();
			fail();
		} catch (Exception e) {
			// We are expecting an exception.
		}

		Mockito.verifyNoMoreInteractions(persistence);
		assertThat(testJob.getNumProcessedElements(), is(2));
		assertThat(testJob.getNumSuccessfulElements(), is(1));
		assertThat(testJob.getNumFailedElements(), is(1));
		assertThat(testJob.getPercentComplete(), is(25));
	}

	@Test
	public void testCommitEveryElement() throws Exception {
		Mockito.when(testJob.getCommitFrequency()).thenReturn(1);
		testJob.setElements(Arrays.asList("element1", "element2", "element3"));
		testJob.execute();

		Mockito.verify(persistence, Mockito.times(3)).commit(false);
		Mockito.verify(persistence, Mockito.times(3)).begin();
		Mockito.verifyNoMoreInteractions(persistence);
		assertThat(testJob.getNumProcessedElements(), is(3));
		assertThat(testJob.getNumSuccessfulElements(), is(3));
		assertThat(testJob.getNumFlushedElements(), is(3));
		assertThat(testJob.getNumFailedElements(), is(0));
	}

	@Test
	public void testCommitInBatches() throws Exception {
		Mockito.when(testJob.getCommitFrequency()).thenReturn(2);
		testJob.setElements(Arrays.asList("element1", "element2", "element3", "element4"));
		testJob.execute();

		Mockito.verify(persistence, Mockito.times(2)).commit(false);
		Mockito.verify(persistence, Mockito.times(2)).begin();
		Mockito.verifyNoMoreInteractions(persistence);
		assertThat(testJob.getNumProcessedElements(), is(4));
		assertThat(testJob.getNumSuccessfulElements(), is(4));
		assertThat(testJob.getNumFlushedElements(), is(4));
		assertThat(testJob.getNumFailedElements(), is(0));
	}

	@Test
	public void testCommitInBatchesWithErrorInTheMiddleOfTheBatch() {
		Mockito.when(testJob.getCommitFrequency()).thenReturn(3);
		testJob.setElements(Arrays.asList("element1", "element2", "element3", "element4", "exception", "element6"));

		try {
			testJob.execute();
			fail();
		} catch (Exception e) {
			// We are expecting an exception.
		}

		Mockito.verify(persistence, Mockito.times(1)).commit(false);
		Mockito.verify(persistence, Mockito.times(1)).begin();
		Mockito.verifyNoMoreInteractions(persistence);
		assertThat(testJob.getNumProcessedElements(), is(5));
		assertThat(testJob.getNumSuccessfulElements(), is(4));
		assertThat(testJob.getNumFlushedElements(), is(3));
		assertThat(testJob.getNumFailedElements(), is(1));
	}

	@Test
	public void testCommitEveryElementWithErrorOnLastElement() {
		Mockito.when(testJob.getCommitFrequency()).thenReturn(1);
		testJob.setElements(Arrays.asList("element1", "element2", "element3", "exception"));

		try {
			testJob.execute();
			fail();
		} catch (Exception e) {
			// We are expecting an exception.
		}

		Mockito.verify(persistence, Mockito.times(3)).commit(false);
		Mockito.verify(persistence, Mockito.times(3)).begin();
		Mockito.verifyNoMoreInteractions(persistence);
		assertThat(testJob.getNumProcessedElements(), is(4));
		assertThat(testJob.getNumSuccessfulElements(), is(3));
		assertThat(testJob.getNumFailedElements(), is(1));
		assertThat(testJob.getPercentComplete(), is(75));
	}

	@Test
	public void testContinueOnFailureWithErrorOnFirstElement() {
		Mockito.when(testJob.continueOnFailure()).thenReturn(true);
		testJob.setElements(Arrays.asList("exception", "element2", "element3", "element4"));

		try {
			testJob.execute();
			fail();
		} catch (Exception e) {
			// We are expecting an exception.
		}

		Mockito.verifyNoMoreInteractions(persistence);
		assertThat(testJob.getNumProcessedElements(), is(4));
		assertThat(testJob.getNumSuccessfulElements(), is(3));
		assertThat(testJob.getNumFailedElements(), is(1));
		assertThat(testJob.getPercentComplete(), is(100));
	}

	@Test
	public void testContinueOnFailureWithErrorOnLastElement() {
		Mockito.when(testJob.continueOnFailure()).thenReturn(true);
		testJob.setElements(Arrays.asList("element1", "element2", "element3", "exception"));

		try {
			testJob.execute();
			fail();
		} catch (Exception e) {
			// We are expecting an exception.
		}

		Mockito.verifyNoMoreInteractions(persistence);
		assertThat(testJob.getNumProcessedElements(), is(4));
		assertThat(testJob.getNumSuccessfulElements(), is(3));
		assertThat(testJob.getNumFailedElements(), is(1));
		assertThat(testJob.getPercentComplete(), is(100));
	}

	@Test
	public void testCommitEveryElementWithContinueOnFailureAndErrorOnFirstElement() {
		Mockito.when(testJob.getCommitFrequency()).thenReturn(1);
		Mockito.when(testJob.continueOnFailure()).thenReturn(true);
		testJob.setElements(Arrays.asList("exception", "element2", "element3", "element4"));

		try {
			testJob.execute();
			fail();
		} catch (Exception e) {
			// We are expecting an exception.
		}

		Mockito.verify(persistence, Mockito.times(3)).commit(false);
		Mockito.verify(persistence, Mockito.times(4)).begin();
		Mockito.verify(persistence, Mockito.times(1)).rollback();
		Mockito.verifyNoMoreInteractions(persistence);
		assertThat(testJob.getNumProcessedElements(), is(4));
		assertThat(testJob.getNumSuccessfulElements(), is(3));
		assertThat(testJob.getNumFailedElements(), is(1));
		assertThat(testJob.getNumFlushedElements(), is(3));
		assertThat(testJob.getPercentComplete(), is(100));
	}

	@Test
	public void testCommitEveryElementWithContinueOnFailureAndErrorOnLastElement() {
		Mockito.when(testJob.getCommitFrequency()).thenReturn(1);
		Mockito.when(testJob.continueOnFailure()).thenReturn(true);
		testJob.setElements(Arrays.asList("element1", "element2", "element3", "exception"));

		try {
			testJob.execute();
			fail();
		} catch (Exception e) {
			// We are expecting an exception.
		}

		Mockito.verify(persistence, Mockito.times(3)).commit(false);
		Mockito.verify(persistence, Mockito.times(4)).begin();
		Mockito.verify(persistence, Mockito.times(1)).rollback();
		Mockito.verifyNoMoreInteractions(persistence);
		assertThat(testJob.getNumProcessedElements(), is(4));
		assertThat(testJob.getNumSuccessfulElements(), is(3));
		assertThat(testJob.getNumFailedElements(), is(1));
		assertThat(testJob.getNumFlushedElements(), is(3));
		assertThat(testJob.getPercentComplete(), is(100));
	}

	@Test
	public void testCommitInBatchesWithContinueOnFailureAndErrorInTheMiddleOfTheBatch() {
		Mockito.when(testJob.getCommitFrequency()).thenReturn(3);
		Mockito.when(testJob.continueOnFailure()).thenReturn(true);
		testJob.setElements(Arrays.asList("element1", "element2", "element3", "element4", "exception", "element6"));

		try {
			testJob.execute();
			fail();
		} catch (Exception e) {
			// We are expecting an exception.
		}

		Mockito.verify(persistence, Mockito.times(2)).commit(false);
		Mockito.verify(persistence, Mockito.times(3)).begin();
		Mockito.verify(persistence, Mockito.times(1)).rollback();
		Mockito.verifyNoMoreInteractions(persistence);
		assertThat(testJob.getNumProcessedElements(), is(6));
		assertThat(testJob.getNumSuccessfulElements(), is(5));
		assertThat(testJob.getNumFlushedElements(), is(4));
		assertThat(testJob.getNumFailedElements(), is(1));
	}

	@Test
	public void testCommitInBatchesWithContinueOnFailureAndErrorsInMultipleBatches() {
		Mockito.when(testJob.getCommitFrequency()).thenReturn(3);
		Mockito.when(testJob.continueOnFailure()).thenReturn(true);
		testJob.setElements(Arrays.asList("element1", "element2", "element3",
				"element4", "exception", "element6",
				"exception", "element8", "element9",
				"element10", "element11", "exception"));

		try {
			testJob.execute();
			fail();
		} catch (Exception e) {
			// We are expecting an exception.
		}

		Mockito.verify(persistence, Mockito.times(3)).commit(false);
		Mockito.verify(persistence, Mockito.times(6)).begin();
		Mockito.verify(persistence, Mockito.times(3)).rollback();
		Mockito.verifyNoMoreInteractions(persistence);
		assertThat(testJob.getNumProcessedElements(), is(12));
		assertThat(testJob.getNumSuccessfulElements(), is(9));
		assertThat(testJob.getNumFlushedElements(), is(6));
		assertThat(testJob.getNumFailedElements(), is(3));
	}
}
