package org.skyve.job;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;

import java.util.Arrays;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.Spy;
import org.skyve.persistence.Persistence;

import jakarta.inject.Inject;

@SuppressWarnings("boxing")
class IteratingJobTest {

	@Mock
	private Persistence persistence;

	@Spy
	@Inject
	@InjectMocks
	private TestJob testJob;
	
	@BeforeEach
	@SuppressWarnings("deprecation")
	void before() {
		MockitoAnnotations.initMocks(this);
	}

	@Test
	void testNoErrors() throws Exception {
		testJob.setElements(Arrays.asList("element1", "element2", "element3"));
		testJob.execute();

		verifyNoMoreInteractions(persistence);
		assertEquals(3, testJob.getNumProcessedElements());
		assertEquals(3, testJob.getNumSuccessfulElements());
		assertEquals(0, testJob.getNumFailedElements());
		assertEquals(100, testJob.getPercentComplete());
	}

	/**
	 * Tests that no elements are processed after a failure.
	 */
	@Test
	void testErrorOnFirstElementWithoutContinue() {
		testJob.setElements(Arrays.asList("exception", "element2", "element3"));

		assertThrows(Exception.class, testJob::execute);

		verifyNoMoreInteractions(persistence);
		assertEquals(1, testJob.getNumProcessedElements());
		assertEquals(0, testJob.getNumSuccessfulElements());
		assertEquals(1, testJob.getNumFailedElements());
		assertEquals(0, testJob.getPercentComplete());
	}

	/**
	 * Tests that no elements are processed after a failure.
	 */
	@Test
	void testErrorOnSecondElementWithoutContinue() {
		testJob.setElements(Arrays.asList("element1", "exception", "element3", "element4"));

		assertThrows(Exception.class, testJob::execute);

		verifyNoMoreInteractions(persistence);
		assertEquals(2, testJob.getNumProcessedElements());
		assertEquals(1, testJob.getNumSuccessfulElements());
		assertEquals(1, testJob.getNumFailedElements());
		assertEquals(25, testJob.getPercentComplete());
	}

	@Test
	void testCommitEveryElement() throws Exception {
		when(testJob.getCommitFrequency()).thenReturn(1);
		testJob.setElements(Arrays.asList("element1", "element2", "element3"));
		testJob.execute();

		verify(persistence, times(3)).commit(false);
		verify(persistence, times(3)).begin();
		verifyNoMoreInteractions(persistence);
		assertEquals(3, testJob.getNumProcessedElements());
		assertEquals(3, testJob.getNumSuccessfulElements());
		assertEquals(3, testJob.getNumFlushedElements());
		assertEquals(0, testJob.getNumFailedElements());
	}

	@Test
	void testCommitInBatches() throws Exception {
		when(testJob.getCommitFrequency()).thenReturn(2);
		testJob.setElements(Arrays.asList("element1", "element2", "element3", "element4"));
		testJob.execute();

		verify(persistence, times(2)).commit(false);
		verify(persistence, times(2)).begin();
		verifyNoMoreInteractions(persistence);
		assertEquals(4, testJob.getNumProcessedElements());
		assertEquals(4, testJob.getNumSuccessfulElements());
		assertEquals(4, testJob.getNumFlushedElements());
		assertEquals(0, testJob.getNumFailedElements());
	}

	@Test
	void testCommitInBatchesWithErrorInTheMiddleOfTheBatch() {
		when(testJob.getCommitFrequency()).thenReturn(3);
		testJob.setElements(Arrays.asList("element1", "element2", "element3", "element4", "exception", "element6"));

		assertThrows(Exception.class, testJob::execute);

		verify(persistence, times(1)).commit(false);
		verify(persistence, times(1)).begin();
		verifyNoMoreInteractions(persistence);
		assertEquals(5, testJob.getNumProcessedElements());
		assertEquals(4, testJob.getNumSuccessfulElements());
		assertEquals(3, testJob.getNumFlushedElements());
		assertEquals(1, testJob.getNumFailedElements());
	}

	@Test
	void testCommitEveryElementWithErrorOnLastElement() {
		when(testJob.getCommitFrequency()).thenReturn(1);
		testJob.setElements(Arrays.asList("element1", "element2", "element3", "exception"));

		assertThrows(Exception.class, testJob::execute);

		verify(persistence, times(3)).commit(false);
		verify(persistence, times(3)).begin();
		verifyNoMoreInteractions(persistence);
		assertEquals(4, testJob.getNumProcessedElements());
		assertEquals(3, testJob.getNumSuccessfulElements());
		assertEquals(1, testJob.getNumFailedElements());
		assertEquals(75, testJob.getPercentComplete());
	}

	@Test
	void testContinueOnFailureWithErrorOnFirstElement() {
		when(testJob.continueOnFailure()).thenReturn(Boolean.TRUE);
		testJob.setElements(Arrays.asList("exception", "element2", "element3", "element4"));

		assertThrows(Exception.class, testJob::execute);

		verifyNoMoreInteractions(persistence);
		assertEquals(4, testJob.getNumProcessedElements());
		assertEquals(3, testJob.getNumSuccessfulElements());
		assertEquals(1, testJob.getNumFailedElements());
		assertEquals(100, testJob.getPercentComplete());
	}

	@Test
	void testContinueOnFailureWithErrorOnLastElement() {
		when(testJob.continueOnFailure()).thenReturn(Boolean.TRUE);
		testJob.setElements(Arrays.asList("element1", "element2", "element3", "exception"));

		assertThrows(Exception.class, testJob::execute);

		verifyNoMoreInteractions(persistence);
		assertEquals(4, testJob.getNumProcessedElements());
		assertEquals(3, testJob.getNumSuccessfulElements());
		assertEquals(1, testJob.getNumFailedElements());
		assertEquals(100, testJob.getPercentComplete());
	}

	@Test
	void testCommitEveryElementWithContinueOnFailureAndErrorOnFirstElement() {
		when(testJob.getCommitFrequency()).thenReturn(1);
		when(testJob.continueOnFailure()).thenReturn(Boolean.TRUE);
		testJob.setElements(Arrays.asList("exception", "element2", "element3", "element4"));

		assertThrows(Exception.class, testJob::execute);

		verify(persistence, times(3)).commit(false);
		verify(persistence, times(4)).begin();
		verify(persistence, times(1)).rollback();
		verifyNoMoreInteractions(persistence);
		assertEquals(4, testJob.getNumProcessedElements());
		assertEquals(3, testJob.getNumSuccessfulElements());
		assertEquals(1, testJob.getNumFailedElements());
		assertEquals(3, testJob.getNumFlushedElements());
		assertEquals(100, testJob.getPercentComplete());
	}

	@Test
	void testCommitEveryElementWithContinueOnFailureAndErrorOnLastElement() {
		when(testJob.getCommitFrequency()).thenReturn(1);
		when(testJob.continueOnFailure()).thenReturn(Boolean.TRUE);
		testJob.setElements(Arrays.asList("element1", "element2", "element3", "exception"));

		assertThrows(Exception.class, testJob::execute);

		verify(persistence, times(3)).commit(false);
		verify(persistence, times(4)).begin();
		verify(persistence, times(1)).rollback();
		verifyNoMoreInteractions(persistence);
		assertEquals(4, testJob.getNumProcessedElements());
		assertEquals(3, testJob.getNumSuccessfulElements());
		assertEquals(1, testJob.getNumFailedElements());
		assertEquals(3, testJob.getNumFlushedElements());
		assertEquals(100, testJob.getPercentComplete());
	}

	@Test
	void testCommitInBatchesWithContinueOnFailureAndErrorInTheMiddleOfTheBatch() {
		when(testJob.getCommitFrequency()).thenReturn(3);
		when(testJob.continueOnFailure()).thenReturn(Boolean.TRUE);
		testJob.setElements(Arrays.asList("element1", "element2", "element3", "element4", "exception", "element6"));

		assertThrows(Exception.class, testJob::execute);

		verify(persistence, times(2)).commit(false);
		verify(persistence, times(3)).begin();
		verify(persistence, times(1)).rollback();
		verifyNoMoreInteractions(persistence);
		assertEquals(6, testJob.getNumProcessedElements());
		assertEquals(5, testJob.getNumSuccessfulElements());
		assertEquals(4, testJob.getNumFlushedElements());
		assertEquals(1, testJob.getNumFailedElements());
	}

	@Test
	void testGetNumRolledBackElementsAfterCommitEveryElementWithFailure() {
		when(testJob.getCommitFrequency()).thenReturn(1);
		when(testJob.continueOnFailure()).thenReturn(Boolean.TRUE);
		testJob.setElements(Arrays.asList("element1", "exception", "element3"));

		assertThrows(Exception.class, testJob::execute);

		assertEquals(1, testJob.getNumRolledBackElements());
	}

	@Test
	void testSetPersistenceExposesFieldForSubclasses() {
		// Verifies that the protected setPersistence setter can be called from within the package.
		testJob.setPersistence(persistence);
		assertEquals(0, testJob.getNumRolledBackElements());
	}

	@Test
	void testCommitInBatchesWithContinueOnFailureAndErrorsInMultipleBatches() {
		when(testJob.getCommitFrequency()).thenReturn(3);
		when(testJob.continueOnFailure()).thenReturn(Boolean.TRUE);
		testJob.setElements(Arrays.asList("element1", "element2", "element3",
				"element4", "exception", "element6",
				"exception", "element8", "element9",
				"element10", "element11", "exception"));

		assertThrows(Exception.class, testJob::execute);

		verify(persistence, times(3)).commit(false);
		verify(persistence, times(6)).begin();
		verify(persistence, times(3)).rollback();
		verifyNoMoreInteractions(persistence);
		assertEquals(12, testJob.getNumProcessedElements());
		assertEquals(9, testJob.getNumSuccessfulElements());
		assertEquals(6, testJob.getNumFlushedElements());
		assertEquals(3, testJob.getNumFailedElements());
	}

	@Test
	void testJobCancelledMidwayStopsProcessing() throws Exception {
		testJob.setElements(Arrays.asList("element1", "element2", "element3"));
		// First call to isCancelled returns false (processes element1), second returns true (cancels before element2)
		when(testJob.isCancelled()).thenReturn(false, true);
		testJob.execute();
		assertEquals(1, testJob.getNumProcessedElements());
		assertEquals(1, testJob.getNumSuccessfulElements());
	}
}
