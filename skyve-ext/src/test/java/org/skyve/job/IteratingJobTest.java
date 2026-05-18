package org.skyve.job;

import static org.junit.Assert.fail;
import static org.junit.Assert.assertEquals;

import java.util.Arrays;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.mockito.Spy;
import org.skyve.persistence.Persistence;

import jakarta.inject.Inject;

public class IteratingJobTest {

	@Mock
	private Persistence persistence;

	@Spy
	@Inject
	@InjectMocks
	private TestJob testJob;

	private AutoCloseable closeable;
	
	@Before
	public void before() {
		closeable = MockitoAnnotations.openMocks(this);
	}
	
	@After
	public void after() throws Exception {
		if (closeable != null) {
			closeable.close();
		}
	}

	@Test
	@SuppressWarnings("boxing")
	public void testNoErrors() throws Exception {
		testJob.setElements(Arrays.asList("element1", "element2", "element3"));
		testJob.execute();

		Mockito.verifyNoMoreInteractions(persistence);
		assertEquals(3, testJob.getNumProcessedElements());
		assertEquals(3, testJob.getNumSuccessfulElements());
		assertEquals(0, testJob.getNumFailedElements());
		assertEquals(100, testJob.getPercentComplete());
	}

	/**
	 * Tests that no elements are processed after a failure.
	 */
	@Test
	@SuppressWarnings("boxing")
	public void testErrorOnFirstElementWithoutContinue() {
		testJob.setElements(Arrays.asList("exception", "element2", "element3"));

		try {
			testJob.execute();
			fail();
		} catch (@SuppressWarnings("unused") Exception e) {
			// We are expecting an exception.
		}

		Mockito.verifyNoMoreInteractions(persistence);
		assertEquals(1, testJob.getNumProcessedElements());
		assertEquals(0, testJob.getNumSuccessfulElements());
		assertEquals(1, testJob.getNumFailedElements());
		assertEquals(0, testJob.getPercentComplete());
	}

	/**
	 * Tests that no elements are processed after a failure.
	 */
	@Test
	@SuppressWarnings("boxing")
	public void testErrorOnSecondElementWithoutContinue() {
		testJob.setElements(Arrays.asList("element1", "exception", "element3", "element4"));

		try {
			testJob.execute();
			fail();
		} catch (@SuppressWarnings("unused") Exception e) {
			// We are expecting an exception.
		}

		Mockito.verifyNoMoreInteractions(persistence);
		assertEquals(2, testJob.getNumProcessedElements());
		assertEquals(1, testJob.getNumSuccessfulElements());
		assertEquals(1, testJob.getNumFailedElements());
		assertEquals(25, testJob.getPercentComplete());
	}

	@Test
	@SuppressWarnings("boxing")
	public void testCommitEveryElement() throws Exception {
		Mockito.when(testJob.getCommitFrequency()).thenReturn(1);
		testJob.setElements(Arrays.asList("element1", "element2", "element3"));
		testJob.execute();

		Mockito.verify(persistence, Mockito.times(3)).commit(false);
		Mockito.verify(persistence, Mockito.times(3)).begin();
		Mockito.verifyNoMoreInteractions(persistence);
		assertEquals(3, testJob.getNumProcessedElements());
		assertEquals(3, testJob.getNumSuccessfulElements());
		assertEquals(3, testJob.getNumFlushedElements());
		assertEquals(0, testJob.getNumFailedElements());
	}

	@Test
	@SuppressWarnings("boxing")
	public void testCommitInBatches() throws Exception {
		Mockito.when(testJob.getCommitFrequency()).thenReturn(2);
		testJob.setElements(Arrays.asList("element1", "element2", "element3", "element4"));
		testJob.execute();

		Mockito.verify(persistence, Mockito.times(2)).commit(false);
		Mockito.verify(persistence, Mockito.times(2)).begin();
		Mockito.verifyNoMoreInteractions(persistence);
		assertEquals(4, testJob.getNumProcessedElements());
		assertEquals(4, testJob.getNumSuccessfulElements());
		assertEquals(4, testJob.getNumFlushedElements());
		assertEquals(0, testJob.getNumFailedElements());
	}

	@Test
	@SuppressWarnings("boxing")
	public void testCommitInBatchesWithErrorInTheMiddleOfTheBatch() {
		Mockito.when(testJob.getCommitFrequency()).thenReturn(3);
		testJob.setElements(Arrays.asList("element1", "element2", "element3", "element4", "exception", "element6"));

		try {
			testJob.execute();
			fail();
		} catch (@SuppressWarnings("unused") Exception e) {
			// We are expecting an exception.
		}

		Mockito.verify(persistence, Mockito.times(1)).commit(false);
		Mockito.verify(persistence, Mockito.times(1)).begin();
		Mockito.verifyNoMoreInteractions(persistence);
		assertEquals(5, testJob.getNumProcessedElements());
		assertEquals(4, testJob.getNumSuccessfulElements());
		assertEquals(3, testJob.getNumFlushedElements());
		assertEquals(1, testJob.getNumFailedElements());
	}

	@Test
	@SuppressWarnings("boxing")
	public void testCommitEveryElementWithErrorOnLastElement() {
		Mockito.when(testJob.getCommitFrequency()).thenReturn(1);
		testJob.setElements(Arrays.asList("element1", "element2", "element3", "exception"));

		try {
			testJob.execute();
			fail();
		} catch (@SuppressWarnings("unused") Exception e) {
			// We are expecting an exception.
		}

		Mockito.verify(persistence, Mockito.times(3)).commit(false);
		Mockito.verify(persistence, Mockito.times(3)).begin();
		Mockito.verifyNoMoreInteractions(persistence);
		assertEquals(4, testJob.getNumProcessedElements());
		assertEquals(3, testJob.getNumSuccessfulElements());
		assertEquals(1, testJob.getNumFailedElements());
		assertEquals(75, testJob.getPercentComplete());
	}

	@Test
	@SuppressWarnings("boxing")
	public void testContinueOnFailureWithErrorOnFirstElement() {
		Mockito.when(testJob.continueOnFailure()).thenReturn(Boolean.TRUE);
		testJob.setElements(Arrays.asList("exception", "element2", "element3", "element4"));

		try {
			testJob.execute();
			fail();
		} catch (@SuppressWarnings("unused") Exception e) {
			// We are expecting an exception.
		}

		Mockito.verifyNoMoreInteractions(persistence);
		assertEquals(4, testJob.getNumProcessedElements());
		assertEquals(3, testJob.getNumSuccessfulElements());
		assertEquals(1, testJob.getNumFailedElements());
		assertEquals(100, testJob.getPercentComplete());
	}

	@Test
	@SuppressWarnings("boxing")
	public void testContinueOnFailureWithErrorOnLastElement() {
		Mockito.when(testJob.continueOnFailure()).thenReturn(Boolean.TRUE);
		testJob.setElements(Arrays.asList("element1", "element2", "element3", "exception"));

		try {
			testJob.execute();
			fail();
		} catch (@SuppressWarnings("unused") Exception e) {
			// We are expecting an exception.
		}

		Mockito.verifyNoMoreInteractions(persistence);
		assertEquals(4, testJob.getNumProcessedElements());
		assertEquals(3, testJob.getNumSuccessfulElements());
		assertEquals(1, testJob.getNumFailedElements());
		assertEquals(100, testJob.getPercentComplete());
	}

	@Test
	@SuppressWarnings("boxing")
	public void testCommitEveryElementWithContinueOnFailureAndErrorOnFirstElement() {
		Mockito.when(testJob.getCommitFrequency()).thenReturn(1);
		Mockito.when(testJob.continueOnFailure()).thenReturn(Boolean.TRUE);
		testJob.setElements(Arrays.asList("exception", "element2", "element3", "element4"));

		try {
			testJob.execute();
			fail();
		} catch (@SuppressWarnings("unused") Exception e) {
			// We are expecting an exception.
		}

		Mockito.verify(persistence, Mockito.times(3)).commit(false);
		Mockito.verify(persistence, Mockito.times(4)).begin();
		Mockito.verify(persistence, Mockito.times(1)).rollback();
		Mockito.verifyNoMoreInteractions(persistence);
		assertEquals(4, testJob.getNumProcessedElements());
		assertEquals(3, testJob.getNumSuccessfulElements());
		assertEquals(1, testJob.getNumFailedElements());
		assertEquals(3, testJob.getNumFlushedElements());
		assertEquals(100, testJob.getPercentComplete());
	}

	@Test
	@SuppressWarnings("boxing")
	public void testCommitEveryElementWithContinueOnFailureAndErrorOnLastElement() {
		Mockito.when(testJob.getCommitFrequency()).thenReturn(1);
		Mockito.when(testJob.continueOnFailure()).thenReturn(Boolean.TRUE);
		testJob.setElements(Arrays.asList("element1", "element2", "element3", "exception"));

		try {
			testJob.execute();
			fail();
		} catch (@SuppressWarnings("unused") Exception e) {
			// We are expecting an exception.
		}

		Mockito.verify(persistence, Mockito.times(3)).commit(false);
		Mockito.verify(persistence, Mockito.times(4)).begin();
		Mockito.verify(persistence, Mockito.times(1)).rollback();
		Mockito.verifyNoMoreInteractions(persistence);
		assertEquals(4, testJob.getNumProcessedElements());
		assertEquals(3, testJob.getNumSuccessfulElements());
		assertEquals(1, testJob.getNumFailedElements());
		assertEquals(3, testJob.getNumFlushedElements());
		assertEquals(100, testJob.getPercentComplete());
	}

	@Test
	@SuppressWarnings("boxing")
	public void testCommitInBatchesWithContinueOnFailureAndErrorInTheMiddleOfTheBatch() {
		Mockito.when(testJob.getCommitFrequency()).thenReturn(3);
		Mockito.when(testJob.continueOnFailure()).thenReturn(Boolean.TRUE);
		testJob.setElements(Arrays.asList("element1", "element2", "element3", "element4", "exception", "element6"));

		try {
			testJob.execute();
			fail();
		} catch (@SuppressWarnings("unused") Exception e) {
			// We are expecting an exception.
		}

		Mockito.verify(persistence, Mockito.times(2)).commit(false);
		Mockito.verify(persistence, Mockito.times(3)).begin();
		Mockito.verify(persistence, Mockito.times(1)).rollback();
		Mockito.verifyNoMoreInteractions(persistence);
		assertEquals(6, testJob.getNumProcessedElements());
		assertEquals(5, testJob.getNumSuccessfulElements());
		assertEquals(4, testJob.getNumFlushedElements());
		assertEquals(1, testJob.getNumFailedElements());
	}

	@Test
	@SuppressWarnings("boxing")
	public void testCommitInBatchesWithContinueOnFailureAndErrorsInMultipleBatches() {
		Mockito.when(testJob.getCommitFrequency()).thenReturn(3);
		Mockito.when(testJob.continueOnFailure()).thenReturn(Boolean.TRUE);
		testJob.setElements(Arrays.asList("element1", "element2", "element3",
				"element4", "exception", "element6",
				"exception", "element8", "element9",
				"element10", "element11", "exception"));

		try {
			testJob.execute();
			fail();
		} catch (@SuppressWarnings("unused") Exception e) {
			// We are expecting an exception.
		}

		Mockito.verify(persistence, Mockito.times(3)).commit(false);
		Mockito.verify(persistence, Mockito.times(6)).begin();
		Mockito.verify(persistence, Mockito.times(3)).rollback();
		Mockito.verifyNoMoreInteractions(persistence);
		assertEquals(12, testJob.getNumProcessedElements());
		assertEquals(9, testJob.getNumSuccessfulElements());
		assertEquals(6, testJob.getNumFlushedElements());
		assertEquals(3, testJob.getNumFailedElements());
	}
}
