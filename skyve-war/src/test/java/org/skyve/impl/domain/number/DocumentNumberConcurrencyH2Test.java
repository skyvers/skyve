package org.skyve.impl.domain.number;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.ArrayList;
import java.util.List;
import java.util.TreeSet;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

import org.apache.commons.codec.digest.DigestUtils;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.function.Executable;
import org.skyve.CORE;
import org.skyve.domain.app.AppConstants;
import org.skyve.domain.app.admin.DocumentNumber;
import org.skyve.domain.messages.UniqueConstraintViolationException;
import org.skyve.domain.number.NumberGenerator;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.persistence.hibernate.UniquenessEntity;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.user.User;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;

import modules.kitchensink.domain.LookupDescription;
import util.AbstractH2TestTruncate;

/**
 * Proves that concurrent requests take distinct, gapless numbers from one sequence, whether or not
 * the sequence's {@code DocumentNumber} row exists yet.
 *
 * <p>Each worker thread is a request: its own persistence and transaction, begun before a latch
 * releases every worker at once, then committed (or rolled back on failure) and closed.
 */
@SuppressWarnings("static-method") // JUnit 5 test methods are instance methods
class DocumentNumberConcurrencyH2Test extends AbstractH2TestTruncate {
	private static final int USERS = 8;
	private static final int ROUNDS = 10;
	private static final long TIMEOUT_SECONDS = 60;
	private static final String UNIQUE_SEQUENCE = "uniqueSequence";

	@Test
	void concurrentFirstUseOfASequenceInTheCallersTransaction() throws Exception {
		assertRounds(new DocumentNumberGenerator(), "inTransactionFirstUse", false);
	}

	@Test
	void concurrentIncrementsOfAnExistingSequenceInTheCallersTransaction() throws Exception {
		assertRounds(new DocumentNumberGenerator(), "inTransactionExisting", true);
	}

	@Test
	void concurrentFirstUseOfASequenceInAutonomousTransactions() throws Exception {
		assertRounds(new DocumentNumberAutonomousTransactionGenerator(), "autonomousFirstUse", false);
	}

	@Test
	void concurrentIncrementsOfAnExistingSequenceInAutonomousTransactions() throws Exception {
		assertRounds(new DocumentNumberAutonomousTransactionGenerator(), "autonomousExisting", true);
	}

	/**
	 * A creation that conflicts again after the retry is thrown, not retried for ever: here the
	 * conflict is this transaction's own pending row for the sequence, which the query cannot see.
	 */
	@Test
	void aSecondCreationConflictIsThrown() throws Exception {
		Persistence persistence = CORE.getPersistence();
		User user = persistence.getUser();
		Customer customer = user.getCustomer();
		Document document = customer.getModule(AppConstants.ADMIN_MODULE_NAME)
										.getDocument(customer, AppConstants.DOCUMENT_NUMBER_DOCUMENT_NAME);
		DocumentNumber pending = document.newInstance(user);
		pending.setModuleName(LookupDescription.MODULE_NAME);
		pending.setDocumentName(LookupDescription.DOCUMENT_NAME);
		pending.setSequenceName("pending");
		pending.setDocumentNumber("0");
		// checks and claims the sequence's unique key for this transaction without writing the row
		persistence.preMerge(document, pending);

		DocumentNumberGenerator generator = new DocumentNumberGenerator();
		UniqueConstraintViolationException e = assertThrows(UniqueConstraintViolationException.class,
				() -> generator.next(LookupDescription.MODULE_NAME, LookupDescription.DOCUMENT_NAME, "pending"));
		assertAll(() -> assertEquals(UNIQUE_SEQUENCE, e.getConstraintName()),
					() -> assertNull(e.getCause(), "raised in Java, so it was retried once before being thrown"));
	}

	/**
	 * A violation signalled by a database failure is thrown at once, not retried: the failure
	 * (here the uniqueness key's insert failing on a committed key, as a timeout, deadlock or
	 * serialisation failure would) may have rolled back or aborted the transaction. A retry would
	 * end in the same transaction's own key, a violation with no cause.
	 */
	@Test
	void aViolationFromADatabaseFailureIsNotRetried() {
		Persistence persistence = CORE.getPersistence();
		User user = persistence.getUser();
		String sequence = "failing";
		// the key AbstractHibernatePersistence.checkUniqueConstraints inserts for DocumentNumber's customer-scoped constraint
		String key = AppConstants.ADMIN_MODULE_NAME + '|' + AppConstants.DOCUMENT_NUMBER_DOCUMENT_NAME + '|' +
						user.getCustomerName() + '|' + UNIQUE_SEQUENCE + '|' +
						LookupDescription.MODULE_NAME + '|' + LookupDescription.DOCUMENT_NAME + '|' + sequence + '|';
		persistence.newSQL("insert into " + UniquenessEntity.TABLE_NAME + " (" + UniquenessEntity.HASH_COLUMN_NAME +
							") values (:" + UniquenessEntity.HASH_COLUMN_NAME + ")")
						.putParameter(UniquenessEntity.HASH_COLUMN_NAME, DigestUtils.sha256Hex(key), false)
						.execute();
		persistence.commit(false);
		persistence.begin();

		DocumentNumberGenerator generator = new DocumentNumberGenerator();
		UniqueConstraintViolationException e = assertThrows(UniqueConstraintViolationException.class,
				() -> generator.next(LookupDescription.MODULE_NAME, LookupDescription.DOCUMENT_NAME, sequence));
		assertAll(() -> assertEquals(UNIQUE_SEQUENCE, e.getConstraintName()),
					() -> assertNotNull(e.getCause(), "the insert's failure, so the first attempt's violation, not a retry's"));
	}

	/**
	 * Races {@link #USERS} workers on a fresh sequence per round and asserts every round: no worker
	 * failed, the numbers are consecutive from the sequence's next number with no duplicate or gap,
	 * and the sequence has one row, holding the last number.
	 */
	private static void assertRounds(NumberGenerator generator, String sequencePrefix, boolean existing) throws Exception {
		User user = CORE.getUser();
		List<Executable> checks = new ArrayList<>(ROUNDS);
		ExecutorService executor = Executors.newFixedThreadPool(USERS);
		try {
			for (int round = 1; round <= ROUNDS; round++) {
				String sequence = sequencePrefix + round;
				int first = 1;
				if (existing) {
					race(executor, generator, sequence, user, 1);
					first = 2;
				}
				List<String> outcomes = race(executor, generator, sequence, user, USERS);
				checks.add(roundCheck(round, sequence, first, outcomes));
			}
		}
		finally {
			executor.shutdownNow();
		}
		assertAll(checks);
	}

	private static Executable roundCheck(int round, String sequence, int first, List<String> outcomes) {
		List<String> expected = new ArrayList<>(USERS);
		for (int i = 0; i < USERS; i++) {
			expected.add(String.valueOf(first + i));
		}
		TreeSet<Integer> numbers = new TreeSet<>();
		for (String outcome : outcomes) {
			try {
				numbers.add(Integer.valueOf(outcome));
			}
			catch (@SuppressWarnings("unused") NumberFormatException e) {
				// a failure message: the comparison below reports it
			}
		}
		List<String> actual = new ArrayList<>(numbers.size());
		for (Integer number : numbers) {
			actual.add(number.toString());
		}
		String last = expected.get(USERS - 1);
		List<String> rows = documentNumbers(sequence);
		return () -> assertAll("round " + round,
								() -> assertEquals(expected, actual, "round " + round + " outcomes " + outcomes),
								() -> assertEquals(List.of(last), rows, "round " + round + " DocumentNumber rows"));
	}

	/**
	 * Starts {@code users} workers together and returns each one's number, or its failure as
	 * {@code "failed: <exception>"}.
	 */
	private static List<String> race(ExecutorService executor,
										NumberGenerator generator,
										String sequence,
										User user,
										int users)
	throws InterruptedException, ExecutionException, TimeoutException {
		CountDownLatch ready = new CountDownLatch(users);
		CountDownLatch start = new CountDownLatch(1);
		List<Future<String>> futures = new ArrayList<>(users);
		for (int i = 0; i < users; i++) {
			futures.add(executor.submit(() -> nextInOwnTransaction(generator, sequence, user, ready, start)));
		}
		assertTrue(ready.await(TIMEOUT_SECONDS, TimeUnit.SECONDS), "every worker began its transaction");
		start.countDown();
		List<String> result = new ArrayList<>(users);
		for (Future<String> future : futures) {
			result.add(future.get(TIMEOUT_SECONDS, TimeUnit.SECONDS));
		}
		return result;
	}

	private static String nextInOwnTransaction(NumberGenerator generator,
												String sequence,
												User user,
												CountDownLatch ready,
												CountDownLatch start)
	throws InterruptedException {
		AbstractPersistence persistence = AbstractPersistence.get();
		persistence.setUser(user);
		persistence.begin();
		boolean committed = false;
		try {
			ready.countDown();
			start.await();
			String result = generator.next(LookupDescription.MODULE_NAME, LookupDescription.DOCUMENT_NAME, sequence);
			persistence.commit(true);
			committed = true;
			return result;
		}
		catch (RuntimeException e) {
			return "failed: " + e;
		}
		finally {
			// whatever ended the worker (a failure, an interrupt or an Error), release its persistence and connection
			if (! committed) {
				persistence.rollback();
				persistence.commit(true);
			}
		}
	}

	/**
	 * Returns the number held by each {@code DocumentNumber} row of the sequence, read in a new
	 * transaction on this thread.
	 */
	private static List<String> documentNumbers(String sequence) {
		Persistence persistence = CORE.getPersistence();
		persistence.commit(false);
		persistence.begin();
		DocumentQuery query = persistence.newDocumentQuery(AppConstants.ADMIN_MODULE_NAME,
															AppConstants.DOCUMENT_NUMBER_DOCUMENT_NAME);
		query.getFilter().addEquals(AppConstants.SEQUENCE_NAME_ATTRIBUTE_NAME, sequence);
		List<DocumentNumber> rows = query.beanResults();
		List<String> result = new ArrayList<>(rows.size());
		for (DocumentNumber row : rows) {
			result.add(row.getDocumentNumber());
		}
		return result;
	}
}
