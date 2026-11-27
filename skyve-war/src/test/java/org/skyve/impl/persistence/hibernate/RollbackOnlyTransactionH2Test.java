package org.skyve.impl.persistence.hibernate;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.hibernate.Hibernate;
import org.hibernate.LazyInitializationException;
import org.junit.jupiter.api.Test;
import org.skyve.domain.messages.UniqueConstraintViolationException;
import org.skyve.domain.types.Decimal2;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.job.IteratingJob;
import org.skyve.persistence.DocumentQuery;

import jakarta.persistence.EntityTransaction;
import modules.test.AbstractSkyveTest;
import modules.test.domain.AllAttributesPersistent;
import modules.test.domain.UniqueConstraintNonNullable;

/**
 * Saves that fail in the database leave the transaction rollback-only.
 * {@link AbstractHibernatePersistence#rollback()} leaves such a transaction open, so that beans loaded in it can still
 * lazy load, as a Faces page does when it renders after a failed action.
 * {@link AbstractHibernatePersistence#begin()} rolls it back, so that the work after it, such as the rest of a job,
 * is not lost with it.
 * <p>
 * H2 lets a transaction carry on reading after a failed statement, as SQL Server and MySQL do.
 * PostgreSQL aborts the transaction instead, so there the reads and lazy loads after a failed save fail anyway.
 */
// The Session and EntityManager belong to the test harness persistence, so they are not closed here
@SuppressWarnings({"static-method", "resource"})
class RollbackOnlyTransactionH2Test extends AbstractSkyveTest {
	/** Too big for decimal2's 20 digit precision, so the save fails in the database and not in Skyve's validation. */
	private static final Decimal2 OVERFLOWING_DECIMAL = new Decimal2(new BigDecimal("1E+25"));

	@Test
	void testFailedSaveLeavesTheTransactionActiveAndRollbackOnly() {
		AbstractHibernatePersistence ahp = persistence();
		failASave(ahp);
	}

	@Test
	void testRollbackOfAFailedSaveKeepsLoadedBeansLazyLoading() {
		AbstractHibernatePersistence ahp = persistence();
		String parentId = commitParentWithChildren("parent", "child 1", "child 2");

		AllAttributesPersistent parent = retrieve(parentId);
		assertFalse(Hibernate.isInitialized(parent.getAggregatedCollection()));
		assertFalse(Hibernate.isInitialized(parent.getAggregatedAssociation()));

		failASave(ahp);
		ahp.rollback();

		assertTrue(transaction(ahp).isActive());
		assertEquals(List.of("child 1", "child 2"), parent.getAggregatedCollection().stream().map(AllAttributesPersistent::getText).sorted().toList());
		assertEquals("association", parent.getAggregatedAssociation().getText());
	}

	@Test
	void testFacesFailedActionRendersAndCommitsNothing() {
		AbstractHibernatePersistence ahp = persistence();
		String parentId = commitParentWithChildren("parent", "child");

		// restore view loads the bean
		AllAttributesPersistent parent = retrieve(parentId);
		// the action changes and saves it, saves something else, then fails
		parent.setText("changed by the failed action");
		ahp.save(parent);
		ahp.save(newBean("saved by the failed action"));
		failASave(ahp);
		// FacesAction
		ahp.setRollbackOnly();
		// a catch block in the action code
		ahp.rollback();
		// render response lazy loads
		assertEquals(1, parent.getAggregatedCollection().size());
		assertEquals("association", parent.getAggregatedAssociation().getText());
		// SkyveFacesPhaseListener after render response (commit(true) there, but the test harness owns this persistence)
		ahp.commit(false);
		ahp.begin();

		assertEquals(List.of("association", "child", "parent"), committedTexts());
	}

	@Test
	void testBeginAfterAFailedSaveKeepsTheWorkAfterIt() {
		AbstractHibernatePersistence ahp = persistence();
		ahp.save(newBean("lost with the failure"));
		failASave(ahp);
		ahp.rollback();

		ahp.begin();

		assertFalse(transaction(ahp).getRollbackOnly());
		ahp.save(newBean("saved after the failure"));
		ahp.commit(false);
		ahp.begin();

		assertEquals(List.of("saved after the failure"), committedTexts());
	}

	@Test
	void testBeginAfterAFailedSaveDetachesLoadedBeans() {
		AbstractHibernatePersistence ahp = persistence();
		String parentId = commitParentWithChildren("parent", "child");
		AllAttributesPersistent parent = retrieve(parentId);

		failASave(ahp);
		ahp.rollback();
		ahp.begin();

		assertFalse(ahp.getSession().contains(entityName(ahp), parent));
		List<AllAttributesPersistent> children = parent.getAggregatedCollection();
		assertThrows(LazyInitializationException.class, children::size);
		// Retrieving it again gives a bean of the new transaction
		AllAttributesPersistent again = retrieve(parentId);
		assertEquals(1, again.getAggregatedCollection().size());
	}

	/**
	 * The downstream job that found this: one transaction per document, and a failure is recorded on the document
	 * in a transaction of its own, after retrieving it again.
	 */
	@Test
	void testFailuresRecordedOnTheDocumentInTheirOwnTransactionAreCommitted() {
		AbstractHibernatePersistence ahp = persistence();
		List<String> ids = new ArrayList<>();
		for (int i = 1; i <= 4; i++) {
			ids.add(ahp.save(newBean("received " + i)).getBizId());
		}
		ahp.commit(false);
		ahp.begin();

		for (String bizId : ids) {
			try {
				AllAttributesPersistent document = retrieve(bizId);
				document.setText(document.getText().replace("received", "processed"));
				if (document.getText().endsWith("2") || document.getText().endsWith("3")) {
					document.setDecimal2(OVERFLOWING_DECIMAL);
				}
				ahp.save(document);
				ahp.commit(false);
			}
			catch (@SuppressWarnings("unused") RuntimeException e) {
				ahp.rollback();
				ahp.begin();
				AllAttributesPersistent document = retrieve(bizId);
				document.setText(document.getText().replace("received", "failed"));
				ahp.save(document);
				ahp.commit(false);
			}
			finally {
				ahp.begin();
			}
		}

		assertEquals(List.of("failed 2", "failed 3", "processed 1", "processed 4"), committedTexts());
	}

	@Test
	void testIteratingJobKeepsTheElementsAfterAFailedSave() {
		AbstractHibernatePersistence ahp = persistence();
		SaveBeansJob job = new SaveBeansJob(ahp, List.of("1", "fail", "3", "fail", "5"), 1);

		assertJobReportsFailures(job, 2);
		ahp.commit(false); // as the job runner does
		ahp.begin();

		assertEquals(3, job.getNumSuccessfulElements());
		assertEquals(2, job.getNumFailedElements());
		assertEquals(List.of("1", "3", "5"), committedTexts());
	}

	@Test
	void testIteratingJobInBatchesKeepsTheBatchesAfterAFailedSave() {
		AbstractHibernatePersistence ahp = persistence();
		SaveBeansJob job = new SaveBeansJob(ahp, List.of("1", "2", "3", "fail", "5", "6"), 2);

		assertJobReportsFailures(job, 1);
		ahp.commit(false);
		ahp.begin();

		// 3 is rolled back with the failure
		assertEquals(List.of("1", "2", "5", "6"), committedTexts());
	}

	/**
	 * The unique constraint checks of the failed transaction are forgotten with it,
	 * so the same bean is checked again in the next one.
	 * There is no rollback() before begin(), because rollback() forgets them too.
	 */
	@Test
	void testUniqueConstraintsAreCheckedAgainAfterAFailedTransaction() {
		AbstractHibernatePersistence ahp = persistence();
		UniqueConstraintNonNullable first = newUnique();
		ahp.save(first);
		failASave(ahp);
		ahp.begin();

		// The first one was rolled back, so a second with the same values can be saved
		UniqueConstraintNonNullable second = newUnique();
		ahp.save(second);
		// and the first one now clashes with it
		assertThrows(UniqueConstraintViolationException.class, () -> ahp.save(first));
		ahp.rollback();
		ahp.begin();

		ahp.save(newUnique());
		ahp.commit(false);
		ahp.begin();
		UniqueConstraintNonNullable duplicate = newUnique();
		assertThrows(UniqueConstraintViolationException.class, () -> ahp.save(duplicate));
	}

	/**
	 * Saves a bean per element, failing the elements called "fail".
	 */
	private static final class SaveBeansJob extends IteratingJob<String> {
		private final AbstractHibernatePersistence persistence;
		private final List<String> texts;
		private final int commitFrequency;

		SaveBeansJob(AbstractHibernatePersistence persistence, List<String> texts, int commitFrequency) {
			this.persistence = persistence;
			this.texts = texts;
			this.commitFrequency = commitFrequency;
			setPersistence(persistence);
		}

		@Override
		protected Collection<String> getElements() {
			return texts;
		}

		@Override
		protected void operation(String text) {
			AllAttributesPersistent bean = newBean(text);
			if ("fail".equals(text)) {
				bean.setDecimal2(OVERFLOWING_DECIMAL);
			}
			persistence.save(bean);
		}

		@Override
		protected boolean continueOnFailure() {
			return true;
		}

		@Override
		protected int getCommitFrequency() {
			return commitFrequency;
		}
	}

	/** Run the job, which throws at the end when an element failed. */
	private static void assertJobReportsFailures(IteratingJob<?> job, int failures) {
		RuntimeException e = assertThrows(RuntimeException.class, job::execute);
		assertEquals("Failed to process " + failures + " elements.", e.getMessage());
	}

	private static AbstractHibernatePersistence persistence() {
		return (AbstractHibernatePersistence) AbstractPersistence.get();
	}

	private static String entityName(AbstractHibernatePersistence ahp) {
		return ahp.getDocumentEntityName(AllAttributesPersistent.MODULE_NAME, AllAttributesPersistent.DOCUMENT_NAME);
	}

	private static EntityTransaction transaction(AbstractHibernatePersistence ahp) {
		return ahp.getEntityManager().getTransaction();
	}

	private AllAttributesPersistent retrieve(String bizId) {
		AllAttributesPersistent result = persistence().retrieve(aapd, bizId);
		if (result == null) {
			throw new AssertionError("No bean " + bizId);
		}
		return result;
	}

	private static AllAttributesPersistent newBean(String text) {
		AllAttributesPersistent result = AllAttributesPersistent.newInstance();
		result.setText(text);
		return result;
	}

	private static UniqueConstraintNonNullable newUnique() {
		UniqueConstraintNonNullable result = UniqueConstraintNonNullable.newInstance();
		result.setBooleanFlag(Boolean.TRUE);
		result.setEnum3(UniqueConstraintNonNullable.Enum3.one);
		result.setText("unique");
		return result;
	}

	/** Fail a save in the database, and check that Hibernate has marked the transaction rollback-only. */
	private static void failASave(AbstractHibernatePersistence ahp) {
		AllAttributesPersistent bad = newBean("fails");
		bad.setDecimal2(OVERFLOWING_DECIMAL);
		assertThrows(RuntimeException.class, () -> ahp.save(bad));
		EntityTransaction et = transaction(ahp);
		assertTrue(et.isActive());
		assertTrue(et.getRollbackOnly());
	}

	/** Commit a parent with an aggregated association and children, and begin a new transaction with an empty session. */
	private static String commitParentWithChildren(String parentText, String... childTexts) {
		AbstractHibernatePersistence ahp = persistence();
		AllAttributesPersistent parent = newBean(parentText);
		parent.setAggregatedAssociation(newBean("association"));
		for (String childText : childTexts) {
			parent.getAggregatedCollection().add(newBean(childText));
		}
		String result = ahp.save(parent).getBizId();
		ahp.commit(false);
		ahp.evictAllCached();
		ahp.begin();
		return result;
	}

	/** The texts of the committed beans, read with an empty session after the caller's commit. */
	private List<String> committedTexts() {
		AbstractHibernatePersistence ahp = persistence();
		ahp.evictAllCached();
		DocumentQuery q = ahp.newDocumentQuery(aapd);
		q.addBoundProjection(AllAttributesPersistent.textPropertyName);
		q.addBoundOrdering(AllAttributesPersistent.textPropertyName);
		return q.scalarResults(String.class);
	}
}
