package org.skyve.impl.cache;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.Iterator;

import org.hibernate.internal.util.SerializationHelper;
import org.junit.jupiter.api.Test;
import org.quartz.JobDataMap;
import org.quartz.JobExecutionContext;
import org.skyve.impl.job.AbstractSkyveJob;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.web.AbstractWebContext;
import org.skyve.job.ViewBackgroundTask;
import org.skyve.metadata.user.User;
import org.skyve.persistence.AutoClosingIterable;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import modules.test.domain.AllAttributesPersistent;
import modules.test.domain.TestDocument;
import util.AbstractH2TestTruncate;

/**
 * Characterises conversation serialisation while Hibernate has completed or open JDBC work.
 */
class ConversationSerializationH2Test extends AbstractH2TestTruncate {
	private static final class CheckpointingFailingBackgroundTask extends ViewBackgroundTask<AllAttributesPersistent> {
		private String laterBizId;

		@Override
		public void execute(AllAttributesPersistent bean) throws Exception {
			AbstractPersistence persistence = AbstractPersistence.get();
			bean.setText("background checkpoint");
			persistence.save(bean);
			cacheConversationAndCycleTransaction();

			AllAttributesPersistent laterBean = new DataBuilder().fixture(FixtureType.crud).build(TestDocument.ALL_ATTRIBUTES_PERSISTENT);
			laterBean.setText("background rollback");
			laterBean = persistence.save(laterBean);
			laterBizId = laterBean.getBizId();
			throw new IllegalStateException("Roll back work after the background checkpoint");
		}
	}

	@Test
	@SuppressWarnings("static-method")
	void cacheConversationCommitsActiveTransactionAfterCompletedQuery() {
		AbstractPersistence persistence = AbstractPersistence.get();
		AbstractWebContext webContext = mockWebContext();
		webContext.setConversation(persistence);

		Number result = persistence.newSQL("select 1").scalarResult(Number.class);
		assertNotNull(result);

		assertDoesNotThrow(() -> StateUtil.commitAndCacheConversation(webContext));
	}

	@Test
	@SuppressWarnings("static-method")
	void cacheConversationClosesOpenQueryCursorBeforeSerialisation() throws Exception {
		AbstractPersistence persistence = AbstractPersistence.get();
		AbstractWebContext webContext = mockWebContext();
		webContext.setConversation(persistence);

		try (AutoClosingIterable<Number> rows = persistence.newSQL("select 1").scalarIterable(Number.class)) {
			Iterator<Number> iterator = rows.iterator();
			assertTrue(iterator.hasNext());

			IllegalStateException failure = assertThrows(IllegalStateException.class,
					() -> SerializationHelper.serialize(webContext));
			assertTrue(failure.getMessage().contains("Cannot serialize SessionImpl"), failure.getMessage());

			assertDoesNotThrow(() -> StateUtil.commitAndCacheConversation(webContext));
		}
	}

	@Test
	@SuppressWarnings({"static-method", "null"}) // a null here fails the test
	void cachedConversationCanRetainValidationStateAfterDeliberateRollback() {
		AbstractPersistence persistence = AbstractPersistence.get();
		User user = persistence.getUser();
		AllAttributesPersistent bean = new DataBuilder().fixture(FixtureType.crud).build(TestDocument.ALL_ATTRIBUTES_PERSISTENT);
		bean.setText("cached but rolled back");
		bean = persistence.save(bean);

		AbstractWebContext webContext = mockWebContext();
		webContext.setCurrentBean(bean);
		webContext.setConversation(persistence);
		String webId = webContext.getWebId();
		String bizId = bean.getBizId();

		persistence.rollback();
		assertDoesNotThrow(() -> StateUtil.commitAndCacheConversation(webContext));
		persistence.commit(true);

		AbstractWebContext restoredContext = StateUtil.getCachedConversation(webId, null);
		assertNotNull(restoredContext);
		AllAttributesPersistent restoredBean = (AllAttributesPersistent) restoredContext.getCurrentBean();
		assertEquals("cached but rolled back", restoredBean.getText());

		AbstractPersistence restoredPersistence = restoredContext.getConversation();
		restoredPersistence.setForThread();
		restoredPersistence.begin();
		restoredPersistence.setUser(user);
		AllAttributesPersistent cachedEntity = restoredPersistence.retrieve(TestDocument.ALL_ATTRIBUTES_PERSISTENT,
				bizId);
		assertNull(cachedEntity);

		Number databaseRows = restoredPersistence
				.newSQL("select count(1) from TEST_AllAttributesPersistent where bizId = :bizId")
				.putParameter("bizId", bizId, false)
				.scalarResult(Number.class);
		assertNotNull(databaseRows);
		assertEquals(0, databaseRows.intValue());
	}

	@Test
	@SuppressWarnings({"static-method", "null"}) // a null here fails the test
	void cacheConversationCommitsMutationAndKeepsConversationUsable() {
		AbstractPersistence persistence = AbstractPersistence.get();
		User user = persistence.getUser();
		AllAttributesPersistent bean = new DataBuilder().fixture(FixtureType.crud).build(TestDocument.ALL_ATTRIBUTES_PERSISTENT);
		bean.setText("committed before cache");
		bean = persistence.save(bean);

		AbstractWebContext webContext = mockWebContext();
		webContext.setCurrentBean(bean);
		webContext.setConversation(persistence);
		String webId = webContext.getWebId();
		String bizId = bean.getBizId();

		assertDoesNotThrow(() -> StateUtil.commitAndCacheConversation(webContext));
		persistence.commit(true);

		AbstractWebContext restoredContext = StateUtil.getCachedConversation(webId, null);
		assertNotNull(restoredContext);
		AbstractPersistence restoredPersistence = restoredContext.getConversation();
		restoredPersistence.setForThread();
		restoredPersistence.begin();
		restoredPersistence.setUser(user);

		AllAttributesPersistent retrieved = restoredPersistence.retrieve(TestDocument.ALL_ATTRIBUTES_PERSISTENT,
				bizId);
		assertNotNull(retrieved);
		assertEquals("committed before cache", retrieved.getText());
	}

	/**
	 * Covers terminal cache calls made by download/export, upload/import, SmartClient
	 * and the Faces phase listener before their final {@code commit(true)} cleanup.
	 */
	@Test
	@SuppressWarnings("static-method")
	void terminalWorkflowRollbackAfterCacheDoesNotUndoCheckpoint() {
		AbstractPersistence persistence = AbstractPersistence.get();
		User user = persistence.getUser();
		AllAttributesPersistent bean = new DataBuilder().fixture(FixtureType.crud).build(TestDocument.ALL_ATTRIBUTES_PERSISTENT);
		bean.setText("terminal checkpoint");
		bean = persistence.save(bean);
		String bizId = bean.getBizId();

		AbstractWebContext webContext = mockWebContext();
		webContext.setCurrentBean(bean);
		webContext.setConversation(persistence);
		StateUtil.commitAndCacheConversation(webContext);

		// This is the defensive rollback used by outer workflow catch blocks.
		persistence.rollback();
		persistence.commit(true);

		AbstractPersistence verificationPersistence = AbstractPersistence.get();
		verificationPersistence.begin();
		verificationPersistence.setUser(user);
		AllAttributesPersistent retrieved = verificationPersistence.retrieve(TestDocument.ALL_ATTRIBUTES_PERSISTENT,
				bizId);
		assertNotNull(retrieved);
		assertEquals("terminal checkpoint", retrieved.getText());
	}

	/**
	 * Covers the SmartClient remove workflow, which deletes a bean and then caches
	 * the conversation before the servlet performs its final persistence cleanup.
	 */
	@Test
	@SuppressWarnings("static-method")
	void smartClientDeleteIsCommittedBeforeConversationIsCached() {
		AbstractPersistence persistence = AbstractPersistence.get();
		User user = persistence.getUser();
		AllAttributesPersistent bean = new DataBuilder().fixture(FixtureType.crud).build(TestDocument.ALL_ATTRIBUTES_PERSISTENT);
		bean = persistence.save(bean);
		String bizId = bean.getBizId();
		persistence.commit(false);
		persistence.begin();

		persistence.delete(bean);
		AbstractWebContext webContext = mockWebContext();
		webContext.setCurrentBean(bean);
		webContext.setConversation(persistence);
		assertDoesNotThrow(() -> StateUtil.commitAndCacheConversation(webContext));
		persistence.commit(true);

		AbstractPersistence verificationPersistence = AbstractPersistence.get();
		verificationPersistence.begin();
		verificationPersistence.setUser(user);
		Number rows = verificationPersistence
				.newSQL("select count(1) from TEST_AllAttributesPersistent where bizId = :bizId")
				.putParameter("bizId", bizId, false)
				.scalarResult(Number.class);
		assertNotNull(rows);
		assertEquals(0, rows.intValue());
	}

	/**
	 * Covers an explicit action/redirect cache followed by the normal Faces phase
	 * listener cache in the same request, where the second call has no active transaction.
	 */
	@Test
	@SuppressWarnings("static-method")
	void repeatedTerminalCacheDoesNotRequireAReplacementTransaction() {
		AbstractPersistence persistence = AbstractPersistence.get();
		AllAttributesPersistent bean = new DataBuilder().fixture(FixtureType.crud).build(TestDocument.ALL_ATTRIBUTES_PERSISTENT);
		bean.setText("cached twice");
		bean = persistence.save(bean);

		AbstractWebContext webContext = mockWebContext();
		webContext.setCurrentBean(bean);
		webContext.setConversation(persistence);

		assertDoesNotThrow(() -> StateUtil.commitAndCacheConversation(webContext));
		assertDoesNotThrow(() -> StateUtil.commitAndCacheConversation(webContext));
	}

	@Test
	@SuppressWarnings("static-method")
	void webContextCacheBeginsAReplacementTransactionForContinuedWork() {
		AbstractPersistence persistence = AbstractPersistence.get();
		AllAttributesPersistent checkpointedBean = new DataBuilder().fixture(FixtureType.crud).build(TestDocument.ALL_ATTRIBUTES_PERSISTENT);
		checkpointedBean.setText("checkpointed");
		checkpointedBean = persistence.save(checkpointedBean);

		AbstractWebContext webContext = mockWebContext();
		webContext.setCurrentBean(checkpointedBean);
		webContext.setConversation(persistence);
		webContext.cacheConversationAndCycleTransaction();

		AllAttributesPersistent laterBean = new DataBuilder().fixture(FixtureType.crud).build(TestDocument.ALL_ATTRIBUTES_PERSISTENT);
		laterBean.setText("rolled back later");
		laterBean = persistence.save(laterBean);
		String laterBizId = laterBean.getBizId();
		persistence.rollback();
		persistence.begin();

		Number checkpointedRows = persistence
				.newSQL("select count(1) from TEST_AllAttributesPersistent where bizId = :bizId")
				.putParameter("bizId", checkpointedBean.getBizId(), false)
				.scalarResult(Number.class);
		Number laterRows = persistence
				.newSQL("select count(1) from TEST_AllAttributesPersistent where bizId = :bizId")
				.putParameter("bizId", laterBizId, false)
				.scalarResult(Number.class);
		assertNotNull(checkpointedRows);
		assertNotNull(laterRows);
		assertEquals(1, checkpointedRows.intValue());
		assertEquals(0, laterRows.intValue());
	}

	/**
	 * Exercises the real background-task lifecycle: its explicit cache checkpoint
	 * commits prior work, begins a replacement transaction, and the job-level catch
	 * rolls back only work performed after that checkpoint.
	 */
	@Test
	@SuppressWarnings("static-method")
	void backgroundTaskCacheCyclesTransactionAndLaterFailureRollsBackOnlyLaterWork() throws Exception {
		AbstractPersistence persistence = AbstractPersistence.get();
		User user = persistence.getUser();
		AllAttributesPersistent bean = new DataBuilder().fixture(FixtureType.crud).build(TestDocument.ALL_ATTRIBUTES_PERSISTENT);
		bean.setText("before background");
		bean = persistence.save(bean);
		String bizId = bean.getBizId();

		AbstractWebContext webContext = mockWebContext();
		webContext.setCurrentBean(bean);
		webContext.setConversation(persistence);
		String webId = webContext.getWebId();
		StateUtil.commitAndCacheConversation(webContext);
		persistence.commit(true);

		JobDataMap parameters = new JobDataMap();
		parameters.put(AbstractWebContext.CONTEXT_NAME, webId);
		parameters.put(AbstractSkyveJob.USER_JOB_PARAMETER_KEY, user);
		JobExecutionContext jobContext = mock(JobExecutionContext.class);
		when(jobContext.getMergedJobDataMap()).thenReturn(parameters);

		CheckpointingFailingBackgroundTask task = new CheckpointingFailingBackgroundTask();
		task.execute(jobContext);

		AbstractPersistence verificationPersistence = AbstractPersistence.get();
		verificationPersistence.begin();
		verificationPersistence.setUser(user);
		AllAttributesPersistent checkpointed = verificationPersistence.retrieve(TestDocument.ALL_ATTRIBUTES_PERSISTENT,
				bizId);
		assertNotNull(checkpointed);
		assertEquals("background checkpoint", checkpointed.getText());
		assertNotNull(task.laterBizId);

		Number laterRows = verificationPersistence
				.newSQL("select count(1) from TEST_AllAttributesPersistent where bizId = :bizId")
				.putParameter("bizId", task.laterBizId, false)
				.scalarResult(Number.class);
		assertNotNull(laterRows);
		assertEquals(0, laterRows.intValue());
	}

}
