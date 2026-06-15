package org.skyve.impl.job;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;
import java.util.HashMap;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.quartz.JobDataMap;
import org.quartz.JobExecutionContext;
import org.quartz.JobExecutionException;
import org.quartz.UnableToInterruptJobException;
import org.skyve.domain.Bean;
import org.skyve.domain.DynamicPersistentBean;
import org.skyve.domain.app.AppConstants;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.job.JobStatus;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;

class AbstractSkyveJobTest {

	/**
	 * Minimal concrete subclass for testing AbstractSkyveJob's non-abstract behaviour.
	 */
	private static class TestJob extends AbstractSkyveJob {
		private boolean cancelled = false;

		@Override
		public void execute() throws Exception {
			// no-op
		}

		@Override
		public void execute(org.skyve.job.Job job) throws Exception {
			// no-op
		}

		@Override
		public String cancel() {
			if (cancelled) {
				return "already cancelled";
			}
			cancelled = true;
			return null; // null means cancellable
		}

		@Override
		public boolean shouldRollbackOnCancel() {
			return false;
		}

		@Override
		public boolean shouldBeSilent() {
			return false;
		}
	}

	/**
	 * A TestJob that cannot be cancelled.
	 */
	private static class UncancellableJob extends AbstractSkyveJob {
		@Override
		public void execute() throws Exception {
			// no-op
		}

		@Override
		public void execute(org.skyve.job.Job job) throws Exception {
			// no-op
		}

		@Override
		public String cancel() {
			return "cannot cancel";
		}

		@Override
		public boolean shouldRollbackOnCancel() {
			return true;
		}

		@Override
		public boolean shouldBeSilent() {
			return true;
		}
	}

	private TestJob job;

	@BeforeEach
	void setUp() {
		job = new TestJob();
	}

	@AfterEach
	@SuppressWarnings("static-method")
	void tearDown() throws Exception {
		unbindPersistenceFromThread();
	}

	// ── display name ────────────────────────────────────────────────────────

	@Test
	@SuppressWarnings("static-method")
	void displayNameIsNullByDefault() {
		assertNull(new TestJob().getDisplayName());
	}

	@Test
	void setDisplayNameStoresValue() {
		job.setDisplayName("My Job");
		assertEquals("My Job", job.getDisplayName());
	}

	// ── percent complete ─────────────────────────────────────────────────────

	@Test
	void percentCompleteStartsAtZero() {
		assertEquals(0, job.getPercentComplete());
	}

	@Test
	void setPercentCompleteStoresValue() {
		job.setPercentComplete(42);
		assertEquals(42, job.getPercentComplete());
	}

	@Test
	void setPercentCompleteFromTotalProcessedAndSize() {
		job.setPercentComplete(1, 4);
		assertEquals(25, job.getPercentComplete());
	}

	@Test
	void setPercentCompleteFromTotalsIsHundredWhenComplete() {
		job.setPercentComplete(10, 10);
		assertEquals(100, job.getPercentComplete());
	}

	// ── timestamps ──────────────────────────────────────────────────────────

	@Test
	void startTimeIsNotNullAfterConstruction() {
		assertNotNull(job.getStartTime());
	}

	@Test
	void endTimeIsNullBeforeExecution() {
		assertNull(job.getEndTime());
	}

	// ── status ──────────────────────────────────────────────────────────────

	@Test
	void statusIsNullByDefault() {
		assertNull(job.getStatus());
	}

	// ── log ─────────────────────────────────────────────────────────────────

	@Test
	void logIsEmptyByDefault() {
		assertNotNull(job.getLog());
		assertTrue(job.getLog().isEmpty());
	}

	@Test
	void logCanAcceptEntries() {
		job.getLog().add("first entry");
		assertEquals(1, job.getLog().size());
	}

	@Test
	void createLogDescriptionStringReturnsEmptyStringWhenLogIsEmpty() {
		assertEquals("", job.createLogDescriptionString());
	}

	@Test
	void createLogDescriptionStringReturnsAllEntries() {
		job.getLog().add("line one");
		job.getLog().add("line two");
		String desc = job.createLogDescriptionString();
		assertTrue(desc.contains("line one"));
		assertTrue(desc.contains("line two"));
	}

	// ── bean ────────────────────────────────────────────────────────────────

	@Test
	void beanIsNullByDefault() {
		assertNull(job.getBean());
	}

	@Test
	void setBeanStoresAndRetrievesNull() {
		job.setBean(null);
		assertNull(job.getBean());
	}

	// ── interrupt / cancel ──────────────────────────────────────────────────

	@Test
	void interruptSetsStatusToCancelledWhenJobIsCancellable() throws UnableToInterruptJobException {
		job.interrupt();
		assertEquals(JobStatus.cancelled, job.getStatus());
	}

	@Test
	@SuppressWarnings("static-method")
	void interruptThrowsUnableToInterruptWhenJobCannotBeCancelled() {
		UncancellableJob uncancellable = new UncancellableJob();
		assertThrows(UnableToInterruptJobException.class, uncancellable::interrupt);
	}

	// ── persistJobExecutionOnSuccess ─────────────────────────────────────────

	@Test
	void persistJobExecutionOnSuccessReturnsTrueByDefault() {
		assertTrue(job.persistJobExecutionOnSuccess());
	}

	// ── shouldRollbackOnCancel / shouldBeSilent ──────────────────────────────

	@Test
	void shouldRollbackOnCancelDelegatesToSubclass() {
		assertFalse(job.shouldRollbackOnCancel());
		assertTrue(new UncancellableJob().shouldRollbackOnCancel());
	}

	@Test
	void shouldBeSilentDelegatesToSubclass() {
		assertFalse(job.shouldBeSilent());
		assertTrue(new UncancellableJob().shouldBeSilent());
	}

	// ── setLog ───────────────────────────────────────────────────────────────

	@Test
	void setLogReplacesLogList() {
		java.util.List<String> newLog = new java.util.ArrayList<>();
		newLog.add("entry1");
		newLog.add("entry2");
		job.setLog(newLog);
		assertEquals(newLog, job.getLog());
	}

	@Test
	void createLogDescriptionStringIncludesNewlines() {
		job.getLog().add("a");
		job.getLog().add("b");
		String result = job.createLogDescriptionString();
		assertTrue(result.contains("a\n"));
		assertTrue(result.contains("b\n"));
	}

	// ── setPercentComplete edge cases ─────────────────────────────────────────

	@Test
	void setPercentCompleteFromTotalsHandlesZeroSize() {
		job.setPercentComplete(0, 0);
		assertEquals(0, job.getPercentComplete());
	}

	@Test
	void setPercentCompleteFromTotalsHandlesMidpoint() {
		job.setPercentComplete(5, 10);
		assertEquals(50, job.getPercentComplete());
	}

	@Test
	void executeContextRecordsFailureWhenInjectionIsUnavailable() throws Exception {
		AbstractPersistence persistence = mock(AbstractPersistence.class);
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		Module adminModule = mock(Module.class);
		Document jobDocument = mock(Document.class);
		DynamicPersistentBean persistedJob = newJobBean();
		Bean contextBean = mock(Bean.class);
		JobExecutionContext context = mock(JobExecutionContext.class);
		JobDataMap dataMap = new JobDataMap();
		dataMap.put(AbstractSkyveJob.DISPLAY_NAME_JOB_PARAMETER_KEY, "Nightly Work");
		dataMap.put(AbstractSkyveJob.USER_JOB_PARAMETER_KEY, user);
		dataMap.put(AbstractSkyveJob.BEAN_JOB_PARAMETER_KEY, contextBean);
		when(context.getMergedJobDataMap()).thenReturn(dataMap);
		when(user.getCustomer()).thenReturn(customer);
		when(customer.getModule(AppConstants.ADMIN_MODULE_NAME)).thenReturn(adminModule);
		when(adminModule.getDocument(customer, AppConstants.JOB_DOCUMENT_NAME)).thenReturn(jobDocument);
		when(jobDocument.newInstance(user)).thenReturn(persistedJob);
		when(contextBean.getBizId()).thenReturn("BIZ-1");
		when(contextBean.getBizModule()).thenReturn("sales");
		when(contextBean.getBizDocument()).thenReturn("Order");
		bindPersistenceToThread(persistence);
		job.setPercentComplete(33);
		job.getLog().add("worked");

		JobExecutionException thrown = assertThrows(JobExecutionException.class, () -> job.execute(context));

		assertTrue(thrown.getCause() instanceof IllegalStateException);
		assertEquals(JobStatus.failed, job.getStatus());
		assertEquals("Nightly Work", job.getDisplayName());
		assertNotNull(job.getEndTime());
		assertEquals("Nightly Work", persistedJob.get(AppConstants.DISPLAY_NAME_ATTRIBUTE_NAME));
		assertEquals(JobStatus.failed.toString(), persistedJob.get(AppConstants.STATUS_ATTRIBUTE_NAME));
		assertEquals(Integer.valueOf(33), persistedJob.get(AppConstants.PERCENTAGE_COMPLETE_ATTRIBUTE_NAME));
		assertEquals("BIZ-1", persistedJob.get(AppConstants.BEAN_BIZID_ATTRIBUTE_NAME));
		assertEquals("sales", persistedJob.get(AppConstants.BEAN_MODULE_NAME_ATTRIBUTE_NAME));
		assertEquals("Order", persistedJob.get(AppConstants.BEAN_DOCUMENT_NAME_ATTRIBUTE_NAME));
		assertTrue(job.createLogDescriptionString().contains("No org.apache.deltaspike.core.api.provider.BeanManagerProvider"));
		verify(persistence).setUser(user);
		verify(persistence).setAsyncThread(true);
		verify(persistence).setAsyncThread(false);
		verify(persistence).rollback();
		verify(persistence).save(jobDocument, persistedJob);
		verify(persistence).commit(false);
		verify(persistence).commit(true);
	}

	private static DynamicPersistentBean newJobBean() {
		HashMap<String, Object> properties = new HashMap<>();
		properties.put(AppConstants.START_TIME_ATTRIBUTE_NAME, null);
		properties.put(AppConstants.DISPLAY_NAME_ATTRIBUTE_NAME, null);
		properties.put(AppConstants.STATUS_ATTRIBUTE_NAME, null);
		properties.put(AppConstants.END_TIME_ATTRIBUTE_NAME, null);
		properties.put(AppConstants.PERCENTAGE_COMPLETE_ATTRIBUTE_NAME, null);
		properties.put(AppConstants.LOG_ATTRIBUTE_NAME, null);
		properties.put(AppConstants.BEAN_BIZID_ATTRIBUTE_NAME, null);
		properties.put(AppConstants.BEAN_MODULE_NAME_ATTRIBUTE_NAME, null);
		properties.put(AppConstants.BEAN_DOCUMENT_NAME_ATTRIBUTE_NAME, null);
		return new DynamicPersistentBean(AppConstants.ADMIN_MODULE_NAME, AppConstants.JOB_DOCUMENT_NAME, properties);
	}

	private static void bindPersistenceToThread(AbstractPersistence persistence) throws Exception {
		Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
		field.setAccessible(true);
		@SuppressWarnings("unchecked")
		ThreadLocal<AbstractPersistence> threadLocal = (ThreadLocal<AbstractPersistence>) field.get(null);
		threadLocal.set(persistence);
	}

	private static void unbindPersistenceFromThread() throws Exception {
		Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
		field.setAccessible(true);
		@SuppressWarnings("unchecked")
		ThreadLocal<AbstractPersistence> threadLocal = (ThreadLocal<AbstractPersistence>) field.get(null);
		threadLocal.remove();
	}
}
