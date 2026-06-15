package org.skyve.impl.job;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.sameInstance;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Set;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.ArgumentMatcher;
import org.quartz.CronScheduleBuilder;
import org.quartz.JobBuilder;
import org.quartz.JobDataMap;
import org.quartz.JobDetail;
import org.quartz.JobExecutionContext;
import org.quartz.JobKey;
import org.quartz.ObjectAlreadyExistsException;
import org.quartz.Scheduler;
import org.quartz.SchedulerException;
import org.quartz.Trigger;
import org.quartz.TriggerBuilder;
import org.quartz.TriggerKey;
import org.quartz.UnableToInterruptJobException;
import org.quartz.impl.matchers.GroupMatcher;
import org.skyve.domain.Bean;
import org.skyve.domain.DynamicBean;
import org.skyve.domain.app.AppConstants;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.messages.ValidationException;
import org.skyve.impl.backup.RestoreJob;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.AbstractWebContext;
import org.skyve.job.JobDescription;
import org.skyve.job.JobSchedule;
import org.skyve.job.ViewBackgroundTask;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.module.JobMetaData;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.repository.ProvidedRepository;
import org.skyve.metadata.user.User;

@SuppressWarnings({"static-method", "boxing"})
public class QuartzJobSchedulerTest {
	private Scheduler originalScheduler;
	private Scheduler scheduler;

	@Before
	public void setUp() throws Exception {
		originalScheduler = getScheduler();
		scheduler = mock(Scheduler.class);
		setScheduler(scheduler);
	}

	@After
	public void tearDown() throws Exception {
		setScheduler(originalScheduler);
		unbindPersistenceFromThread();
	}

	@Test
	public void runOneShotJobSchedulesTriggerWithJobData() throws Exception {
		JobMetaData job = job("mod", "job", "Display");
		Bean bean = mock(Bean.class);
		User user = user("customer");

		new QuartzJobScheduler().runOneShotJob(job, bean, user, 7);

		Trigger trigger = scheduledTrigger();
		assertThat(trigger.getJobKey().getGroup(), is("mod"));
		assertThat(trigger.getJobKey().getName(), is("job"));
		assertThat(trigger.getKey().getGroup(), is("customer"));
		assertThat(trigger.getJobDataMap().get(AbstractSkyveJob.DISPLAY_NAME_JOB_PARAMETER_KEY), is("Display"));
		assertThat(trigger.getJobDataMap().get(AbstractSkyveJob.BEAN_JOB_PARAMETER_KEY), is(sameInstance(bean)));
		assertThat(trigger.getJobDataMap().get(AbstractSkyveJob.USER_JOB_PARAMETER_KEY), is(sameInstance(user)));
		assertThat(trigger.getJobDataMap().get(AbstractSkyveJob.SLEEP_JOB_PARAMETER_KEY), is(Integer.valueOf(7)));
	}

	@Test
	public void runOneShotJobWrapsSchedulerException() throws Exception {
		doThrow(new SchedulerException("boom")).when(scheduler).scheduleJob(any(Trigger.class));
		QuartzJobScheduler quartzJobScheduler = new QuartzJobScheduler();
		JobMetaData metaData = job("mod", "job", "Display");
		User currentUser = user("customer");

		DomainException thrown = assertThrows(DomainException.class,
				() -> quartzJobScheduler.runOneShotJob(metaData, null, currentUser));

		assertThat(thrown.getMessage(), is("Cannot schedule job Display"));
	}

	@Test
	public void runOneShotJobConvertsDuplicateTriggerToValidationException() throws Exception {
		doThrow(new ObjectAlreadyExistsException("duplicate")).when(scheduler).scheduleJob(any(Trigger.class));
		QuartzJobScheduler quartzJobScheduler = new QuartzJobScheduler();
		JobMetaData metaData = job("mod", "job", "Display");
		User currentUser = user("customer");

		assertThrows(ValidationException.class,
				() -> quartzJobScheduler.runOneShotJob(metaData, null, currentUser));
	}

	@Test
	public void scheduleOneShotJobUsesRequestedStartTime() throws Exception {
		Date start = new Date(System.currentTimeMillis() + 60_000L);
		JobMetaData job = job("mod", "job", "Display");

		new QuartzJobScheduler().scheduleOneShotJob(job, null, user("customer"), start);

		Trigger trigger = scheduledTrigger();
		assertThat(trigger.getStartTime(), is(start));
		assertThat(trigger.getJobDataMap().get(AbstractSkyveJob.DISPLAY_NAME_JOB_PARAMETER_KEY), is("Display"));
	}

	@Test
	public void scheduleJobParsesModuleJobNameAndBuildsCronTrigger() throws Exception {
		JobSchedule schedule = new JobSchedule();
		schedule.setUuid("schedule-id");
		schedule.setJobName("mod.job");
		schedule.setCronExpression("0 0 12 * * ?");
		schedule.setStartTime(new Date(System.currentTimeMillis() + 60_000L));
		schedule.setEndTime(new Date(System.currentTimeMillis() + 120_000L));
		JobMetaData job = job("mod", "job", "Display");
		Module module = mock(Module.class);
		when(module.getJob("job")).thenReturn(job);
		User user = user("customer");
		when(user.getCustomer().getModule("mod")).thenReturn(module);

		new QuartzJobScheduler().scheduleJob(schedule, user);

		Trigger trigger = scheduledTrigger();
		assertThat(trigger.getKey(), is(new TriggerKey("schedule-id", "customer")));
		assertThat(trigger.getJobKey().getGroup(), is("mod"));
		assertThat(trigger.getJobKey().getName(), is("job"));
		assertThat(trigger.getJobDataMap().get(AbstractSkyveJob.USER_JOB_PARAMETER_KEY), is(sameInstance(user)));
	}

	@Test
	public void runBackgroundTaskSchedulesQuartzJobWithUserAndWebId() throws Exception {
		User user = user("customer");

		new QuartzJobScheduler().runBackgroundTask(TestBackgroundTask.class, user, "web-1");

		ArgumentCaptor<org.quartz.JobDetail> detail = ArgumentCaptor.forClass(org.quartz.JobDetail.class);
		ArgumentCaptor<Trigger> trigger = ArgumentCaptor.forClass(Trigger.class);
		verify(scheduler).scheduleJob(detail.capture(), trigger.capture());
		assertSame(TestBackgroundTask.class, detail.getValue().getJobClass());
		assertThat(trigger.getValue().getJobDataMap().get(AbstractSkyveJob.USER_JOB_PARAMETER_KEY), is(sameInstance(user)));
		assertThat(trigger.getValue().getJobDataMap().get(AbstractWebContext.CONTEXT_NAME), is("web-1"));
	}

	@Test
	public void runBackgroundTaskWrapsSchedulerException() throws Exception {
		doThrow(new SchedulerException("boom")).when(scheduler).scheduleJob(any(org.quartz.JobDetail.class), any(Trigger.class));
		QuartzJobScheduler quartzJobScheduler = new QuartzJobScheduler();
		User currentUser = user("customer");

		assertThrows(DomainException.class,
				() -> quartzJobScheduler.runBackgroundTask(TestBackgroundTask.class, currentUser, "web-1"));
	}

	@Test
	public void runContentGarbageCollectorSchedulesInternalJob() throws Exception {
		new QuartzJobScheduler().runContentGarbageCollector();

		ArgumentCaptor<org.quartz.JobDetail> detail = ArgumentCaptor.forClass(org.quartz.JobDetail.class);
		ArgumentCaptor<Trigger> trigger = ArgumentCaptor.forClass(Trigger.class);
		verify(scheduler).scheduleJob(detail.capture(), trigger.capture());
		assertSame(ContentGarbageCollectionJob.class, detail.getValue().getJobClass());
		assertThat(trigger.getValue().getKey().getGroup(), is("INTERNAL"));
	}

	@Test
	public void runContentGarbageCollectorWrapsSchedulerException() throws Exception {
		doThrow(new SchedulerException("boom")).when(scheduler).scheduleJob(any(org.quartz.JobDetail.class), any(Trigger.class));
		QuartzJobScheduler quartzJobScheduler = new QuartzJobScheduler();

		assertThrows(DomainException.class, quartzJobScheduler::runContentGarbageCollector);
	}

	@Test
	public void scheduleJobSkipsWhenEndTimeHasPassed() throws Exception {
		JobSchedule schedule = new JobSchedule();
		schedule.setUuid("schedule-id");
		schedule.setJobName("mod.job");
		schedule.setCronExpression("0 0 12 * * ?");
		schedule.setStartTime(new Date(System.currentTimeMillis() - 120_000L));
		schedule.setEndTime(new Date(System.currentTimeMillis() - 60_000L));
		JobMetaData job = job("mod", "job", "Display");
		Module module = mock(Module.class);
		when(module.getJob("job")).thenReturn(job);
		User user = user("customer");
		when(user.getCustomer().getModule("mod")).thenReturn(module);

		new QuartzJobScheduler().scheduleJob(schedule, user);

		verify(scheduler, never()).scheduleJob(any(Trigger.class));
	}

	@Test
	public void unscheduleJobDelegatesToQuartz() throws Exception {
		new QuartzJobScheduler().unscheduleJob("uuid", "customer");

		verify(scheduler).unscheduleJob(new TriggerKey("uuid", "customer"));
	}

	@Test
	public void unscheduleJobWrapsSchedulerException() throws Exception {
		doThrow(new SchedulerException("boom")).when(scheduler).unscheduleJob(any(TriggerKey.class));
		QuartzJobScheduler quartzJobScheduler = new QuartzJobScheduler();

		assertThrows(DomainException.class, () -> quartzJobScheduler.unscheduleJob("uuid", "customer"));
	}

	@Test
	public void unscheduleReportDoesNothingWhenSchedulerIsNull() throws Exception {
		setScheduler(null);

		new QuartzJobScheduler().unscheduleReport("uuid", "customer");

		verify(scheduler, never()).unscheduleJob(any(TriggerKey.class));
	}

	@Test
	public void unscheduleReportDelegatesToQuartz() throws Exception {
		new QuartzJobScheduler().unscheduleReport("uuid", "customer");

		verify(scheduler).unscheduleJob(new TriggerKey("uuid", "customer"));
	}

	@Test
	public void unscheduleReportWrapsSchedulerException() throws Exception {
		doThrow(new SchedulerException("boom")).when(scheduler).unscheduleJob(any(TriggerKey.class));
		QuartzJobScheduler quartzJobScheduler = new QuartzJobScheduler();

		assertThrows(DomainException.class, () -> quartzJobScheduler.unscheduleReport("uuid", "customer"));
	}

	@Test
	public void cancelJobDelegatesToQuartzInterrupt() throws Exception {
		when(scheduler.interrupt("instance")).thenReturn(true);

		assertThat(new QuartzJobScheduler().cancelJob("instance"), is(true));
	}

	@Test
	public void cancelJobWrapsUnableToInterruptException() throws Exception {
		doThrow(new UnableToInterruptJobException("nope")).when(scheduler).interrupt("instance");
		QuartzJobScheduler quartzJobScheduler = new QuartzJobScheduler();

		assertThrows(DomainException.class, () -> quartzJobScheduler.cancelJob("instance"));
	}

	@Test
	public void validateMetaDataSchedulesInternalJob() throws Exception {
		new QuartzJobScheduler().validateMetaData();

		ArgumentCaptor<org.quartz.JobDetail> detail = ArgumentCaptor.forClass(org.quartz.JobDetail.class);
		ArgumentCaptor<Trigger> trigger = ArgumentCaptor.forClass(Trigger.class);
		verify(scheduler).scheduleJob(detail.capture(), trigger.capture());
		assertSame(ValidateMetaDataJob.class, detail.getValue().getJobClass());
		assertThat(trigger.getValue().getKey().getGroup(), is("INTERNAL"));
	}

	@Test
	public void validateMetaDataWrapsSchedulerException() throws Exception {
		doThrow(new SchedulerException("boom")).when(scheduler).scheduleJob(any(org.quartz.JobDetail.class), any(Trigger.class));
		QuartzJobScheduler quartzJobScheduler = new QuartzJobScheduler();

		assertThrows(IllegalStateException.class, quartzJobScheduler::validateMetaData);
	}

	@Test
	public void shutdownCancelsRunningSkyveJobsAndStopsScheduler() throws Exception {
		JobExecutionContext context = mock(JobExecutionContext.class);
		TestQuartzJob job = new TestQuartzJob();
		when(context.getJobInstance()).thenReturn(job);
		when(context.getFireInstanceId()).thenReturn("fire-id");
		when(scheduler.getCurrentlyExecutingJobs()).thenReturn(java.util.List.of(context));
		when(scheduler.interrupt("fire-id")).thenReturn(true);

		new QuartzJobScheduler().shutdown();

		verify(scheduler).interrupt("fire-id");
		verify(scheduler).shutdown();
	}

	@Test
	public void shutdownStillStopsSchedulerWhenRunningJobsCannotBeRead() throws Exception {
		when(scheduler.getCurrentlyExecutingJobs()).thenThrow(new SchedulerException("boom"));

		new QuartzJobScheduler().shutdown();

		verify(scheduler).shutdown();
	}

	@Test
	public void getCustomerRunningJobsReturnsOnlySkyveJobsForCurrentCustomer() throws Exception {
		User currentUser = user("customer");
		bindPersistenceWithUser(currentUser);
		TestQuartzJob customerJob = new TestQuartzJob();
		customerJob.setDisplayName("Customer Job");
		customerJob.setPercentComplete(42);
		customerJob.getLog().add("running");
		JobExecutionContext matchingContext = context(customerJob, "customer", "fire-1", currentUser);
		JobExecutionContext otherCustomerContext = context(new TestQuartzJob(), "other", "fire-2", currentUser);
		JobExecutionContext nonSkyveContext = mock(JobExecutionContext.class);
		when(nonSkyveContext.getJobInstance()).thenReturn(mock(org.quartz.Job.class));
		when(scheduler.getCurrentlyExecutingJobs()).thenReturn(List.of(matchingContext, otherCustomerContext, nonSkyveContext));

		List<JobDescription> result = new QuartzJobScheduler().getCustomerRunningJobs();

		assertThat(result.size(), is(1));
		JobDescription description = result.get(0);
		assertThat(description.getUser(), is(sameInstance(currentUser)));
		assertThat(description.getName(), is("Customer Job"));
		assertThat(description.getPercentComplete(), is(42));
		assertThat(description.getLogging(), is("running\n"));
		assertThat(description.getInstanceId(), is("fire-1"));
	}

	@Test
	public void getCustomerRunningJobsWrapsSchedulerException() throws Exception {
		bindPersistenceWithUser(user("customer"));
		when(scheduler.getCurrentlyExecutingJobs()).thenThrow(new SchedulerException("boom"));
		QuartzJobScheduler quartzJobScheduler = new QuartzJobScheduler();

		assertThrows(DomainException.class, quartzJobScheduler::getCustomerRunningJobs);
	}

	@Test
	public void preRestorePausesInternalJobsAndUnschedulesCustomerTriggers() throws Exception {
		boolean originalJobScheduler = UtilImpl.JOB_SCHEDULER;
		try {
			UtilImpl.JOB_SCHEDULER = true;
			bindPersistenceWithUser(user("customer"));
			when(scheduler.getCurrentlyExecutingJobs()).thenReturn(Collections.emptyList());
			when(scheduler.getTriggerKeys(any(GroupMatcher.class))).thenReturn(Set.of(new TriggerKey("job-1", "customer")));

			new QuartzJobScheduler().preRestore();

			verify(scheduler).pauseJobs(jobGroupMatcher("INTERNAL"));
			verify(scheduler).getTriggerKeys(triggerGroupMatcher("customer"));
			verify(scheduler).unscheduleJobs(List.of(new TriggerKey("job-1", "customer")));
		}
		finally {
			UtilImpl.JOB_SCHEDULER = originalJobScheduler;
		}
	}

	@Test
	public void preRestoreThrowsValidationExceptionWhenCustomerOrInternalJobIsRunning() throws Exception {
		User user = user("customer");
		bindPersistenceWithUser(user);
		JobExecutionContext context = context(new TestQuartzJob(), "INTERNAL", "fire-1", user);
		when(scheduler.getCurrentlyExecutingJobs()).thenReturn(List.of(context));
		QuartzJobScheduler quartzJobScheduler = new QuartzJobScheduler();

		assertThrows(ValidationException.class, quartzJobScheduler::preRestore);
	}

	@Test
	public void preRestoreWrapsSchedulerException() throws Exception {
		bindPersistenceWithUser(user("customer"));
		when(scheduler.getCurrentlyExecutingJobs()).thenThrow(new SchedulerException("boom"));
		QuartzJobScheduler quartzJobScheduler = new QuartzJobScheduler();

		assertThrows(DomainException.class, quartzJobScheduler::preRestore);
	}

	@Test
	public void runRestoreJobSchedulesRestoreJobWithCurrentUserAndOptions() throws Exception {
		User user = user("customer");
		bindPersistenceWithUser(user);
		Bean options = mock(Bean.class);

		new QuartzJobScheduler().runRestoreJob(options);

		ArgumentCaptor<org.quartz.JobDetail> detail = ArgumentCaptor.forClass(org.quartz.JobDetail.class);
		ArgumentCaptor<Trigger> trigger = ArgumentCaptor.forClass(Trigger.class);
		verify(scheduler).scheduleJob(detail.capture(), trigger.capture());
		assertSame(RestoreJob.class, detail.getValue().getJobClass());
		assertThat(detail.getValue().getKey().getGroup(), is("customer"));
		assertThat(trigger.getValue().getKey().getGroup(), is("customer"));
		assertThat(trigger.getValue().getJobDataMap().get(AbstractSkyveJob.DISPLAY_NAME_JOB_PARAMETER_KEY), is(RestoreJob.class.getSimpleName()));
		assertThat(trigger.getValue().getJobDataMap().get(AbstractSkyveJob.USER_JOB_PARAMETER_KEY), is(sameInstance(user)));
		assertThat(trigger.getValue().getJobDataMap().get(AbstractSkyveJob.BEAN_JOB_PARAMETER_KEY), is(sameInstance(options)));
	}

	@Test
	public void runRestoreJobWrapsSchedulerException() throws Exception {
		bindPersistenceWithUser(user("customer"));
		doThrow(new SchedulerException("boom")).when(scheduler).scheduleJob(any(org.quartz.JobDetail.class), any(Trigger.class));
		QuartzJobScheduler quartzJobScheduler = new QuartzJobScheduler();
		Bean options = mock(Bean.class);

		assertThrows(DomainException.class, () -> quartzJobScheduler.runRestoreJob(options));
	}

	@Test
	public void scheduleReportAddsUserAndDynamicReportBeanToTriggerData() throws Exception {
		User user = user("customer");
		JobDetail detail = reportDetail("report-id", "customer", "Report Sales");
		Trigger trigger = reportTrigger(detail, "report-id", "customer");

		invokeScheduleReport(detail, user, trigger);

		ArgumentCaptor<JobDetail> scheduledDetail = ArgumentCaptor.forClass(JobDetail.class);
		ArgumentCaptor<Trigger> scheduledTrigger = ArgumentCaptor.forClass(Trigger.class);
		verify(scheduler).scheduleJob(scheduledDetail.capture(), scheduledTrigger.capture());
		assertThat(scheduledDetail.getValue(), is(sameInstance(detail)));
		JobDataMap dataMap = scheduledTrigger.getValue().getJobDataMap();
		assertThat(dataMap.get(AbstractSkyveJob.DISPLAY_NAME_JOB_PARAMETER_KEY), is("Report Sales"));
		assertThat(dataMap.get(AbstractSkyveJob.USER_JOB_PARAMETER_KEY), is(sameInstance(user)));
		DynamicBean parameter = (DynamicBean) dataMap.get(AbstractSkyveJob.BEAN_JOB_PARAMETER_KEY);
		assertThat(parameter.getBizModule(), is(AppConstants.ADMIN_MODULE_NAME));
		assertThat(parameter.getBizDocument(), is(AppConstants.REPORT_TEMPLATE_DOCUMENT_NAME));
		assertThat(parameter.getBizId(), is("report-id"));
	}

	@Test
	public void scheduleReportSkipsExpiredTriggerButStillPopulatesJobData() throws Exception {
		User user = user("customer");
		JobDetail detail = reportDetail("report-id", "customer", "Report Sales");
		Trigger expired = TriggerBuilder.newTrigger()
										.withIdentity("report-id", "customer")
										.forJob(detail)
										.startAt(new Date(946_684_800_000L))
										.endAt(new Date(946_684_860_000L))
										.build();

		invokeScheduleReport(detail, user, expired);

		verify(scheduler, never()).scheduleJob(any(JobDetail.class), any(Trigger.class));
		assertThat(expired.getJobDataMap().get(AbstractSkyveJob.DISPLAY_NAME_JOB_PARAMETER_KEY), is("Report Sales"));
		assertThat(expired.getJobDataMap().get(AbstractSkyveJob.USER_JOB_PARAMETER_KEY), is(sameInstance(user)));
		assertThat(((DynamicBean) expired.getJobDataMap().get(AbstractSkyveJob.BEAN_JOB_PARAMETER_KEY)).getBizId(), is("report-id"));
	}

	@Test
	public void scheduleReportConvertsDuplicateTriggerToValidationException() throws Exception {
		doThrow(new ObjectAlreadyExistsException("duplicate")).when(scheduler).scheduleJob(any(JobDetail.class), any(Trigger.class));
		JobDetail detail = reportDetail("report-id", "customer", "Report Sales");
		User currentUser = user("customer");
		Trigger trigger = reportTrigger(detail, "report-id", "customer");

		assertThrows(ValidationException.class,
				() -> invokeScheduleReport(detail, currentUser, trigger));
	}

	@Test
	public void scheduleReportWrapsSchedulerException() throws Exception {
		doThrow(new SchedulerException("boom")).when(scheduler).scheduleJob(any(JobDetail.class), any(Trigger.class));
		JobDetail detail = reportDetail("report-id", "customer", "Report Sales");
		User currentUser = user("customer");
		Trigger trigger = reportTrigger(detail, "report-id", "customer");

		assertThrows(DomainException.class,
				() -> invokeScheduleReport(detail, currentUser, trigger));
	}

	@Test
	public void postRestoreAlwaysResumesInternalJobs() throws Exception {
		boolean originalJobScheduler = UtilImpl.JOB_SCHEDULER;
		try {
			UtilImpl.JOB_SCHEDULER = false;

			new QuartzJobScheduler().postRestore(true);

			verify(scheduler).resumeJobs(jobGroupMatcher("INTERNAL"));
		}
		finally {
			UtilImpl.JOB_SCHEDULER = originalJobScheduler;
		}
	}

	@Test
	public void postRestoreRefreshesScheduledJobsAndReportsWhenRestoreSucceeded() throws Exception {
		boolean originalJobScheduler = UtilImpl.JOB_SCHEDULER;
		ProvidedRepository originalRepository = ProvidedRepositoryFactory.get();
		ProvidedRepository repository = mock(ProvidedRepository.class);
		when(repository.retrieveAllScheduledJobsForAllCustomers()).thenReturn(List.of());
		when(repository.retrieveAllScheduledReportsForAllCustomers()).thenReturn(List.of());
		try {
			UtilImpl.JOB_SCHEDULER = true;
			ProvidedRepositoryFactory.set(repository);

			new QuartzJobScheduler().postRestore(true);

			verify(scheduler).resumeJobs(jobGroupMatcher("INTERNAL"));
			verify(repository).retrieveAllScheduledJobsForAllCustomers();
			verify(repository).retrieveAllScheduledReportsForAllCustomers();
		}
		finally {
			UtilImpl.JOB_SCHEDULER = originalJobScheduler;
			ProvidedRepositoryFactory.set(originalRepository);
		}
	}

	@Test
	public void postRestoreWrapsSchedulerException() throws Exception {
		doThrow(new SchedulerException("boom")).when(scheduler).resumeJobs(any(GroupMatcher.class));
		QuartzJobScheduler quartzJobScheduler = new QuartzJobScheduler();

		assertThrows(DomainException.class, () -> quartzJobScheduler.postRestore(false));
	}

	private static Trigger scheduledTrigger() throws Exception {
		ArgumentCaptor<Trigger> trigger = ArgumentCaptor.forClass(Trigger.class);
		verify(getScheduler()).scheduleJob(trigger.capture());
		return trigger.getValue();
	}

	private static JobDetail reportDetail(String name, String group, String description) {
		return JobBuilder.newJob(TestQuartzJob.class)
							.withIdentity(name, group)
							.withDescription(description)
							.build();
	}

	private static Trigger reportTrigger(JobDetail detail, String name, String group) {
		return TriggerBuilder.newTrigger()
								.withIdentity(name, group)
								.forJob(detail)
								.withSchedule(CronScheduleBuilder.cronSchedule("0 0 12 * * ?"))
								.build();
	}

	private static void invokeScheduleReport(JobDetail detail, User user, Trigger trigger) throws Exception {
		Method method = QuartzJobScheduler.class.getDeclaredMethod("scheduleReport", JobDetail.class, User.class, Trigger.class);
		method.setAccessible(true);
		try {
			method.invoke(null, detail, user, trigger);
		}
		catch (InvocationTargetException e) {
			Throwable cause = e.getCause();
			if (cause instanceof RuntimeException runtimeException) {
				throw runtimeException;
			}
			if (cause instanceof Error error) {
				throw error;
			}
			throw e;
		}
	}

	private static JobMetaData job(String moduleName, String jobName, String displayName) {
		JobMetaData result = mock(JobMetaData.class);
		when(result.getOwningModuleName()).thenReturn(moduleName);
		when(result.getName()).thenReturn(jobName);
		when(result.getLocalisedDisplayName()).thenReturn(displayName);
		return result;
	}

	private static User user(String customerName) {
		Customer customer = mock(Customer.class);
		when(customer.getName()).thenReturn(customerName);
		User result = mock(User.class);
		when(result.getCustomer()).thenReturn(customer);
		return result;
	}

	private static JobExecutionContext context(AbstractSkyveJob job, String triggerGroup, String fireInstanceId, User user) {
		Trigger trigger = mock(Trigger.class);
		when(trigger.getKey()).thenReturn(new TriggerKey("trigger", triggerGroup));
		JobDataMap dataMap = new JobDataMap();
		dataMap.put(AbstractSkyveJob.USER_JOB_PARAMETER_KEY, user);
		JobExecutionContext context = mock(JobExecutionContext.class);
		when(context.getJobInstance()).thenReturn(job);
		when(context.getTrigger()).thenReturn(trigger);
		when(context.getMergedJobDataMap()).thenReturn(dataMap);
		when(context.getFireInstanceId()).thenReturn(fireInstanceId);
		return context;
	}

	private static GroupMatcher<JobKey> jobGroupMatcher(String expectedGroup) {
		return org.mockito.ArgumentMatchers.argThat((ArgumentMatcher<GroupMatcher<JobKey>>) matcher -> matcher != null && expectedGroup.equals(matcher.getCompareToValue()));
	}

	private static GroupMatcher<TriggerKey> triggerGroupMatcher(String expectedGroup) {
		return org.mockito.ArgumentMatchers.argThat((ArgumentMatcher<GroupMatcher<TriggerKey>>) matcher -> matcher != null && expectedGroup.equals(matcher.getCompareToValue()));
	}

	private static void bindPersistenceWithUser(User user) throws Exception {
		AbstractPersistence persistence = mock(AbstractPersistence.class);
		when(persistence.getUser()).thenReturn(user);
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

	private static Scheduler getScheduler() throws Exception {
		Field field = schedulerField();
		return (Scheduler) field.get(null);
	}

	private static void setScheduler(Scheduler value) throws Exception {
		Field field = schedulerField();
		field.set(null, value);
	}

	private static Field schedulerField() throws NoSuchFieldException {
		Field field = QuartzJobScheduler.class.getDeclaredField("JOB_SCHEDULER");
		field.setAccessible(true);
		return field;
	}

	public static final class TestBackgroundTask extends ViewBackgroundTask<Bean> {
		@Override
		public void execute(Bean bean) {
			// no-op
		}
	}

	public static final class TestQuartzJob extends AbstractSkyveJob {
		@Override
		public void execute() {
			// no-op
		}

		@Override
		public void execute(org.skyve.job.Job job) {
			// no-op
		}

		@Override
		public String cancel() {
			return null;
		}

		@Override
		public boolean shouldRollbackOnCancel() {
			return false;
		}

		@Override
		public boolean shouldBeSilent() {
			return true;
		}
	}

}
