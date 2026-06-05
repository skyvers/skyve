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
import java.util.Date;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.quartz.JobExecutionContext;
import org.quartz.ObjectAlreadyExistsException;
import org.quartz.Scheduler;
import org.quartz.SchedulerException;
import org.quartz.Trigger;
import org.quartz.TriggerKey;
import org.quartz.UnableToInterruptJobException;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.messages.ValidationException;
import org.skyve.impl.web.AbstractWebContext;
import org.skyve.job.JobSchedule;
import org.skyve.job.ViewBackgroundTask;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.module.JobMetaData;
import org.skyve.metadata.module.Module;
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

		DomainException thrown = assertThrows(DomainException.class,
				() -> new QuartzJobScheduler().runOneShotJob(job("mod", "job", "Display"), null, user("customer")));

		assertThat(thrown.getMessage(), is("Cannot schedule job Display"));
	}

	@Test
	public void runOneShotJobConvertsDuplicateTriggerToValidationException() throws Exception {
		doThrow(new ObjectAlreadyExistsException("duplicate")).when(scheduler).scheduleJob(any(Trigger.class));

		assertThrows(ValidationException.class,
				() -> new QuartzJobScheduler().runOneShotJob(job("mod", "job", "Display"), null, user("customer")));
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

		assertThrows(DomainException.class,
				() -> new QuartzJobScheduler().runBackgroundTask(TestBackgroundTask.class, user("customer"), "web-1"));
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

		assertThrows(DomainException.class, () -> new QuartzJobScheduler().runContentGarbageCollector());
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

		assertThrows(DomainException.class, () -> new QuartzJobScheduler().unscheduleJob("uuid", "customer"));
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

		assertThrows(DomainException.class, () -> new QuartzJobScheduler().unscheduleReport("uuid", "customer"));
	}

	@Test
	public void cancelJobDelegatesToQuartzInterrupt() throws Exception {
		when(scheduler.interrupt("instance")).thenReturn(true);

		assertThat(new QuartzJobScheduler().cancelJob("instance"), is(true));
	}

	@Test
	public void cancelJobWrapsUnableToInterruptException() throws Exception {
		doThrow(new UnableToInterruptJobException("nope")).when(scheduler).interrupt("instance");

		assertThrows(DomainException.class, () -> new QuartzJobScheduler().cancelJob("instance"));
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

		assertThrows(IllegalStateException.class, () -> new QuartzJobScheduler().validateMetaData());
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

	private static Trigger scheduledTrigger() throws Exception {
		ArgumentCaptor<Trigger> trigger = ArgumentCaptor.forClass(Trigger.class);
		verify(getScheduler()).scheduleJob(trigger.capture());
		return trigger.getValue();
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
