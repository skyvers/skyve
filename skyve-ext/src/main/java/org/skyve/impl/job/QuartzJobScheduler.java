package org.skyve.impl.job;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.UUID;

import org.quartz.CronScheduleBuilder;
import org.quartz.CronTrigger;
import org.quartz.JobBuilder;
import org.quartz.JobDataMap;
import org.quartz.JobDetail;
import org.quartz.JobExecutionContext;
import org.quartz.JobKey;
import org.quartz.ObjectAlreadyExistsException;
import org.quartz.Scheduler;
import org.quartz.SchedulerException;
import org.quartz.SchedulerFactory;
import org.quartz.Trigger;
import org.quartz.TriggerBuilder;
import org.quartz.TriggerKey;
import org.quartz.UnableToInterruptJobException;
import org.quartz.impl.StdSchedulerFactory;
import org.quartz.impl.matchers.GroupMatcher;
import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.DynamicBean;
import org.skyve.domain.app.AppConstants;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.ValidationException;
import org.skyve.impl.archive.job.ArchiveJob;
import org.skyve.impl.backup.RestoreJob;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.UtilImpl.ArchiveConfig.ArchiveSchedule;
import org.skyve.impl.web.AbstractWebContext;
import org.skyve.job.Job;
import org.skyve.job.JobDescription;
import org.skyve.job.JobSchedule;
import org.skyve.job.JobScheduler;
import org.skyve.job.UserJobSchedule;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.module.JobMetaData;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.repository.ProvidedRepository;
import org.skyve.metadata.user.User;
import org.skyve.util.Util;
import org.skyve.web.BackgroundTask;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class QuartzJobScheduler implements JobScheduler {
	private static final String REPORT_JOB_CLASS_NAME = "modules.admin.ReportTemplate.jobs.ReportJob";
	private static final String INTERNAL_JOB_GROUP_NAME = "INTERNAL";
	private static final Logger LOGGER = LoggerFactory.getLogger(QuartzJobScheduler.class);

	// The quartz job scheduler singleton
	@SuppressWarnings("java:S3008") // treated as constant
	private static Scheduler JOB_SCHEDULER = null;

	QuartzJobScheduler() {
		// nothing to see here
	}

	@Override
	@SuppressWarnings("java:S2696") // startup initialises the static
	public void startup() {
		SchedulerFactory sf = new StdSchedulerFactory();
		try {
			JOB_SCHEDULER = sf.getScheduler();
			JOB_SCHEDULER.start();
		}
		catch (SchedulerException e) {
			throw new IllegalStateException("Could not start scheduler", e);
		}

		try {
			// Add metadata jobs
			ProvidedRepository repository = ProvidedRepositoryFactory.get();
			for (String moduleName : repository.getAllVanillaModuleNames()) {
				Module module = repository.getModule(null, moduleName);
				addJobs(module);
			}

			// Add triggers if this Skyve instance is able to schedule jobs
			if (UtilImpl.JOB_SCHEDULER) {
				scheduleJobsAndReports();
			}

			scheduleInternalJobs();
		}
		catch (Exception e) {
			throw new IllegalStateException("Could not schedule jobs", e);
		}
	}
	
	@Override
	public void shutdown() {
		try {
			// NB Could be null if startup failed
			if (JOB_SCHEDULER != null) {
				cancelAllRunningJobs();
				JOB_SCHEDULER.shutdown();
			}
		}
		catch (SchedulerException e) {
			LOGGER.error("Cannot shutdown Job Scheduler", e);
		}
	}

	@SuppressWarnings("java:S1141") // simple enough
	private void cancelAllRunningJobs() {
		try {
			for (JobExecutionContext context : JOB_SCHEDULER.getCurrentlyExecutingJobs()) {
				org.quartz.Job instance = context.getJobInstance();
				if (instance instanceof AbstractSkyveJob job) {
					String id = context.getFireInstanceId();
					try {
						cancelJob(id);
					}
					catch (Exception e) {
						LOGGER.error("Job Scheduler Shutdown: Cannot cancel job {}: {}", id, job.getDisplayName(), e);
					}
				}
			}
		}
		catch (SchedulerException e) {
			LOGGER.error("Job Scheduler Shutdown: Cannot determine running jobs", e);
		}
	}
	
	private static void addJobs(Module module)
	throws SchedulerException, ClassNotFoundException {
		for (JobMetaData job : module.getJobs()) {
			@SuppressWarnings("unchecked")
			Class<? extends Job> jobClass = (Class<? extends Job>) Thread.currentThread().getContextClassLoader().loadClass(job.getClassName());
			// remain in store even when no triggers are using it
			JobDetail detail = JobBuilder.newJob(jobClass).withIdentity(job.getName(), module.getName()).withDescription(job.getDisplayName()).storeDurably().build();

			JOB_SCHEDULER.addJob(detail, false);
		}
	}

	private void scheduleJobsAndReports() {
		ProvidedRepository repository = ProvidedRepositoryFactory.get();

		// Add job triggers
		List<UserJobSchedule> jobSchedules = repository.retrieveAllScheduledJobsForAllCustomers();
		for (UserJobSchedule ujs : jobSchedules) {
			scheduleJob(ujs.getJobSchedule(), ujs.getUser());
		}

		// Add report jobs and triggers
		final List<UserJobSchedule> reportSchedules = repository.retrieveAllScheduledReportsForAllCustomers();
		for (UserJobSchedule ujs : reportSchedules) {
			scheduleReport(ujs.getJobSchedule(), ujs.getUser());
		}
	}
	
	private static void scheduleInternalJobs()
	throws SchedulerException {
		// Initialise BrowsCap load in a 1 shot immediate job
		JobDetail detail = JobBuilder.newJob(LoadBrowsCapJob.class)
										.withIdentity("Load BrowsCap", INTERNAL_JOB_GROUP_NAME)
										.storeDurably(false)
										.build();
		Trigger trigger = TriggerBuilder.newTrigger()
							.forJob(detail)
							.withIdentity("Load Browscap trigger", INTERNAL_JOB_GROUP_NAME)
							.startNow()
							.build();
		JOB_SCHEDULER.scheduleJob(detail, trigger);

		// Initialise the CMS in a 1 shot immediate job
		detail = JobBuilder.newJob(ContentStartupJob.class)
							.withIdentity("CMS Startup", INTERNAL_JOB_GROUP_NAME)
							.storeDurably(false)
							.build();
		trigger = TriggerBuilder.newTrigger()
									.forJob(detail)
									.withIdentity("CMS Startup trigger", INTERNAL_JOB_GROUP_NAME)
									.startNow()
									.build();
		JOB_SCHEDULER.scheduleJob(detail, trigger);

		// Do CMS garbage collection as schedule in the CRON expression in the application properties file
		// starting in 5 minutes time to ensure the system has settled down
		detail = JobBuilder.newJob(ContentGarbageCollectionJob.class)
							.withIdentity("CMS Garbage Collection", INTERNAL_JOB_GROUP_NAME)
							.storeDurably()
							.build();
		Date in5Minutes = new Date(System.currentTimeMillis() + 300000);
		trigger = TriggerBuilder.newTrigger()
									.forJob(detail)
									.withIdentity("CMS Garbage Collection Trigger", INTERNAL_JOB_GROUP_NAME)
									.startAt(in5Minutes) // start in 5 minutes
									// Pausing the internal group will not fire any missed executions
									.withSchedule(CronScheduleBuilder.cronSchedule(UtilImpl.CONTENT_GC_CRON).withMisfireHandlingInstructionDoNothing())
									.build();
		try {
			JOB_SCHEDULER.scheduleJob(detail, trigger);
			LOGGER.info("CMS Garbage Collection Job [cron={}] scheduled for {}", UtilImpl.CONTENT_GC_CRON, trigger.getNextFireTime());
		}
		catch (SchedulerException e) {
			LOGGER.error("CMS Garbage Collection Job was not scheduled because - {}", e.getLocalizedMessage());
		}

		scheduleArchiveJob();

		// Do expired state eviction as schedule in the CRON expression in the application properties file
		// starting in 5 minutes time to ensure the system has settled down
		if (UtilImpl.STATE_EVICT_CRON != null) {
			detail = JobBuilder.newJob(EvictStateJob.class)
								.withIdentity("Evict Expired State", INTERNAL_JOB_GROUP_NAME)
								.storeDurably()
								.build();
			trigger = TriggerBuilder.newTrigger()
										.forJob(detail)
										.withIdentity("Evict Expired State Trigger", INTERNAL_JOB_GROUP_NAME)
										.startAt(in5Minutes) // start in 5 minutes
										// Pausing the internal group will not fire any missed executions
										.withSchedule(CronScheduleBuilder.cronSchedule(UtilImpl.STATE_EVICT_CRON).withMisfireHandlingInstructionDoNothing())
										.build();
			try {
				JOB_SCHEDULER.scheduleJob(detail, trigger);
				LOGGER.info("Evict Expired State Job [cron={}] scheduled for {}", UtilImpl.STATE_EVICT_CRON, trigger.getNextFireTime());
			}
			catch (SchedulerException e) {
				LOGGER.error("Evict Expired State Job was not scheduled because - {}", e.getLocalizedMessage());
			}
		}
		else {
			LOGGER.info("Evict Expired State Job was not scheduled because there was no conversations.evictCron in the json.");
		}
	}

    /**
     * Schedule the archive job, if configured in the application JSON.
     */
    private static void scheduleArchiveJob() {

        if (!UtilImpl.ARCHIVE_CONFIG.cronScheduleEnabled()) {
            LOGGER.debug("ArchiveJob not configured to run on a schedule");
            return;
        }

        ArchiveSchedule scheduleConfig = UtilImpl.ARCHIVE_CONFIG.schedule();
        String cronSchedule = scheduleConfig.cron();
        LOGGER.debug("Scheduling ArchiveJob to run with schedule: '{}'", cronSchedule);

        JobDetail archiveJobDetail = JobBuilder.newJob(ArchiveJob.class)
                                               .withIdentity("Archive Job", INTERNAL_JOB_GROUP_NAME)
                                               .storeDurably()
                                               .build();

        CronTrigger trigger = TriggerBuilder.newTrigger()
                                            .forJob(archiveJobDetail)
                                            .withIdentity("Archive Job Trigger", INTERNAL_JOB_GROUP_NAME)
        									// Pausing the internal group will not fire any missed executions
                                            .withSchedule(CronScheduleBuilder.cronSchedule(cronSchedule).withMisfireHandlingInstructionDoNothing())
                                            .startNow()
                                            .build();

        trigger.getJobDataMap()
               .put(AbstractSkyveJob.DISPLAY_NAME_JOB_PARAMETER_KEY, ArchiveJob.class.getSimpleName());
        trigger.getJobDataMap()
               .put(AbstractSkyveJob.USER_JOB_PARAMETER_KEY, scheduleConfig.getUser());

        try {
            JOB_SCHEDULER.scheduleJob(archiveJobDetail, trigger);
        } catch (SchedulerException e) {
            LOGGER.atWarn()
                  .setCause(e)
                  .log("Could not schedule archive job");
        }
    }

    @Override
	public void runOneShotJob(JobMetaData job, Bean parameter, User user) {
		Trigger trigger = TriggerBuilder.newTrigger()
											.withIdentity(UUID.randomUUID().toString(), user.getCustomer().getName())
											.forJob(job.getName(), job.getOwningModuleName())
											.build();
		scheduleJob(job, parameter, user, trigger, null);
	}

	@Override
	public void runOneShotJob(JobMetaData job, Bean parameter, User user, int sleepAtEndInSeconds) {
		Trigger trigger = TriggerBuilder.newTrigger()
											.withIdentity(UUID.randomUUID().toString(), user.getCustomer().getName())
											.forJob(job.getName(), job.getOwningModuleName())
											.build();
		scheduleJob(job, parameter, user, trigger, Integer.valueOf(sleepAtEndInSeconds));
	}
	
	@Override
	public <T extends Bean> void runBackgroundTask(Class<? extends BackgroundTask<T>> taskClass, User user, String webId) {
		@SuppressWarnings("unchecked")
		Class<? extends org.quartz.Job> jobClass = (Class<? extends org.quartz.Job>) taskClass;
		JobDetail detail = JobBuilder.newJob(jobClass)
										.withIdentity(UUID.randomUUID().toString(), Scheduler.DEFAULT_GROUP)
										.storeDurably(false)
										.build();
		Trigger trigger = TriggerBuilder.newTrigger()
											.withIdentity(UUID.randomUUID().toString(), Scheduler.DEFAULT_GROUP)
											.forJob(detail)
											.build();
		JobDataMap map = trigger.getJobDataMap();
		map.put(AbstractSkyveJob.USER_JOB_PARAMETER_KEY, user);
		map.put(AbstractWebContext.CONTEXT_NAME, webId);
		
		try {
			JOB_SCHEDULER.scheduleJob(detail, trigger);
		}
		catch (SchedulerException e) {
			throw new DomainException("Cannot run background task " + taskClass, e);
		}
	}
	
	@Override
	public void runContentGarbageCollector() {
		JobDetail detail = JobBuilder.newJob(ContentGarbageCollectionJob.class)
												.withIdentity(UUID.randomUUID().toString(), INTERNAL_JOB_GROUP_NAME)
												.storeDurably(false)
												.build();
		Trigger trigger = TriggerBuilder.newTrigger()
											.withIdentity(UUID.randomUUID().toString(), INTERNAL_JOB_GROUP_NAME)
											.forJob(detail)
											.build();
		try {
			JOB_SCHEDULER.scheduleJob(detail, trigger);
		}
		catch (SchedulerException e) {
			throw new DomainException("Cannot run content garbage collector", e);
		}
	}

	@Override
	public void scheduleOneShotJob(JobMetaData job, Bean parameter, User user, Date when) {
		Trigger trigger = TriggerBuilder.newTrigger()
													.withIdentity(UUID.randomUUID().toString(), user.getCustomer().getName())
													.forJob(job.getName(), job.getOwningModuleName())
													.startAt(when)
													.build();
		scheduleJob(job, parameter, user, trigger, null);
	}
	
	@Override
	public void scheduleJob(JobSchedule jobSchedule, User user) {
		String jobName = jobSchedule.getJobName();
		int dotIndex = jobName.indexOf('.');
		String moduleName = jobName.substring(0, dotIndex);
		jobName = jobName.substring(dotIndex + 1);

		Customer customer = user.getCustomer();
		Module module = customer.getModule(moduleName);
		JobMetaData job = module.getJob(jobName);
		
		TriggerBuilder<CronTrigger> tb = TriggerBuilder.newTrigger()
														.withIdentity(jobSchedule.getUuid(), customer.getName())
														.forJob(jobName, moduleName)
														.withSchedule(CronScheduleBuilder.cronSchedule(jobSchedule.getCronExpression()));

		Date startTime = jobSchedule.getStartTime();
		if (startTime != null) {
			tb.startAt(startTime);
		}
		Date endTime = jobSchedule.getEndTime();
		if (endTime != null) {
			tb.endAt(endTime);
		}
		
		scheduleJob(job, null, user, tb.build(), null);
	}
	
	private static void scheduleJob(JobMetaData job,
										Bean parameter,
										User user,
										Trigger trigger, 
										Integer sleepAtEndInSeconds) {
		Trigger mutableTrigger = trigger;
		
		// Add the job data
		JobDataMap map = mutableTrigger.getJobDataMap();
		map.put(AbstractSkyveJob.DISPLAY_NAME_JOB_PARAMETER_KEY, job.getLocalisedDisplayName());
		map.put(AbstractSkyveJob.BEAN_JOB_PARAMETER_KEY, parameter);
		map.put(AbstractSkyveJob.USER_JOB_PARAMETER_KEY, user);
		if (sleepAtEndInSeconds != null) {
			map.put(AbstractSkyveJob.SLEEP_JOB_PARAMETER_KEY, sleepAtEndInSeconds);
		}

		StringBuilder trace = new StringBuilder(128);
		
		// check end time
		Date currentTime = new Date();
		Date triggerEndTime = mutableTrigger.getEndTime(); 
		Date firstFireTime = mutableTrigger.getFireTimeAfter(currentTime);

		if ((triggerEndTime != null) && triggerEndTime.before(currentTime)) {
			trace.append("No scheduling required (end time = ").append(triggerEndTime).append(") of ");
		}
		else {
			// Set the first fire time (if job is scheduled and recurring)
			if (firstFireTime != null) {
				trace.append("Scheduled execution of ");
				mutableTrigger = mutableTrigger.getTriggerBuilder().startAt(firstFireTime).build();
			}
			else {
				trace.append("Immediate execution of ");
			}
			
			// schedule
			try {
				JOB_SCHEDULER.scheduleJob(mutableTrigger);
			}
			catch (@SuppressWarnings("unused") ObjectAlreadyExistsException e) {
				throw new ValidationException(new Message("You are already running job " + job.getLocalisedDisplayName() +
															".  Look in the jobs list for more information."));
			}
			catch (SchedulerException e) {
				throw new DomainException("Cannot schedule job " + job.getLocalisedDisplayName(), e);
			}
		}
		
		if (LOGGER.isInfoEnabled()) {
			JobKey jobKey = mutableTrigger.getJobKey();
			trace.append(jobKey.getGroup()).append('.').append(jobKey.getName());
			trace.append(": ").append(job.getLocalisedDisplayName()).append(" with trigger ");
			if (mutableTrigger instanceof CronTrigger cronTrigger) {
				trace.append("[cron=").append(cronTrigger.getCronExpression()).append("] ");
			}
			TriggerKey key = mutableTrigger.getKey();
			trace.append(key.getGroup() + '/' + key.getName());
			
			if (firstFireTime != null) {
				trace.append(" first at ").append(firstFireTime);
			}
			LOGGER.info(trace.toString());
		}
	}

	@Override
	public void unscheduleJob(String uuid, String customerName) {
		try {
			JOB_SCHEDULER.unscheduleJob(new TriggerKey(uuid, customerName));
			LOGGER.info("Unscheduled Job {} for customer {}", uuid, customerName);
		}
		catch (SchedulerException e) {
			throw new DomainException("Cannot unschedule job " + uuid + " for customer " + customerName, e);
		}
	}

	@Override
	public void scheduleReport(JobSchedule reportSchedule, User user) {
		String uuid = reportSchedule.getUuid();
		String reportName = reportSchedule.getJobName();

		Customer customer = user.getCustomer();
		String customerName = customer.getName();

		String cronExpression = (String) BindUtil.get(reportSchedule, "cronExpression");
		
		try {
			@SuppressWarnings("unchecked")
			Class<? extends Job> jobClass = (Class<? extends Job>) Thread.currentThread().getContextClassLoader().loadClass(REPORT_JOB_CLASS_NAME);
			JobDetail job = JobBuilder.newJob(jobClass).withIdentity(uuid, customerName).withDescription("Report " + reportName).storeDurably(false).build();
	
			TriggerBuilder<CronTrigger> tb = TriggerBuilder.newTrigger()
															.withIdentity(uuid, customerName)
															.forJob(job)
															.withSchedule(CronScheduleBuilder.cronSchedule(cronExpression));
	
			Date startTime = reportSchedule.getStartTime();
			if (startTime != null) {
				tb.startAt(startTime);
			}
			Date endTime = reportSchedule.getEndTime();
			if (endTime != null) {
				tb.endAt(endTime);
			}
	
			scheduleReport(job, user, tb.build());
		}
		catch (Exception e) {
			throw new DomainException("Could not schedule report " + reportName, e);
		}
	}

	private static void scheduleReport(JobDetail job,
									   User user,
									   Trigger trigger) {
		Trigger mutableTrigger = trigger;
		
		// Add the job data
		JobDataMap map = mutableTrigger.getJobDataMap();
		map.put(AbstractSkyveJob.DISPLAY_NAME_JOB_PARAMETER_KEY, job.getDescription());
		map.put(AbstractSkyveJob.USER_JOB_PARAMETER_KEY, user);

		Map<String, Object> properties = new TreeMap<>();
		properties.put(Bean.DOCUMENT_ID, trigger.getKey().getName());
		Bean parameter = new DynamicBean(AppConstants.ADMIN_MODULE_NAME,
											AppConstants.REPORT_TEMPLATE_DOCUMENT_NAME,
											properties);
		map.put(AbstractSkyveJob.BEAN_JOB_PARAMETER_KEY, parameter);

		StringBuilder trace = new StringBuilder(128);

		// check end time
		Date currentTime = new Date();
		Date triggerEndTime = mutableTrigger.getEndTime(); 
		Date firstFireTime = mutableTrigger.getFireTimeAfter(currentTime);

		if ((triggerEndTime != null) && triggerEndTime.before(currentTime)) {
			trace.append("No scheduling required (end time = ").append(triggerEndTime).append(") of ");
		}
		else {
			// Set the first fire time (if job is scheduled and recurring)
			if (firstFireTime != null) {
				trace.append("Scheduled execution of ");
				mutableTrigger = mutableTrigger.getTriggerBuilder().startAt(firstFireTime).build();
			}
			else {
				trace.append("Immediate execution of ");
			}

			// schedule
			try {
				JOB_SCHEDULER.scheduleJob(job, mutableTrigger);
			}
			catch (@SuppressWarnings("unused") ObjectAlreadyExistsException e) {
				throw new ValidationException(new Message("You are already running job " + job.getDescription() +
															".  Look in the jobs list for more information."));
			}
			catch (SchedulerException e) {
				throw new DomainException("Cannot schedule job " + job.getDescription(), e);
			}
		}

		if (LOGGER.isInfoEnabled()) {
			JobKey jobKey = mutableTrigger.getJobKey();
			trace.append(jobKey.getGroup()).append('.').append(jobKey.getName());
			trace.append(": ").append(job.getDescription()).append(" with trigger ");
			if (mutableTrigger instanceof CronTrigger cronTrigger) {
				trace.append("[cron=").append(cronTrigger.getCronExpression()).append("] ");
			}
			TriggerKey key = mutableTrigger.getKey();
			trace.append(key.getGroup() + '/' + key.getName());
			if (firstFireTime != null) {
				trace.append(" first at ").append(firstFireTime);
			}
			LOGGER.info(trace.toString());
		}
	}

	@Override
	public void unscheduleReport(String uuid, String customerName) {
		if (JOB_SCHEDULER != null) {
			try {
				JOB_SCHEDULER.unscheduleJob(new TriggerKey(uuid, customerName));
				LOGGER.info("Unscheduled report {} for customer {}", uuid, customerName);
			}
			catch (SchedulerException e) {
				throw new DomainException("Cannot unschedule report " + uuid + " for customer " + customerName, e);
			}
		}
	}

	@Override
	public List<JobDescription> getCustomerRunningJobs() {
		User user = AbstractPersistence.get().getUser();
		List<JobDescription> result = new ArrayList<>();

		String customerName = user.getCustomer().getName();

		try {
			for (JobExecutionContext context : JOB_SCHEDULER.getCurrentlyExecutingJobs()) {
				org.quartz.Job instance = context.getJobInstance();
				if (instance instanceof AbstractSkyveJob job) {
					Trigger trigger = context.getTrigger();
					if (customerName.equals(trigger.getKey().getGroup())) {
						JobDescription jd = new JobDescription();
						jd.setUser((User) context.getMergedJobDataMap().get(AbstractSkyveJob.USER_JOB_PARAMETER_KEY));
						jd.setStartTime(job.getStartTime());
						jd.setName(job.getDisplayName());
						jd.setPercentComplete(job.getPercentComplete());
						jd.setLogging(job.createLogDescriptionString());
						jd.setInstanceId(context.getFireInstanceId());
			
						result.add(jd);
					}
				}
			}
		}
		catch (SchedulerException e) {
			throw new DomainException("Cannot determine running jobs", e);
		}
		
		return result;
	}
	
	@Override
	public boolean cancelJob(String instanceId) {
		try {
			boolean result = JOB_SCHEDULER.interrupt(instanceId);
			LOGGER.info("{} job {}", (result ? "Cancelled" : "Unable to cancel"), instanceId);
			return result;
		}
		catch (UnableToInterruptJobException e) {
			throw new DomainException("Cannot cancel job " + instanceId, e);
		}
	}
	
	@Override
	public void validateMetaData() {
		// Initialise validate metadata in a 1 shot immediate job
		try {
			JobDetail detail = JobBuilder.newJob(ValidateMetaDataJob.class)
											.withIdentity("Validate MetaData", INTERNAL_JOB_GROUP_NAME)
											.storeDurably(false)
											.build();
			Trigger trigger = TriggerBuilder.newTrigger()
												.forJob(detail)
												.withIdentity("Validate MetaData trigger", INTERNAL_JOB_GROUP_NAME)
												.startNow()
												.build();
			JOB_SCHEDULER.scheduleJob(detail, trigger);
		}
		catch (Exception e) {
			throw new IllegalStateException("Could not schedule validate meta data job", e);
		}
	}
	
	@Override
	public void preRestore() {
		String customerName = CORE.getCustomer().getName();
		
		try {
			// Check if customer job or system jobs are currently running and throw
			boolean customerOrSystemJobRunning = JOB_SCHEDULER.getCurrentlyExecutingJobs().stream().anyMatch(ctx -> {
				String group = ctx.getTrigger().getKey().getGroup();
				return INTERNAL_JOB_GROUP_NAME.equals(group) || customerName.equals(group);
			});
			if (customerOrSystemJobRunning) {
				throw new ValidationException(new Message(Util.nullSafeI18n("admin.dataMaintenance.actions.restore.jobAlreadyRunningException")));
			}
			
			// Pause system jobs
			JOB_SCHEDULER.pauseJobs(GroupMatcher.groupEquals(INTERNAL_JOB_GROUP_NAME));
	
			// Unschedule customer job triggers
			// Durable jobs (from metadata) will remain for other customers.
			// Non-Durable Report jobs will disappear when the triggers are removed here.
			if (UtilImpl.JOB_SCHEDULER) {
				JOB_SCHEDULER.unscheduleJobs(new ArrayList<>(JOB_SCHEDULER.getTriggerKeys(GroupMatcher.triggerGroupEquals(customerName))));
			}
		}
		catch (SchedulerException e) {
			throw new DomainException("Could not handle the job scheduler prior to restore operation", e);
		}
	}
	
	@Override
	public void runRestoreJob(Bean restoreOptions) {
		String customerName = CORE.getCustomer().getName();
		JobDetail job = JobBuilder.newJob(RestoreJob.class)
									.withIdentity("Restore customer " + customerName, customerName)
									.storeDurably(false)
									.build();
		Trigger trigger = TriggerBuilder.newTrigger()
										.withIdentity(UUID.randomUUID().toString(), customerName)
										.forJob(job)
										.build();
		JobDataMap map = trigger.getJobDataMap();
		map.put(AbstractSkyveJob.DISPLAY_NAME_JOB_PARAMETER_KEY, RestoreJob.class.getSimpleName());
		map.put(AbstractSkyveJob.USER_JOB_PARAMETER_KEY, CORE.getUser());
		map.put(AbstractSkyveJob.BEAN_JOB_PARAMETER_KEY, restoreOptions);

		// Note postRestore called in the job
		try {
			JOB_SCHEDULER.scheduleJob(job, trigger);
		}
		catch (SchedulerException e) {
			throw new DomainException("Could not schedule the restore job", e);
		}
	}
	
	@Override
	public void postRestore(boolean restoreSuccessful) {
		// Resume system jobs
		try {
			JOB_SCHEDULER.resumeJobs(GroupMatcher.groupEquals(INTERNAL_JOB_GROUP_NAME));
		}
		catch (SchedulerException e) {
			throw new DomainException("Could not handle the job scheduler prior to restore operation", e);
		}

		// If successful restore, schedule customer jobs
		if (restoreSuccessful && UtilImpl.JOB_SCHEDULER) {
			scheduleJobsAndReports();
		}
	}
}
