package org.skyve.impl.job;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;

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
import org.skyve.domain.Bean;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.ValidationException;
import org.skyve.domain.types.DateTime;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.util.SQLMetaDataUtil;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.AbstractWebContext;
import org.skyve.job.Job;
import org.skyve.job.JobDescription;
import org.skyve.job.JobScheduler;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.module.JobMetaData;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.repository.ProvidedRepository;
import org.skyve.metadata.user.User;
import org.skyve.util.Util;
import org.skyve.web.BackgroundTask;

public class QuartzJobScheduler implements JobScheduler {
	private static final QuartzJobScheduler INSTANCE = new QuartzJobScheduler();

	private static Scheduler JOB_SCHEDULER = null;
	private static final String REPORT_JOB_CLASS_NAME = "modules.admin.ReportTemplate.jobs.ReportJob";

	private QuartzJobScheduler() {
		// nothing to see here
	}
	
	public static QuartzJobScheduler get() {
		return INSTANCE;
	}
	
	@Override
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
				List<Bean> jobSchedules = SQLMetaDataUtil.retrieveAllJobSchedulesForAllCustomers().stream()
						.filter(js -> ! Boolean.TRUE.equals(BindUtil.get(js, "disabled")))
						.collect(Collectors.toList());
				for (Bean jobSchedule : jobSchedules) {
					scheduleJob(jobSchedule, (User) BindUtil.get(jobSchedule, "user"));
				}

				// Add report triggers
				final List<Bean> reportSchedules = SQLMetaDataUtil.retrieveAllReportSchedulesForAllCustomers();
				for (Bean reportSchedule : reportSchedules) {
					addReportJob((String) BindUtil.get(reportSchedule, "name"));
					if (Boolean.TRUE.equals(BindUtil.get(reportSchedule, "scheduled"))) {
						scheduleReport(reportSchedule, (User) BindUtil.get(reportSchedule, "user"));
					}
				}
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
			JOB_SCHEDULER.shutdown();
		}
		catch (SchedulerException e) {
			e.printStackTrace();
		}
	}

	private static void addJobs(Module module)
	throws Exception {
		for (JobMetaData job : module.getJobs()) {
			@SuppressWarnings("unchecked")
			Class<? extends Job> jobClass = (Class<? extends Job>) Thread.currentThread().getContextClassLoader().loadClass(job.getClassName());
			// remain in store even when no triggers are using it
			JobDetail detail = JobBuilder.newJob(jobClass).withIdentity(job.getName(), module.getName()).storeDurably().build();

			JOB_SCHEDULER.addJob(detail, false);
		}
	}

	@Override
	public void addReportJob(String reportName) {
		try {
			@SuppressWarnings("unchecked")
			Class<? extends Job> jobClass = (Class<? extends Job>) Thread.currentThread().getContextClassLoader().loadClass(REPORT_JOB_CLASS_NAME);
			// remain in store even when no triggers are using it
			JobDetail detail = JobBuilder.newJob(jobClass).withIdentity(reportName, REPORTS_GROUP).storeDurably().build();
	
			JOB_SCHEDULER.addJob(detail, true);
		}
		catch (Exception e) {
			throw new DomainException("Could not add report job " + reportName, e);
		}
	}

	private static void scheduleInternalJobs()
	throws Exception {
		// initialise the CMS in a 1 shot immediate job
		JobDetail detail = JobBuilder.newJob(ContentStartupJob.class)
										.withIdentity("CMS Startup", Scheduler.DEFAULT_GROUP)
										.storeDurably(false)
										.build();
		Trigger trigger = TriggerBuilder.newTrigger()
											.forJob(detail)
											.withIdentity("CMS Startup trigger", Scheduler.DEFAULT_GROUP)
											.startNow()
											.build();
		JOB_SCHEDULER.scheduleJob(detail, trigger);

		// Do CMS garbage collection as schedule in the CRON expression in the application properties file
		detail = JobBuilder.newJob(ContentGarbageCollectionJob.class)
							.withIdentity("CMS Garbage Collection", Scheduler.DEFAULT_GROUP)
							.storeDurably()
							.build();
		Date in5Minutes = new Date(System.currentTimeMillis() + 300000);
		trigger = TriggerBuilder.newTrigger()
									.forJob(detail)
									.withIdentity("CMS Garbage Collection Trigger", Scheduler.DEFAULT_GROUP)
									.startAt(in5Minutes) // start in 5 minutes once the CMS has settled down
									.withSchedule(CronScheduleBuilder.cronSchedule(UtilImpl.CONTENT_GC_CRON))
									.build();
		try {
			JOB_SCHEDULER.scheduleJob(detail, trigger);
			Util.LOGGER.info("CMS Garbage Collection Job scheduled for " + trigger.getNextFireTime());
		}
		catch (SchedulerException e) {
			Util.LOGGER.severe("CMS Garbage Collection Job was not scheduled because - " + e.getLocalizedMessage());
		}

		// Do expired state eviction as schedule in the CRON expression in the application properties file
		if (UtilImpl.STATE_EVICT_CRON != null) {
			detail = JobBuilder.newJob(EvictStateJob.class)
								.withIdentity("Evict Expired State", Scheduler.DEFAULT_GROUP)
								.storeDurably()
								.build();
			trigger = TriggerBuilder.newTrigger()
										.forJob(detail)
										.withIdentity("Evict Expired State Trigger", Scheduler.DEFAULT_GROUP)
										.startAt(in5Minutes) // start in 5 minutes once everything has settled down
										.withSchedule(CronScheduleBuilder.cronSchedule(UtilImpl.STATE_EVICT_CRON))
										.build();
			try {
				JOB_SCHEDULER.scheduleJob(detail, trigger);
				Util.LOGGER.info("Evict Expired State Job scheduled for " + trigger.getNextFireTime());
			}
			catch (SchedulerException e) {
				Util.LOGGER.severe("Evict Expired State Job was not scheduled because - " + e.getLocalizedMessage());
			}
		}
		else {
			Util.LOGGER.info("Evict Expired State Job was not scheduled because there was no conversations.evictCron in the json.");
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
										.withIdentity(UUID.randomUUID().toString())
										.storeDurably(false)
										.build();
		Trigger trigger = TriggerBuilder.newTrigger()
											.withIdentity(UUID.randomUUID().toString())
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
	public void scheduleOneShotJob(JobMetaData job, Bean parameter, User user, Date when) {
		Trigger trigger = TriggerBuilder.newTrigger()
													.withIdentity(UUID.randomUUID().toString(), user.getCustomer().getName())
													.forJob(job.getName(), job.getOwningModuleName())
													.startAt(when)
													.build();
		scheduleJob(job, parameter, user, trigger, null);
	}
	
	@Override
	public void scheduleJob(Bean jobSchedule, User user) {
		String bizId = (String) BindUtil.get(jobSchedule, Bean.DOCUMENT_ID);
		String jobName = (String) BindUtil.get(jobSchedule, "jobName");
		
		int dotIndex = jobName.indexOf('.');
		String moduleName = jobName.substring(0, dotIndex);
		jobName = jobName.substring(dotIndex + 1);

		Customer customer = user.getCustomer();
		Module module = customer.getModule(moduleName);
		JobMetaData job = module.getJob(jobName);
		if (job == null) { // no job defined
			throw new MetaDataException(String.format("Job %s.%s in the data store (ADM_JobSchedule) is not defined in the skyve metadata.",
														moduleName, jobName)); 
		}
		
		Date sqlStartTime = (Date) BindUtil.get(jobSchedule, "startTime");
		DateTime startTime = (sqlStartTime == null) ? null : new DateTime(sqlStartTime.getTime());
		Date sqlEndTime = (Date) BindUtil.get(jobSchedule, "endTime");
		DateTime endTime = (sqlEndTime == null) ? null : new DateTime(sqlEndTime.getTime());
		String cronExpression = (String) BindUtil.get(jobSchedule, "cronExpression");
		
		TriggerBuilder<CronTrigger> tb = TriggerBuilder.newTrigger()
														.withIdentity(bizId, customer.getName())
														.forJob(jobName, moduleName)
														.withSchedule(CronScheduleBuilder.cronSchedule(cronExpression));

		if (startTime != null) {
			tb.startAt(startTime);
		}
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
			trace.append("No scheduling required (end time = ").append(triggerEndTime).append(" of ");
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
		
		JobKey jobKey = mutableTrigger.getJobKey();
		trace.append(jobKey.getGroup()).append('.').append(jobKey.getName());
		trace.append(": ").append(job.getLocalisedDisplayName()).append(" with trigger ");
		TriggerKey key = mutableTrigger.getKey();
		trace.append(key.getGroup() + '/' + key.getName());
		if (firstFireTime != null) {
			trace.append(" first at ").append(firstFireTime);
		}
		UtilImpl.LOGGER.info(trace.toString());
	}

	@Override
	public void unscheduleJob(Bean jobSchedule, Customer customer) {
		String bizId = jobSchedule.getBizId();
		String customerName = customer.getName();
		try {
			JOB_SCHEDULER.unscheduleJob(new TriggerKey(bizId, customerName));
			Util.LOGGER.info("Unscheduled Job " + bizId + " for customer " + customerName);
		}
		catch (SchedulerException e) {
			throw new DomainException("Cannot unschedule job " + bizId + " for customer " + customerName, e);
		}
	}

	@Override
	public void scheduleReport(Bean reportSchedule, User user) {
		String bizId = (String) BindUtil.get(reportSchedule, Bean.DOCUMENT_ID);
		String reportName = (String) BindUtil.get(reportSchedule, "name");

		Customer customer = user.getCustomer();

		Date sqlStartTime = (Date) BindUtil.get(reportSchedule, "startTime");
		DateTime startTime = (sqlStartTime == null) ? null : new DateTime(sqlStartTime.getTime());
		Date sqlEndTime = (Date) BindUtil.get(reportSchedule, "endTime");
		DateTime endTime = (sqlEndTime == null) ? null : new DateTime(sqlEndTime.getTime());
		String cronExpression = (String) BindUtil.get(reportSchedule, "cronExpression");
		
		TriggerBuilder<CronTrigger> tb = TriggerBuilder.newTrigger()
														.withIdentity(bizId, customer.getName())
														.forJob(reportName, REPORTS_GROUP)
														.withSchedule(CronScheduleBuilder.cronSchedule(cronExpression));

		if (startTime != null) {
			tb.startAt(startTime);
		}
		if (endTime != null) {
			tb.endAt(endTime);
		}

		scheduleReport((String) BindUtil.get(reportSchedule, "name"), reportSchedule, user, tb.build(), null);
	}

	private static void scheduleReport(String jobName,
									   Bean parameter,
									   User user,
									   Trigger trigger,
									   Integer sleepAtEndInSeconds) {
		Trigger mutableTrigger = trigger;
		
		// Add the job data
		JobDataMap map = mutableTrigger.getJobDataMap();
		map.put(AbstractSkyveJob.DISPLAY_NAME_JOB_PARAMETER_KEY, jobName);
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
			trace.append("No scheduling required (end time = ").append(triggerEndTime).append(" of ");
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
				throw new ValidationException(new Message("You are already running job " + jobName +
						".  Look in the jobs list for more information."));
			}
			catch (SchedulerException e) {
				throw new DomainException("Cannot schedule job " + jobName, e);
			}
		}

		JobKey jobKey = mutableTrigger.getJobKey();
		trace.append(jobKey.getGroup()).append('.').append(jobKey.getName());
		trace.append(": ").append(jobName).append(" with trigger ");
		TriggerKey key = mutableTrigger.getKey();
		trace.append(key.getGroup() + '/' + key.getName());
		if (firstFireTime != null) {
			trace.append(" first at ").append(firstFireTime);
		}
		UtilImpl.LOGGER.info(trace.toString());
	}

	@Override
	public void unscheduleReport(Bean reportSchedule, Customer customer) {
		if (JOB_SCHEDULER != null) {
			String bizId = reportSchedule.getBizId();
			String customerName = customer.getName();
			try {
				JOB_SCHEDULER.unscheduleJob(new TriggerKey(bizId, customerName));
				Util.LOGGER.info("Unscheduled report " + bizId + " for customer " + customerName);
			}
			catch (SchedulerException e) {
				throw new DomainException("Cannot unschedule report " + bizId + " for customer " + customerName, e);
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
				AbstractSkyveJob job = (AbstractSkyveJob) context.getJobInstance();
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
		catch (SchedulerException e) {
			throw new DomainException("Cannot determine running jobs", e);
		}
		
		return result;
	}

	@Override
	public boolean cancelJob(String instanceId) {
		try {
			boolean result = JOB_SCHEDULER.interrupt(instanceId);
			Util.LOGGER.info((result ? "Cancelled job " : "Unable to cancel job ") + instanceId);
			return result;
		}
		catch (UnableToInterruptJobException e) {
			throw new DomainException("Cannot cancel job " + instanceId, e);
		}
	}
}
