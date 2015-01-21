package org.skyve.wildcat.job;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.UUID;

import org.quartz.CronTrigger;
import org.quartz.JobDataMap;
import org.quartz.JobDetail;
import org.quartz.ObjectAlreadyExistsException;
import org.quartz.Scheduler;
import org.quartz.SchedulerException;
import org.quartz.SchedulerFactory;
import org.quartz.SimpleTrigger;
import org.quartz.Trigger;
import org.quartz.TriggerUtils;
import org.quartz.impl.StdSchedulerFactory;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.ValidationException;
import org.skyve.domain.types.DateTime;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.module.Job;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.wildcat.bind.BindUtil;
import org.skyve.wildcat.metadata.repository.AbstractRepository;
import org.skyve.wildcat.persistence.AbstractPersistence;
import org.skyve.wildcat.util.SQLMetaDataUtil;
import org.skyve.wildcat.util.UtilImpl;

public class JobScheduler {
	private static Scheduler JOB_SCHEDULER = null;
	private static final WildcatTriggerListener BIZHUB_TRIGGER_LISTENER = new WildcatTriggerListener();

	public static void init() {
		SchedulerFactory sf = new StdSchedulerFactory();
		try {
			JOB_SCHEDULER = sf.getScheduler();
			JOB_SCHEDULER.addGlobalTriggerListener(BIZHUB_TRIGGER_LISTENER);
			JOB_SCHEDULER.start();
		}
		catch (SchedulerException e) {
			throw new IllegalStateException("Could not start scheduler", e);
		}
		
		try {
			// Add metadata jobs
			AbstractRepository repository = AbstractRepository.get();
			for (String moduleName : repository.getAllVanillaModuleNames()) {
				Module module = repository.getModule(null, moduleName);
				addJobs(module);
			}

			// Add triggers
			List<Bean> jobSchedules = SQLMetaDataUtil.retrieveAllJobSchedulesForAllCustomers();
			for (Bean jobSchedule : jobSchedules) {
				scheduleJob(jobSchedule, (User) BindUtil.get(jobSchedule, "user"));
			}

			scheduleInternalJobs();
		}
		catch (Exception e) {
			throw new IllegalStateException("Could not schedule jobs", e);
		}
	}
	
	public static void dispose() {
		try {
			JOB_SCHEDULER.shutdown();
		}
		catch (SchedulerException e) {
			e.printStackTrace();
		}
	}

	private static void addJobs(Module module) 
	throws Exception {
		for (Job job : module.getJobs()) {
			Class<?> jobClass = Thread.currentThread().getContextClassLoader().loadClass(job.getClassName());
			JobDetail detail = new JobDetail(job.getName(), module.getName(), jobClass);
			detail.setDurability(true); // remain in store even when no triggers are using it

			JOB_SCHEDULER.addJob(detail, false);
		}
	}

	private static void scheduleInternalJobs()
	throws Exception {
		JobDetail detail = new JobDetail("CMS Garbage Collection",
											Scheduler.DEFAULT_GROUP,
											ContentGarbageCollectionJob.class);
		detail.setDurability(true);
		SimpleTrigger trigger = new SimpleTrigger("CMS Garbage Collection Trigger",
													Scheduler.DEFAULT_GROUP,
													new Date(),
													null,
													SimpleTrigger.REPEAT_INDEFINITELY,
													3607000L); // note: 1 hr 7 secs
		JOB_SCHEDULER.scheduleJob(detail, trigger);
	}

	/**
	 * Run a job once. 
	 * The job disappears from the Scheduler once it is run and a record of the run in placed in admin.Job. 
	 * User must look in admin to see if job was successful.
	 * 
	 * @param job The job to run
	 * @param bean 	The job parameter - can be null.
	 * @param user	The user to run the job as.
	 * 
	 * @throws Exception Anything.
	 */
	public static void runOneShotJob(Job job, Bean parameter, User user)
	throws Exception {
		Trigger trigger = TriggerUtils.makeImmediateTrigger(UUID.randomUUID().toString(), 0, 0);
		trigger.setGroup(user.getCustomer().getName());
		trigger.setJobGroup(job.getOwningModuleName());
		trigger.setJobName(job.getName());

		scheduleJob(job, parameter, user, trigger, null);
	}

	/**
	 * Extra parameter gives polling UIs the chance to display the results of the job.
	 * 
	 * @param job The job to run
	 * @param bean 	The job parameter - can be null.
	 * @param user	The user to run the job as.
	 * @param sleepAtEndInSeconds Set this 5 secs higher than the polling time of the UI
	 * @throws Exception
	 */
	public static void runOneShotJob(Job job, Bean parameter, User user, int sleepAtEndInSeconds)
	throws Exception {
		Trigger trigger = TriggerUtils.makeImmediateTrigger(UUID.randomUUID().toString(), 0, 0);
		trigger.setGroup(user.getCustomer().getName());
		trigger.setJobGroup(job.getOwningModuleName());
		trigger.setJobName(job.getName());

		scheduleJob(job, parameter, user, trigger, new Integer(sleepAtEndInSeconds));
	}
	
	public static void scheduleJob(Bean jobSchedule,
									User user)
	throws Exception {
		String bizId = (String) BindUtil.get(jobSchedule, Bean.DOCUMENT_ID);
		String jobName = (String) BindUtil.get(jobSchedule, "jobName");
		
		int dotIndex = jobName.indexOf('.');
		String moduleName = jobName.substring(0, dotIndex);
		jobName = jobName.substring(dotIndex + 1);

		Customer customer = user.getCustomer();
		Module module = customer.getModule(moduleName);
		Job job = module.getJob(jobName);
		
		Date sqlStartTime = (Date) BindUtil.get(jobSchedule, "startTime");
		DateTime startTime = (sqlStartTime == null) ? null : new DateTime(sqlStartTime.getTime());
		Date sqlEndTime = (Date) BindUtil.get(jobSchedule, "endTime");
		DateTime endTime = (sqlEndTime == null) ? null : new DateTime(sqlEndTime.getTime());
		String cronExpression = (String) BindUtil.get(jobSchedule, "cronExpression");
		
		CronTrigger trigger = new CronTrigger();
		trigger.setCronExpression(cronExpression);
		trigger.setGroup(customer.getName());
		trigger.setName(bizId);
		trigger.setJobGroup(moduleName);
		trigger.setJobName(jobName);
		
		if (startTime != null) {
			trigger.setStartTime(startTime);
		}
		if (endTime != null) {
			trigger.setEndTime(endTime);
		}
		
		scheduleJob(job, null, user, trigger, null);
	}
	
	private static void scheduleJob(Job job, 
										Bean parameter, 
										User user, 
										Trigger trigger, 
										Integer sleepAtEndInSeconds)
	throws Exception {
		// Add the job data
		JobDataMap map = trigger.getJobDataMap();
		map.put(AbstractWildcatJob.DISPLAY_NAME_JOB_PARAMETER_KEY, job.getDisplayName());
		map.put(AbstractWildcatJob.BEAN_JOB_PARAMETER_KEY, parameter);
		map.put(AbstractWildcatJob.USER_JOB_PARAMETER_KEY, user);
		if (sleepAtEndInSeconds != null) {
			map.put(AbstractWildcatJob.SLEEP_JOB_PARAMETER_KEY, sleepAtEndInSeconds);
		}

		StringBuilder trace = new StringBuilder(128);
		
		// check end time
		Date currentTime = new Date();
		Date triggerEndTime = trigger.getEndTime(); 
		Date firstFireTime = trigger.getFireTimeAfter(currentTime);

		if ((triggerEndTime != null) && triggerEndTime.before(currentTime)) {
			trace.append("No scheduling required (end time = ").append(triggerEndTime).append(" of ");
		}
		else {
			// Set the first fire time (if job is scheduled and recurring)
			if (firstFireTime != null) {
				trace.append("Scheduled execution of ");
				trigger.setStartTime(firstFireTime);
			}
			else {
				trace.append("Immediate execution of ");
			}
			
			// schedule
			try {
				JOB_SCHEDULER.scheduleJob(trigger);
			}
			catch (ObjectAlreadyExistsException e) {
				throw new ValidationException(new Message("You are already running job " + job.getDisplayName() +
															".  Look in the jobs list for more information."));
			}
		}
		
		trace.append(trigger.getJobGroup()).append('.').append(trigger.getJobName());
		trace.append(": ").append(job.getDisplayName()).append(" with trigger ");
		trace.append(trigger.getGroup() + '/' + trigger.getName());
		if (firstFireTime != null) {
			trace.append(" first at ").append(firstFireTime);
		}
		UtilImpl.LOGGER.info(trace.toString());
	}

	public static void unscheduleJob(Bean jobSchedule, Customer customer)
	throws Exception {
		JOB_SCHEDULER.unscheduleJob(jobSchedule.getBizId(), customer.getName());
	}

	public static List<JobDescription> getCustomerRunningJobs()
	throws Exception {
		User user = AbstractPersistence.get().getUser();
		List<JobDescription> result = new ArrayList<>();

		String customerName = user.getCustomer().getName();

		for (String triggerName : JOB_SCHEDULER.getTriggerNames(customerName)) {
			Trigger trigger = JOB_SCHEDULER.getTrigger(triggerName, customerName);
			AbstractWildcatJob job = BIZHUB_TRIGGER_LISTENER.getRunningJob(customerName, trigger.getName());
			if (job != null) {
				JobDescription jd = new JobDescription();
				jd.setStartTime(job.getStartTime());
				jd.setName(job.getDisplayName());
				jd.setPercentComplete(job.getPercentComplete());
				jd.setLogging(job.createLogDescriptionString());

				result.add(jd);
			}
		}

		return result;
	}
}
