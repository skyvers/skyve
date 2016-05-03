package org.skyve.impl.job;

import java.util.TreeMap;

import org.quartz.JobExecutionContext;
import org.quartz.Trigger;
import org.quartz.TriggerListener;
import org.skyve.impl.job.AbstractSkyveJob;

public final class SkyveTriggerListener implements TriggerListener {
	/**
	 * This is a map customer -> (triggerName -> execution context).
	 */
	private TreeMap<String, TreeMap<String, JobExecutionContext>> runningJobs = new TreeMap<>();

	@Override
	public String getName() {
		return "SkyveTriggerListener";
	}

	@Override
	public synchronized void triggerComplete(Trigger trigger, JobExecutionContext context, int triggerInstructionCode) {
		String customerName = trigger.getGroup();
		String triggerName = trigger.getName();

		TreeMap<String, JobExecutionContext> jobs = runningJobs.get(customerName);
		if (jobs != null) {
			jobs.remove(triggerName);
			if (jobs.isEmpty()) {
				runningJobs.remove(customerName);
			}
		}
	}

	@Override
	public synchronized void triggerFired(Trigger trigger, JobExecutionContext context) {
		String customerName = trigger.getGroup();
		String triggerName = trigger.getName();
		TreeMap<String, JobExecutionContext> jobs = runningJobs.get(customerName);
		if (jobs == null) {
			jobs = new TreeMap<>();
			runningJobs.put(customerName, jobs);
		}

		jobs.put(triggerName, context);
	}

	@Override
	public void triggerMisfired(Trigger trigger) {
		// do nothing
	}

	@Override
	public boolean vetoJobExecution(Trigger trigger, JobExecutionContext context) {
		return false;
	}

	public AbstractSkyveJob getRunningJob(String customerName, String triggerName) {
		AbstractSkyveJob result = null;

		TreeMap<String, JobExecutionContext> jobs = runningJobs.get(customerName);
		if (jobs != null) {
			JobExecutionContext context = jobs.get(triggerName);
			if (context != null) {
				result = (AbstractSkyveJob) context.getJobInstance();
			}
		}

		return result;
	}
}
