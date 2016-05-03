package org.skyve.impl.job;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.quartz.InterruptableJob;
import org.quartz.JobExecutionContext;
import org.quartz.JobExecutionException;
import org.quartz.UnableToInterruptJobException;
import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.messages.MessageException;
import org.skyve.domain.messages.Message;
import org.skyve.domain.types.Timestamp;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.job.JobStatus;
import org.skyve.metadata.MetaData;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;

public abstract class AbstractSkyveJob implements InterruptableJob, MetaData {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = -1272267642792331001L;

	static final String DISPLAY_NAME_JOB_PARAMETER_KEY = "displayName";
	static final String BEAN_JOB_PARAMETER_KEY = "bean";
	static final String USER_JOB_PARAMETER_KEY = "user";
	static final String SLEEP_JOB_PARAMETER_KEY = "sleep";

	private String displayName;
	private Timestamp startTime = new Timestamp();
	private Timestamp endTime;
	private int percentComplete = 0;
	private JobStatus status = null;
	private List<String> log = Collections.synchronizedList(new ArrayList<String>());
	private Bean bean;

	public String getDisplayName() {
		return displayName;
	}

	public void setDisplayName(String displayName) {
		this.displayName = displayName;
	}

	public final int getPercentComplete() {
		return percentComplete;
	}

	public final void setPercentComplete(int percentComplete) {
		this.percentComplete = percentComplete;
	}

	public final Timestamp getStartTime() {
		return startTime;
	}

	public final Timestamp getEndTime() {
		return endTime;
	}

	public final JobStatus getStatus() {
		return status;
	}

	public final List<String> getLog() {
		return log;
	}

	public final String createLogDescriptionString() {
		StringBuilder sb = new StringBuilder(256);
		synchronized (log) {
			for (String logEntry : log) {
				sb.append(logEntry).append('\n');
			}
		}

		return sb.toString();
	}

	public final Bean getBean() {
		return bean;
	}

	public final void setBean(Bean bean) {
		this.bean = bean;
	}

	public abstract void execute() throws Exception;

	public abstract String cancel();

	@Override
	public final void interrupt() 
	throws UnableToInterruptJobException {
		String cantCancelReason = cancel();
		if (cantCancelReason != null) {
			throw new UnableToInterruptJobException(cantCancelReason);
		}
	}

	@Override
	public final void execute(JobExecutionContext context) 
	throws JobExecutionException {
		AbstractPersistence persistence = AbstractPersistence.get();
		User user = null;
		Customer customer = null;
		displayName = (String) context.getMergedJobDataMap().get(DISPLAY_NAME_JOB_PARAMETER_KEY);
		Integer sleepInSeconds = (Integer) context.getMergedJobDataMap().get(SLEEP_JOB_PARAMETER_KEY);

		try {
			bean = (Bean) context.getMergedJobDataMap().get(BEAN_JOB_PARAMETER_KEY);
			user = (User) context.getMergedJobDataMap().get(USER_JOB_PARAMETER_KEY);
			customer = user.getCustomer();
			persistence.setUser(user);
			persistence.begin();

			execute();
			status = JobStatus.complete;
		}
		catch (Throwable t) {
			status = JobStatus.failed;
			persistence.rollback();

			if (t instanceof MessageException) {
				getLog().add("Job Failed :- ");
				for (Message em : ((MessageException) t).getMessages()) {
					getLog().add(em.getErrorMessage());
				}
			}
			else if (t.getMessage() != null) {
				getLog().add("Job Failed :- " + t.getMessage());
			}

			throw new JobExecutionException("Job failed", t);
		}
		finally {
			endTime = new Timestamp();

			persistence.evictAllCached();
			persistence.commit(false);

			persistence.begin();
			// save the job to the database
			if ((customer == null) || (user == null)) {
				throw new JobExecutionException("Could not insert completed job in the database as customer or user is undefined");
			}

			try {
				Module module = customer.getModule("admin");
				Document document = module.getDocument(customer, "Job");
				PersistentBean job = document.newInstance(user);

				BindUtil.set(job, "startTime", getStartTime());
				BindUtil.set(job, "displayName", getDisplayName());
				BindUtil.set(job, "status", status.toString());
				BindUtil.set(job, "endTime", getEndTime());
				BindUtil.set(job, "percentComplete", new Integer(getPercentComplete()));
				BindUtil.set(job, "log", createLogDescriptionString());
				
				persistence.save(document, job);
			}
			catch (Exception e) {
				throw new JobExecutionException("Could not insert completed job in the database", e);
			}

			persistence.evictAllCached();
			persistence.commit(true);

			if ((sleepInSeconds != null) && (sleepInSeconds.intValue() > 0)) {
				try {
					Thread.sleep(30000);
				}
				catch (InterruptedException e) {
					// Do nothing - can't do anything here - its all too late
				}
			}
		}
	}
}
