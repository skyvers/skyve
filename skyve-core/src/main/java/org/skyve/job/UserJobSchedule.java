package org.skyve.job;

import org.skyve.metadata.user.User;

import jakarta.annotation.Nonnull;

/**
 * Associates a {@link JobSchedule} with a specific {@link User}.
 * <p>
 * This class binds a job schedule to the user context under which
 * the scheduled job should execute.
 * </p>
 * 
 * @author Mike
 * @since 9.5.0
 */
public class UserJobSchedule {
	private JobSchedule jobSchedule;
	private User user;
	
	/**
	 * Constructs a new UserJobSchedule with the specified schedule and user.
	 * 
	 * @param jobSchedule the job schedule configuration, must not be null
	 * @param user the user under whose context the job will execute, must not be null
	 */
	public UserJobSchedule(@Nonnull JobSchedule jobSchedule, @Nonnull User user) {
		this.jobSchedule = jobSchedule;
		this.user = user;
	}
	
	/**
	 * Returns the job schedule configuration.
	 * 
	 * @return the job schedule, never null
	 */
	public @Nonnull JobSchedule getJobSchedule() {
		return jobSchedule;
	}
	
	/**
	 * Returns the user under whose context the job will execute.
	 * 
	 * @return the user, never null
	 */
	public @Nonnull User getUser() {
		return user;
	}
}
