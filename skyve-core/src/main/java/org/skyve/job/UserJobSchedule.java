package org.skyve.job;


import org.skyve.metadata.user.User;

import jakarta.annotation.Nonnull;

/**
 * Binds a {@link JobSchedule} to the user security context that must be used
 * when executing that schedule.
 *
 * <p>The pairing expresses an execution invariant: job logic must run as the
 * supplied {@link User}, not as an ambient/system principal. Callers should
 * treat both constructor arguments as required and non-null for the lifetime
 * of the instance.
 *
 * <p>Threading: effectively immutable after construction and safe for
 * concurrent read access if referenced as published immutable state.
 *
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
