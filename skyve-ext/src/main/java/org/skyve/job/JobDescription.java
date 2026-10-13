package org.skyve.job;

import org.skyve.domain.types.Timestamp;
import org.skyve.metadata.user.User;

import jakarta.annotation.Nullable;

/**
 * Carries the current state snapshot for a scheduled or running job.
 *
 * <p>Instances are used for status reporting APIs and include timing, completion,
 * log text, owning user, and runtime instance identity.
 */
public class JobDescription {
	private Timestamp startTime;
	private Timestamp endTime;
	private String name;
	private int percentComplete;
	private String logging;
	private JobStatus status;
	private User user;
	private String instanceId;

	/**
	 * Returns the display name of the job.
	 *
	 * @return Job name
	 */
	public @Nullable String getName() {
		return name;
	}

	/**
	 * Sets the display name of the job.
	 *
	 * @param name Job name
	 */
	public void setName(@Nullable String name) {
		this.name = name;
	}

	/**
	 * Returns completion percentage for the job.
	 *
	 * @return Completion percentage, usually in range 0-100
	 */
	public int getPercentComplete() {
		return percentComplete;
	}

	/**
	 * Sets completion percentage for the job.
	 *
	 * @param percentComplete Completion percentage, usually in range 0-100
	 */
	public void setPercentComplete(int percentComplete) {
		this.percentComplete = percentComplete;
	}

	/**
	 * Returns captured job log output.
	 *
	 * @return Job log text
	 */
	public @Nullable String getLogging() {
		return logging;
	}

	/**
	 * Sets captured job log output.
	 *
	 * @param logging Job log text
	 */
	public void setLogging(@Nullable String logging) {
		this.logging = logging;
	}

	/**
	 * Returns current terminal/non-terminal job status.
	 *
	 * @return Job status value
	 */
	public @Nullable JobStatus getStatus() {
		return status;
	}

	/**
	 * Sets current terminal/non-terminal job status.
	 *
	 * @param status Job status value
	 */
	public void setStatus(@Nullable JobStatus status) {
		this.status = status;
	}

	/**
	 * Returns job completion/end timestamp.
	 *
	 * @return End timestamp, or {@code null} while running
	 */
	public @Nullable Timestamp getEndTime() {
		return endTime;
	}

	/**
	 * Sets job completion/end timestamp.
	 *
	 * @param endTime End timestamp
	 */
	public void setEndTime(@Nullable Timestamp endTime) {
		this.endTime = endTime;
	}

	/**
	 * Returns job start timestamp.
	 *
	 * @return Start timestamp
	 */
	public @Nullable Timestamp getStartTime() {
		return startTime;
	}

	/**
	 * Sets job start timestamp.
	 *
	 * @param startTime Start timestamp
	 */
	public void setStartTime(@Nullable Timestamp startTime) {
		this.startTime = startTime;
	}
	
	/**
	 * Returns user context associated with this job execution.
	 *
	 * @return Job user context
	 */
	public @Nullable User getUser() {
		return user;
	}
	
	/**
	 * Sets user context associated with this job execution.
	 *
	 * @param user Job user context
	 */
	public void setUser(@Nullable User user) {
		this.user = user;
	}

	/**
	 * Returns scheduler/runtime instance identifier for this execution.
	 *
	 * @return Instance identifier
	 */
	public @Nullable String getInstanceId() {
		return instanceId;
	}

	/**
	 * Sets scheduler/runtime instance identifier for this execution.
	 *
	 * @param instanceId Instance identifier
	 */
	public void setInstanceId(@Nullable String instanceId) {
		this.instanceId = instanceId;
	}
}
