package org.skyve.job;

import org.skyve.domain.types.Timestamp;

public class JobDescription {
	private Timestamp startTime;
	private Timestamp endTime;
	private String name;
	private int percentComplete;
	private String logging;
	private JobStatus status;

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public int getPercentComplete() {
		return percentComplete;
	}

	public void setPercentComplete(int percentComplete) {
		this.percentComplete = percentComplete;
	}

	public String getLogging() {
		return logging;
	}

	public void setLogging(String logging) {
		this.logging = logging;
	}

	public JobStatus getStatus() {
		return status;
	}

	public void setStatus(JobStatus status) {
		this.status = status;
	}

	public Timestamp getEndTime() {
		return endTime;
	}

	public void setEndTime(Timestamp endTime) {
		this.endTime = endTime;
	}

	public Timestamp getStartTime() {
		return startTime;
	}

	public void setStartTime(Timestamp startTime) {
		this.startTime = startTime;
	}
}
