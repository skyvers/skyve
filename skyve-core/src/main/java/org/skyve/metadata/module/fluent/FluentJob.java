package org.skyve.metadata.module.fluent;

import org.skyve.impl.metadata.module.JobMetaDataImpl;
import org.skyve.metadata.module.JobMetaData;

public class FluentJob {
	private JobMetaDataImpl job = new JobMetaDataImpl();
	
	public FluentJob() {
		// nothing to see
	}

	public FluentJob(JobMetaData job) {
		name(job.getName());
		displayName(job.getDisplayName());
		description(job.getDescription());
		className(job.getClassName());
	}
	
	public FluentJob name(String name) {
		job.setName(name);
		return this;
	}
	
	public FluentJob displayName(String displayName) {
		job.setDisplayName(displayName);
		return this;
	}
	
	public FluentJob description(String description) {
		job.setDescription(description);
		return this;
	}
	
	public FluentJob className(String fullyQualifiedClassName) {
		job.setClassName(fullyQualifiedClassName);
		return this;
	}
	
	public JobMetaDataImpl get() {
		return job;
	}
}
