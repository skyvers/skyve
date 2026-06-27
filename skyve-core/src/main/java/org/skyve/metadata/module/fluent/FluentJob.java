package org.skyve.metadata.module.fluent;

import org.skyve.impl.metadata.module.JobMetaDataImpl;
import org.skyve.metadata.module.JobMetaData;

/**
 * Builds scheduled job metadata for a module definition.
 */
public class FluentJob {
	private JobMetaDataImpl job = null;
	
	/**
	 * Creates a fluent wrapper with a new metadata instance.
	 */
	public FluentJob() {
		job = new JobMetaDataImpl();
	}

	/**
	 * Creates a fluent wrapper around an existing metadata instance.
	 *
	 * @param job The metadata to mutate.
	 */
	public FluentJob(JobMetaDataImpl job) {
		this.job = job;
	}

	/**
	 * Copies job fields from an existing job definition.
	 *
	 * @param job The source definition.
	 * @return this fluent instance.
	 */
	public FluentJob from(@SuppressWarnings("hiding") JobMetaData job) {
		name(job.getName());
		displayName(job.getDisplayName());
		description(job.getDescription());
		className(job.getClassName());
		return this;
	}
	
	/**
	 * Sets the unique job name.
	 *
	 * @param name The job name.
	 * @return this fluent instance.
	 */
	public FluentJob name(String name) {
		job.setName(name);
		return this;
	}
	
	/**
	 * Sets the display label for the job.
	 *
	 * @param displayName The display name.
	 * @return this fluent instance.
	 */
	public FluentJob displayName(String displayName) {
		job.setDisplayName(displayName);
		return this;
	}
	
	/**
	 * Sets the descriptive text for the job.
	 *
	 * @param description The job description.
	 * @return this fluent instance.
	 */
	public FluentJob description(String description) {
		job.setDescription(description);
		return this;
	}
	
	/**
	 * Sets the fully qualified job implementation class.
	 *
	 * @param fullyQualifiedClassName The implementation class name.
	 * @return this fluent instance.
	 */
	public FluentJob className(String fullyQualifiedClassName) {
		job.setClassName(fullyQualifiedClassName);
		return this;
	}
	
	/**
	 * Returns the mutable metadata backing this fluent wrapper.
	 *
	 * @return The job metadata instance.
	 */
	public JobMetaDataImpl get() {
		return job;
	}
}
