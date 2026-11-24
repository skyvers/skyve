package org.skyve.metadata.module;

import org.skyve.CORE;
import org.skyve.metadata.customer.Customer;

import jakarta.annotation.Nonnull;

/**
 * A compile-time reference to a job declared in a module - implemented by the generated
 * per-module job enums (e.g. <code>AdminJob.J_BULK_USER_CREATION</code>) so code can resolve a
 * job's metadata without a module name and job name String pair.
 * <p/>
 * See {@link ModuleMember} for when the String form is still required.
 */
public interface ModuleJob extends ModuleMember {
	/**
	 * The job name within the module.
	 *
	 * @return the job name
	 */
	@Nonnull String jobName();

	/**
	 * The name a job schedule stores for this module job.
	 *
	 * @return <code>moduleName() + '.' + jobName()</code> - e.g. <code>admin.jBackup</code>
	 */
	default @Nonnull String scheduledJobName() {
		return moduleName() + '.' + jobName();
	}

	/**
	 * Resolves this reference to the job metadata through the current user's customer, so
	 * per-customer module overrides are honoured.
	 *
	 * @return the job metadata
	 * @throws org.skyve.metadata.MetaDataException if the module declares no such job for the
	 * 				customer
	 */
	default @Nonnull JobMetaData toJob() {
		return toJob(CORE.getCustomer());
	}

	/**
	 * Resolves this reference to the job metadata through the given customer, so per-customer
	 * module overrides are honoured.
	 *
	 * @param customer	the customer to resolve the job for
	 * @return the job metadata
	 * @throws org.skyve.metadata.MetaDataException if the module declares no such job for the
	 * 				customer
	 */
	default @Nonnull JobMetaData toJob(@Nonnull Customer customer) {
		return toModule(customer).getJob(jobName());
	}
}
