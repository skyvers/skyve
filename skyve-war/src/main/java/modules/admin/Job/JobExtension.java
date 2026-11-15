package modules.admin.Job;

import java.util.List;

import org.skyve.CORE;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.module.JobMetaData;
import org.skyve.metadata.module.Module;

import modules.admin.domain.Job;

/**
 * Extends {@link Job} with rerun eligibility logic for admin UI actions.
 */
public class JobExtension extends Job {
	private static final long serialVersionUID = 3821211430282929423L;

	/**
	 * Business logic for the {@link Job#isRerunnabble()} condition.
	 * 
	 * Checks whether this job has a status (complete/failed), was started without a bean (see
	 * {@link #startedWithABean()}), and has a unique display name so we know we can identify the job to rerun.
	 * 
	 * @return {@code true} when this job can be rerun, otherwise {@code false}
	 */
	public boolean rerunnable() {
		return (getStatus() != null) && (! startedWithABean()) && (jobsWithThisName() == 1);
	}

	/**
	 * Counts the customer's jobs with this job's display name, module by module, and stops after a module once it has
	 * found more than one.
	 *
	 * @return the number of jobs found
	 */
	private int jobsWithThisName() {
		int result = 0;
		Customer c = CORE.getCustomer();
		List<Module> modules = c.getModules();
		for (Module m : modules) {
			List<JobMetaData> jobs = m.getJobs();

			for (JobMetaData j : jobs) {
				if (j.getDisplayName().equals(this.getDisplayName())) {
					result++;
				}
			}

			if (result > 1) {
				return result;
			}
		}
		return result;
	}

	/**
	 * Returns whether this job was started with a bean, such as the Control Panel that Generate Test Data starts from.
	 *
	 * <p>A rerun cannot give the job that bean back. The job record keeps only the bean's identity (its bizId, module
	 * and document), not its state: a transient bean (a Control Panel) is gone, and a persistent one would be read back
	 * as it is now saved, without the transient values the job was started with (Data Maintenance's backup and audit
	 * truncation options, say). Rerunning with a bean that lacks them could do something else entirely, such as
	 * truncate the whole audit log instead of the audits selected. So only a job started without a bean, such as a
	 * scheduled one, can be rerun, and it is rerun without a bean, as it first ran.
	 *
	 * @return {@code true} if the job record names the bean the job was started with
	 */
	public boolean startedWithABean() {
		return (getBeanBizId() != null);
	}
}
