package modules.admin.Job;

import java.util.List;

import org.skyve.CORE;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.module.JobMetaData;

import modules.admin.domain.Job;

public class JobExtension extends Job {

	private static final long serialVersionUID = 3821211430282929423L;

	/**
	 * Business logic for the {@link Job#isRerunnabble()} condition.
	 * 
	 * Checks whether this job has a status (complete/failed), and
	 * has a unique display name so we know we can identify the job to rerun.
	 * 
	 * @return true when this job can be rerun, false otherwise
	 */
	public boolean rerunnable() {
		if (getStatus() != null) {
			// count the number of jobs with this job's name
			int jobCount = 0;
			Customer c = CORE.getCustomer();
			List<org.skyve.metadata.module.Module> modules = c.getModules();
			for (org.skyve.metadata.module.Module m : modules) {
				List<JobMetaData> jobs = m.getJobs();

				for (JobMetaData j : jobs) {
					if (j.getDisplayName().equals(this.getDisplayName())) {
						jobCount++;
					}
				}

				if (jobCount > 1) {
					return false;
				}
			}

			if (jobCount == 1) {
				return true;
			}
		}
		return false;
	}
}
