package modules.admin.Jobs;

import java.util.List;

import modules.admin.domain.Job;
import modules.admin.domain.Jobs;

import org.skyve.wildcat.job.JobDescription;
import org.skyve.wildcat.job.JobScheduler;
import org.skyve.CORE;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.persistence.Persistence;

public class JobsBizlet extends Bizlet<Jobs> {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = 2374495221430654562L;

	@Override
	public Jobs newInstance(Jobs jobs) throws Exception {
		refresh(jobs);
		return jobs;
	}
	
	public static final void refresh(Jobs jobs) throws Exception {
		Persistence persistence = CORE.getPersistence();
		User user = persistence.getUser();
		Customer customer = user.getCustomer();
		Module module = customer.getModule(Job.MODULE_NAME);
		Document document = module.getDocument(customer, Job.DOCUMENT_NAME);
		
		List<Job> runningJobs = jobs.getRunningJobs();
		runningJobs.clear();
		
		for (JobDescription jd : JobScheduler.getCustomerRunningJobs()) {
			// the job could be finished but the thread is still sleeping waiting for the last UI poll
			if (jd.getStatus() == null) { // not finished
				Job job = document.newInstance(user);
				job.setStartTime(jd.getStartTime());
				job.setDisplayName(jd.getName());
				job.setPercentComplete(new Integer(jd.getPercentComplete()));
				job.setLog(jd.getLogging());
				
				runningJobs.add(job);
			}
		}
	}
}
