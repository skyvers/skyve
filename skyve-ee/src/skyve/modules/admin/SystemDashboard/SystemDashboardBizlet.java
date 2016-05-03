package modules.admin.SystemDashboard;

import java.util.List;

import modules.admin.domain.Job;
import modules.admin.domain.SystemDashboard;

import org.skyve.CORE;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;

public class SystemDashboardBizlet extends Bizlet<SystemDashboard> {

	/**
	 * 
	 */
	private static final long serialVersionUID = 8137854247367804199L;

	@Override
	public SystemDashboard newInstance(SystemDashboard bean) throws Exception {
		Persistence pers = CORE.getPersistence();
		// find jobs
		// TODO - work out why I only get one job
		DocumentQuery qJobs = pers.newDocumentQuery(Job.MODULE_NAME, Job.DOCUMENT_NAME);

		List<Job> jobs = qJobs.beanResults();
		for (Job job : jobs) {
			bean.getJobs().add(job);
		}
		return super.newInstance(bean);
	}

}
