package org.skyve.impl.job;

import org.quartz.Job;
import org.quartz.JobExecutionContext;
import org.quartz.JobExecutionException;
import org.skyve.impl.web.UserAgent;

/**
 * This job loads the BrowsCap csv.
 * Skyve Request processing can't start until this is loaded. 
 * This is used for user agent sniffing for ux/ui selection. 
 * 
 * @author mike
 */
public class LoadBrowsCapJob implements Job {
	@Override
	public void execute(JobExecutionContext context)
	throws JobExecutionException {
		UserAgent.init();
	}
}
