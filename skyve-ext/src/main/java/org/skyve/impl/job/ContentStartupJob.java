package org.skyve.impl.job;

import org.quartz.Job;
import org.quartz.JobExecutionContext;
import org.quartz.JobExecutionException;
import org.skyve.EXT;
import org.skyve.content.ContentManager;
import org.skyve.impl.util.UtilImpl;

/**
 * This job fires up the content management system - this can take a while 
 * and we don't want to block the app server deployment process.
 * 
 * @author mike
 */
public class ContentStartupJob implements Job {
	@Override
	public void execute(JobExecutionContext context)
	throws JobExecutionException {
		try (ContentManager cm = EXT.newContentManager()) {
			cm.startup();
		}
		catch (Exception e) {
			UtilImpl.LOGGER.info("Could not startup the content manager - this is non-fatal but requires investigation");
			e.printStackTrace();
		}
	}
}
