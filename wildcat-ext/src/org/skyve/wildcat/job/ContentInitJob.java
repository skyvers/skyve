package org.skyve.wildcat.job;

import org.quartz.Job;
import org.quartz.JobExecutionContext;
import org.quartz.JobExecutionException;
import org.skyve.EXT;
import org.skyve.wildcat.content.AbstractContentManager;
import org.skyve.wildcat.util.UtilImpl;

/**
 * This job fires up the content management system - this can take a while 
 * and we don't want to block the app server deployment process.
 * 
 * @author sandsm01
 */
public class ContentInitJob implements Job {
	@Override
	public void execute(JobExecutionContext context)
	throws JobExecutionException {
		try (AbstractContentManager cm = (AbstractContentManager) EXT.newContentManager()) {
			cm.init();
		}
		catch (Exception e) {
			UtilImpl.LOGGER.info("Could not startup the content manager - this is non-fatal but requires investigation");
			e.printStackTrace();
		}
	}
}