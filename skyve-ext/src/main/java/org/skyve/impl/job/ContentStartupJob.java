package org.skyve.impl.job;

import org.quartz.Job;
import org.quartz.JobExecutionContext;
import org.quartz.JobExecutionException;
import org.skyve.EXT;
import org.skyve.content.ContentManager;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This job fires up the content management system - this can take a while 
 * and we don't want to block the app server deployment process.
 * 
 * @author mike
 */
public class ContentStartupJob implements Job {

    private static final Logger LOGGER = LoggerFactory.getLogger(ContentStartupJob.class);

	@Override
	public void execute(JobExecutionContext context)
	throws JobExecutionException {
		try (ContentManager cm = EXT.newContentManager()) {
			cm.startup();
		}
		catch (Exception e) {
			LOGGER.info("Could not startup the content manager - this is non-fatal but requires investigation");
			e.printStackTrace();
		}
	}
}
