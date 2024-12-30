package org.skyve.impl.job;

import org.quartz.Job;
import org.quartz.JobExecutionContext;
import org.quartz.JobExecutionException;
import org.skyve.impl.cache.StateUtil;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This job evicts expired conversations from the conversations cache.
 * 
 * @author sandsm01
 */
public class EvictStateJob implements Job {

    private static final Logger LOGGER = LoggerFactory.getLogger(EvictStateJob.class);

	@Override
	public void execute(JobExecutionContext context)
	throws JobExecutionException {
		try {
			LOGGER.info("Evict expired conversations");
			StateUtil.evictExpiredConversations();
			LOGGER.info("Successfully evicted expired conversations");
			LOGGER.info("Evict expired session CSRF tokens");
			StateUtil.evictExpiredSessionTokens();
			LOGGER.info("Successfully evicted expired session CSRF tokens");
		}
		catch (Exception e) {
			throw new JobExecutionException("Error encountered whilst evicting expired web state", e);
		}
	}
}