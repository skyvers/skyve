package org.skyve.impl.job;

import org.quartz.Job;
import org.quartz.JobExecutionContext;
import org.quartz.JobExecutionException;
import org.skyve.cache.ConversationUtil;
import org.skyve.util.Util;

/**
 * This job evicts expired conversations from the conversations cache.
 * 
 * @author sandsm01
 */
public class EvictConversationsJob implements Job {
	
	@Override
	public void execute(JobExecutionContext context)
	throws JobExecutionException {
		try {
			Util.LOGGER.info("Evict expired conversations");
			ConversationUtil.evictExpiredConversations();
			Util.LOGGER.info("Successfully evicted expired conversations");
		}
		catch (Exception e) {
			throw new JobExecutionException("Error encountered whilst evicting expired conversations", e);
		}
	}
}