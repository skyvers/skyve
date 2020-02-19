package org.skyve.impl.web;

import java.util.concurrent.atomic.AtomicInteger;

import javax.servlet.http.HttpSessionEvent;
import javax.servlet.http.HttpSessionListener;

import org.ehcache.core.statistics.CacheStatistics;
import org.ehcache.core.statistics.TierStatistics;
import org.skyve.cache.CacheTier;
import org.skyve.cache.CacheUtil;
import org.skyve.impl.util.UtilImpl;

/**
 * Used to count the active sessions on a server.
 * 
 * @author mike
 */
public class SkyveSessionListener implements HttpSessionListener {
	private static final AtomicInteger COUNT = new AtomicInteger(0);

	@Override
	public void sessionCreated(HttpSessionEvent se) {
		COUNT.incrementAndGet();
	}

	@Override
	public void sessionDestroyed(HttpSessionEvent se) {
		int value = COUNT.decrementAndGet();
		if (value < 0) {
			COUNT.set(0);
		}
	}
	
	public static int getSessionCount() {
		return COUNT.get();
	}
	
	public static void logSessionAndConversationsStats() {
		CacheStatistics statistics = CacheUtil.getEHCacheStatistics(UtilImpl.CONVERSATION_CACHE.getName());
		if (statistics != null) {
			TierStatistics tier = CacheUtil.getEHTierStatistics(statistics, CacheTier.OnHeap);
			if (tier != null) {
				UtilImpl.LOGGER.info("Count in heap memory = " + tier.getMappings());
			}
			tier = CacheUtil.getEHTierStatistics(statistics, CacheTier.OffHeap);
			if (tier != null) {
				UtilImpl.LOGGER.info("Count in off-heap memory = " + tier.getMappings());
				UtilImpl.LOGGER.info("MB in off-heap memory = " + ((int) (tier.getOccupiedByteSize() / 1024F / 1024F * 10F) / 10F));
			}
			tier = CacheUtil.getEHTierStatistics(statistics, CacheTier.Disk);
			if (tier != null) {
				UtilImpl.LOGGER.info("Count on disk = " + tier.getMappings());
				UtilImpl.LOGGER.info("MB on disk = " + ((int) (tier.getOccupiedByteSize() / 1024F / 1024F * 10F) / 10F));
			}
		}
		UtilImpl.LOGGER.info("Session count = " + SkyveSessionListener.getSessionCount());
		UtilImpl.LOGGER.info("**************************************************************");
	}
}
