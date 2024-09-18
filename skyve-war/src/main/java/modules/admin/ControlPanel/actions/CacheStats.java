package modules.admin.ControlPanel.actions;

import java.io.Serializable;

import javax.cache.management.CacheStatisticsMXBean;

import org.ehcache.core.statistics.CacheStatistics;
import org.ehcache.core.statistics.TierStatistics;
import org.skyve.EXT;
import org.skyve.cache.CacheConfig;
import org.skyve.cache.CacheTier;
import org.skyve.cache.Caching;
import org.skyve.cache.EHCacheConfig;
import org.skyve.cache.HibernateCacheConfig;
import org.skyve.cache.JCacheConfig;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;

import modules.admin.ControlPanel.ControlPanelExtension;

public class CacheStats implements ServerSideAction<ControlPanelExtension> {
	@Override
	public ServerSideActionResult<ControlPanelExtension> execute(ControlPanelExtension bean, WebContext webContext) throws Exception {
		final Caching caching = EXT.getCaching();
		
		StringBuilder result = new StringBuilder(512);
		result.append("<table>");

		String cacheName = UtilImpl.CONVERSATION_CACHE.getName();
		addEHCacheStats(cacheName, caching.getEHCacheStatistics(cacheName), result);
		cacheName = UtilImpl.CSRF_TOKEN_CACHE.getName();
		addEHCacheStats(cacheName, caching.getEHCacheStatistics(cacheName), result);
		cacheName = UtilImpl.SESSION_CACHE.getName();
		addEHCacheStats(cacheName, caching.getEHCacheStatistics(cacheName), result);
		cacheName = UtilImpl.GEO_IP_CACHE.getName();
		addEHCacheStats(cacheName, caching.getEHCacheStatistics(cacheName), result);
		for (HibernateCacheConfig c : UtilImpl.HIBERNATE_CACHES) {
			cacheName = c.getName();
			addJCacheStats(cacheName, caching.getJCacheStatisticsMXBean(cacheName), result);
		}
		for (CacheConfig<? extends Serializable, ? extends Serializable> c : UtilImpl.APP_CACHES) {
			cacheName = c.getName();
			if (c instanceof EHCacheConfig<?, ?>) {
				addEHCacheStats(cacheName, caching.getEHCacheStatistics(cacheName), result);
			}
			else if (c instanceof JCacheConfig<?, ?>) {
				addJCacheStats(cacheName, caching.getJCacheStatisticsMXBean(cacheName), result);
			}
		}
		result.append("</table>");
		
		bean.setResults(result.toString(), false);
		bean.setTabIndex(Integer.valueOf(2));
		return new ServerSideActionResult<>(bean);
	}
	
	public static void addEHCacheStats(String cacheName, CacheStatistics stats, StringBuilder sb) {
		final Caching caching = EXT.getCaching();

		sb.append("<tr><td style=\"padding:20px\">");
		sb.append("<h1>").append(cacheName).append("</h1>");
		addStats(stats, sb);
		sb.append("</td>");
		
		TierStatistics ts = caching.getEHTierStatistics(stats, CacheTier.OnHeap);
		if (ts != null) {
			sb.append("<td style=\"padding:20px\">");
			sb.append("<h2>").append("Heap").append("</h2>");
			addStats(ts, sb);
			sb.append("</td>");
		}
		
		ts = caching.getEHTierStatistics(stats, CacheTier.OffHeap);
		if (ts != null) {
			sb.append("<td style=\"padding:20px\">");
			sb.append("<h2>").append("Off-Heap").append("</h2>");
			addStats(ts, sb);
			sb.append("</td>");
		}

		ts = caching.getEHTierStatistics(stats, CacheTier.Disk);
		if (ts != null) {
			sb.append("<td style=\"padding:20px\">");
			sb.append("<h2>").append("Disk").append("</h2>");
			addStats(ts, sb);
			sb.append("</td>");
		}
		sb.append("</tr>");
	}

	public static void addJCacheStats(String cacheName, CacheStatisticsMXBean stats, StringBuilder sb) {
		sb.append("<tr><td style=\"padding:20px\">");
		sb.append("<h1>").append(cacheName).append("</h1>");
		addStats(stats, sb);
		sb.append("</td>");
		sb.append("</tr>");
	}

	private static void addStats(CacheStatistics stats, StringBuilder sb) {
		if (stats == null) {
			sb.append("No stats<br/>");
		}
		else {
			sb.append("Evictions: ").append(stats.getCacheEvictions()).append("<br/>");
			sb.append("Expirations: ").append(stats.getCacheExpirations()).append("<br/>");
			sb.append("Gets: ").append(stats.getCacheGets()).append("<br/>");
			sb.append("Hits: ").append(stats.getCacheHits()).append("<br/>");
			sb.append("Hit (%): ").append(stats.getCacheHitPercentage()).append("<br/>");
			sb.append("Misses: ").append(stats.getCacheMisses()).append("<br/>");
			sb.append("Miss (%): ").append(stats.getCacheMissPercentage()).append("<br/>");
			sb.append("Puts: ").append(stats.getCachePuts()).append("<br/>");
			sb.append("Removals: ").append(stats.getCacheRemovals()).append("<br/>");
		}
	}
	
	private static void addStats(TierStatistics stats, StringBuilder sb) {
		if (stats != null) {
			sb.append("Allocated Byte Size: ").append(stats.getAllocatedByteSize()).append("<br/>");
			sb.append("Occupied Byte Size: ").append(stats.getOccupiedByteSize()).append("<br/>");
			sb.append("Entries: ").append(stats.getMappings()).append("<br/>");
			sb.append("Evictions: ").append(stats.getEvictions()).append("<br/>");
			sb.append("Expirations: ").append(stats.getExpirations()).append("<br/>");
			sb.append("Hits: ").append(stats.getHits()).append("<br/>");
			sb.append("Misses: ").append(stats.getMisses()).append("<br/>");
			sb.append("Puts: ").append(stats.getPuts()).append("<br/>");
			sb.append("Removals: ").append(stats.getRemovals()).append("<br/>");
		}
	}

	private static void addStats(CacheStatisticsMXBean stats, StringBuilder sb) {
		if (stats == null) {
			sb.append("No Stats<br/>");
		}
		else {
			sb.append("Evictions: ").append(stats.getCacheEvictions()).append("<br/>");
			sb.append("Gets: ").append(stats.getCacheGets()).append("<br/>");
			sb.append("Hit (%): ").append(stats.getCacheHitPercentage()).append("<br/>");
			sb.append("Hits: ").append(stats.getCacheHits()).append("<br/>");
			sb.append("Misses: ").append(stats.getCacheMisses()).append("<br/>");
			sb.append("Miss (%): ").append(stats.getCacheMissPercentage()).append("<br/>");
			sb.append("Puts: ").append(stats.getCachePuts()).append("<br/>");
			sb.append("Removals: ").append(stats.getCacheRemovals()).append("<br/>");
			sb.append("Average Get Time: ").append(stats.getAverageGetTime()).append("<br/>");
			sb.append("Average Put Time: ").append(stats.getAveragePutTime()).append("<br/>");
			sb.append("Average Remove Time: ").append(stats.getAverageRemoveTime()).append("<br/>");
		}
	}
}
