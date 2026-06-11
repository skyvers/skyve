package modules.admin.ControlPanel.actions;

import java.io.Serializable;

import javax.cache.Cache;

import org.skyve.EXT;
import org.skyve.cache.CacheConfig;
import org.skyve.cache.Caching;
import org.skyve.cache.EHCacheConfig;
import org.skyve.cache.JCacheConfig;
import org.skyve.domain.messages.MessageSeverity;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;

import modules.admin.ControlPanel.ControlPanelExtension;

/**
 * Starts or stops selected caches in the cache provider.
 */
public class StopOrStartSelectedCache implements ServerSideAction<ControlPanelExtension> {
	private static final String CACHE_PREFIX = "Cache ";
	private static final String HAS_BEEN_STARTED = " has been started";
	private static final String HAS_BEEN_STOPPED = " has been stopped";

	/**
	 * Performs the execute operation.
	 * @param bean the bean value
	 * @param webContext the webContext value
	 * @return the operation result
	 * @throws Exception if the operation fails
	 */
	@Override
	public ServerSideActionResult<ControlPanelExtension> execute(ControlPanelExtension bean, WebContext webContext) throws Exception {
		bean.setTabIndex(null);
		
		String cacheName = bean.getSelectedCache();
		boolean found = false;
		if (cacheName != null) {
			final Caching caching = EXT.getCaching();

			for (CacheConfig<? extends Serializable, ? extends Serializable> c : UtilImpl.APP_CACHES) {
				String appCacheName = c.getName();
				if (cacheName.equals(appCacheName)) {
					found = true;
					if (c instanceof EHCacheConfig<?, ?>) {
						if (caching.getEHCache(cacheName, c.getKeyClass(), c.getValueClass()) == null) {
							caching.createEHCache((EHCacheConfig<? extends Serializable, ? extends Serializable>) c);
							
							webContext.growl(MessageSeverity.info, CACHE_PREFIX + cacheName + HAS_BEEN_STARTED);
						}
						else {
							caching.removeEHCache(cacheName);
							webContext.growl(MessageSeverity.info, CACHE_PREFIX + cacheName + HAS_BEEN_STOPPED);
						}
					}
					else if (c instanceof JCacheConfig<?, ?>) {
						@SuppressWarnings("resource")
						Cache<?, ?> cache = caching.getJCache(cacheName, c.getKeyClass(), c.getValueClass());
						if (cache == null) {
							@SuppressWarnings({ "resource", "unused" })
							Cache<?, ?> created = caching.createJCache((JCacheConfig<? extends Serializable, ? extends Serializable>) c);
							webContext.growl(MessageSeverity.info, CACHE_PREFIX + cacheName + HAS_BEEN_STARTED);
						}
						else {
							caching.destroyJCache(cacheName);
							webContext.growl(MessageSeverity.info, CACHE_PREFIX + cacheName + HAS_BEEN_STOPPED);
						}
					}
					break;
				}
			}
		}
		
		if (! found) {
			webContext.growl(MessageSeverity.error, "Cant stop/start cache " + cacheName);
		}
		
		return new ServerSideActionResult<>(bean);
	}
}
