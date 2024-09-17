package modules.admin.ControlPanel.actions;

import java.io.Serializable;

import org.ehcache.Cache;
import org.skyve.EXT;
import org.skyve.cache.CacheConfig;
import org.skyve.cache.Caching;
import org.skyve.cache.EHCacheConfig;
import org.skyve.cache.HibernateCacheConfig;
import org.skyve.cache.JCacheConfig;
import org.skyve.domain.messages.MessageSeverity;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;

import modules.admin.ControlPanel.ControlPanelExtension;

public class EvictSelectedCache implements ServerSideAction<ControlPanelExtension> {
	@Override
	public ServerSideActionResult<ControlPanelExtension> execute(ControlPanelExtension bean, WebContext webContext) throws Exception {
		bean.setTabIndex(null);

		String cacheName = bean.getSelectedCache();
		if (cacheName != null) {
			Caching caching = EXT.getCaching();
			
			if (UtilImpl.CONVERSATION_CACHE.getName().equals(cacheName)) {
				Cache<? extends Serializable, ? extends Serializable> cache = caching.getEHCache(cacheName, UtilImpl.CONVERSATION_CACHE.getKeyClass(), UtilImpl.CONVERSATION_CACHE.getValueClass());
				cache.clear();
				webContext.growl(MessageSeverity.info, "Cache " + cacheName + " has been cleared");
			}
			else if (UtilImpl.CSRF_TOKEN_CACHE.getName().equals(cacheName)) {
				Cache<? extends Serializable, ? extends Serializable> cache = caching.getEHCache(cacheName, UtilImpl.CSRF_TOKEN_CACHE.getKeyClass(), UtilImpl.CSRF_TOKEN_CACHE.getValueClass());
				cache.clear();
				webContext.growl(MessageSeverity.info, "Cache " + cacheName + " has been cleared");
			}
			else if (UtilImpl.GEO_IP_CACHE.getName().equals(cacheName)) {
				Cache<? extends Serializable, ? extends Serializable> cache = caching.getEHCache(cacheName, UtilImpl.GEO_IP_CACHE.getKeyClass(), UtilImpl.GEO_IP_CACHE.getValueClass());
				cache.clear();
				webContext.growl(MessageSeverity.info, "Cache " + cacheName + " has been cleared");
			}
			else if (UtilImpl.SESSION_CACHE.getName().equals(cacheName)) {
				Cache<? extends Serializable, ? extends Serializable> cache = caching.getEHCache(cacheName, UtilImpl.SESSION_CACHE.getKeyClass(), UtilImpl.SESSION_CACHE.getValueClass());
				cache.clear();
				webContext.growl(MessageSeverity.info, "Cache " + cacheName + " has been cleared");
			}
			else {
				boolean found = false;
				for (HibernateCacheConfig c : UtilImpl.HIBERNATE_CACHES) {
					String hibernateCacheName = c.getName();
					if (cacheName.equals(hibernateCacheName)) {
						found = true;
						javax.cache.Cache<? extends Serializable, ? extends Serializable> cache = caching.getJCache(cacheName, c.getKeyClass(), c.getValueClass());
						cache.clear();
						webContext.growl(MessageSeverity.info, "Cache " + cacheName + " has been cleared");
						break;
					}
				}
				if (! found) {
					for (CacheConfig<? extends Serializable, ? extends Serializable> c : UtilImpl.APP_CACHES) {
						String appCacheName = c.getName();
						if (cacheName.equals(appCacheName)) {
							if (c instanceof EHCacheConfig<?, ?>) {
								Cache<? extends Serializable, ? extends Serializable> cache = caching.getEHCache(cacheName, c.getKeyClass(), c.getValueClass());
								cache.clear();
							}
							else if (c instanceof JCacheConfig<?, ?>) {
								@SuppressWarnings("resource")
								javax.cache.Cache<? extends Serializable, ? extends Serializable> cache = caching.getJCache(cacheName, c.getKeyClass(), c.getValueClass());
								cache.clear();
							}
							webContext.growl(MessageSeverity.info, "Cache " + cacheName + " has been cleared");
							break;
						}
					}
				}
			}
		}
		return new ServerSideActionResult<>(bean);
	}
}
