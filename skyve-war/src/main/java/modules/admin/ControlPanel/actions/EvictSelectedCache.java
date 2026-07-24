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

import jakarta.annotation.Nonnull;
import modules.admin.ControlPanel.ControlPanelExtension;

/**
 * Evicts selected cache regions from the configured cache provider.
 */
public class EvictSelectedCache implements ServerSideAction<ControlPanelExtension> {
	private static final String CACHE_CLEARED_SUFFIX = " has been cleared";

	/**
	 * Performs the execute operation.
	 *
	 * @param bean the bean value
	 * @param webContext the webContext value
	 * @return the operation result
	 * @throws Exception if the operation fails
	 */
	@Override
	@SuppressWarnings("java:S3776") // Complexity OK
	public ServerSideActionResult<ControlPanelExtension> execute(ControlPanelExtension bean, WebContext webContext)
	throws Exception {
		bean.setTabIndex(null);

		String cacheName = bean.getSelectedCache();
		if (cacheName != null) {
			Caching caching = EXT.getCaching();

			if (UtilImpl.CONVERSATION_CACHE.getName().equals(cacheName)) {
				Cache<? extends Serializable, ? extends Serializable> cache = caching.getEHCache(cacheName, UtilImpl.CONVERSATION_CACHE.getKeyClass(), UtilImpl.CONVERSATION_CACHE.getValueClass());
				clearEHCache(cache, webContext, cacheName);
			}
			else if (UtilImpl.CSRF_TOKEN_CACHE.getName().equals(cacheName)) {
				Cache<? extends Serializable, ? extends Serializable> cache = caching.getEHCache(cacheName, UtilImpl.CSRF_TOKEN_CACHE.getKeyClass(), UtilImpl.CSRF_TOKEN_CACHE.getValueClass());
				clearEHCache(cache, webContext, cacheName);
			}
			else if (UtilImpl.GEO_IP_CACHE.getName().equals(cacheName)) {
				Cache<? extends Serializable, ? extends Serializable> cache = caching.getEHCache(cacheName, UtilImpl.GEO_IP_CACHE.getKeyClass(), UtilImpl.GEO_IP_CACHE.getValueClass());
				clearEHCache(cache, webContext, cacheName);
			}
			else if (UtilImpl.SESSION_CACHE.getName().equals(cacheName)) {
				Cache<? extends Serializable, ? extends Serializable> cache = caching.getEHCache(cacheName, UtilImpl.SESSION_CACHE.getKeyClass(), UtilImpl.SESSION_CACHE.getValueClass());
				clearEHCache(cache, webContext, cacheName);
			}
			else {
				boolean found = false;
				for (HibernateCacheConfig c : UtilImpl.HIBERNATE_CACHES) {
					String hibernateCacheName = c.getName();
					if (cacheName.equals(hibernateCacheName)) {
						found = true;
						javax.cache.Cache<? extends Serializable, ? extends Serializable> cache = caching.getJCache(cacheName, c.getKeyClass(), c.getValueClass());
						clearJCache(cache, webContext, cacheName);
						break;
					}
				}
				if (! found) {
					for (CacheConfig<? extends Serializable, ? extends Serializable> c : UtilImpl.APP_CACHES) {
						String appCacheName = c.getName();
						if (cacheName.equals(appCacheName)) {
							if (c instanceof EHCacheConfig<?, ?>) {
								Cache<? extends Serializable, ? extends Serializable> cache = caching.getEHCache(cacheName, c.getKeyClass(), c.getValueClass());
								clearEHCache(cache, webContext, cacheName);
							}
							else if (c instanceof JCacheConfig<?, ?>) {
								@SuppressWarnings("resource")
								javax.cache.Cache<? extends Serializable, ? extends Serializable> cache = caching.getJCache(cacheName, c.getKeyClass(), c.getValueClass());
								clearJCache(cache, webContext, cacheName);
							}
							break;
						}
					}
				}
			}
		}
		return new ServerSideActionResult<>(bean);
	}

	/**
	 * Clears an Ehcache cache and reports the successful eviction to the user.
	 *
	 * <p>Side effects: removes every entry from {@code cache} and adds an informational growl
	 * message to {@code webContext}.
	 *
	 * @param cache the cache to clear; must not be {@code null}
	 * @param webContext the current web context that receives the growl; must not be {@code null}
	 * @param cacheName the cache name displayed in the growl; must not be {@code null}
	 */
	private static void clearEHCache(@Nonnull Cache<?, ?> cache,
										@Nonnull WebContext webContext,
										@Nonnull String cacheName) {
		cache.clear();
		webContext.growl(MessageSeverity.info, "Cache " + cacheName + CACHE_CLEARED_SUFFIX);
	}

	/**
	 * Clears a JCache cache and reports the successful eviction to the user.
	 *
	 * <p>Side effects: removes every entry from {@code cache} and adds an informational growl
	 * message to {@code webContext}.
	 *
	 * @param cache the cache to clear; must not be {@code null}
	 * @param webContext the current web context that receives the growl; must not be {@code null}
	 * @param cacheName the cache name displayed in the growl; must not be {@code null}
	 */
	private static void clearJCache(@Nonnull javax.cache.Cache<?, ?> cache,
										@Nonnull WebContext webContext,
										@Nonnull String cacheName) {
		cache.clear();
		webContext.growl(MessageSeverity.info, "Cache " + cacheName + CACHE_CLEARED_SUFFIX);
	}
}
