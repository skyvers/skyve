package modules.admin.ControlPanel.actions;

import java.io.Serializable;

import org.skyve.cache.CacheConfig;
import org.skyve.cache.CacheUtil;
import org.skyve.cache.EHCacheConfig;
import org.skyve.cache.JCacheConfig;
import org.skyve.domain.messages.MessageSeverity;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;

import modules.admin.ControlPanel.ControlPanelExtension;

public class StopOrStartSelectedCache implements ServerSideAction<ControlPanelExtension> {
	private static final long serialVersionUID = -8001307700407362818L;

	@Override
	public ServerSideActionResult<ControlPanelExtension> execute(ControlPanelExtension bean, WebContext webContext) throws Exception {
		bean.setTabIndex(null);
		
		String cacheName = bean.getSelectedCache();
		boolean found = false;
		if (cacheName != null) {
			for (CacheConfig<? extends Serializable, ? extends Serializable> c : UtilImpl.APP_CACHES) {
				String appCacheName = c.getName();
				if (cacheName.equals(appCacheName)) {
					found = true;
					if (c instanceof EHCacheConfig<?, ?>) {
						if (CacheUtil.getEHCache(cacheName, c.getKeyClass(), c.getValueClass()) == null) {
							CacheUtil.createEHCache((EHCacheConfig<? extends Serializable, ? extends Serializable>) c);
							
							webContext.growl(MessageSeverity.info, "Cache " + cacheName + " has been started");
						}
						else {
							CacheUtil.removeEHCache(cacheName);
							webContext.growl(MessageSeverity.info, "Cache " + cacheName + " has been stopped");
						}
					}
					else if (c instanceof JCacheConfig<?, ?>) {
						if (CacheUtil.getJCache(cacheName, c.getKeyClass(), c.getValueClass()) == null) {
							CacheUtil.createJCache((JCacheConfig<? extends Serializable, ? extends Serializable>) c);
							webContext.growl(MessageSeverity.info, "Cache " + cacheName + " has been started");
						}
						else {
							CacheUtil.destroyJCache(cacheName);
							webContext.growl(MessageSeverity.info, "Cache " + cacheName + " has been stopped");
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
