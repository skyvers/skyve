package org.skyve.metadata.model.document;

import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;
import org.skyve.metadata.user.DocumentPermissionScope;
import org.skyve.metadata.user.User;
import org.skyve.util.Util;

/**
 * A Thread-safe cached singleton Bizlet implementation that memoises the super.newInstance() call
 * if the permission is global or customer scoped.
 * evict() evicts the cache instance and postSave() calls evict().
 *
 * @param <T>
 */
public abstract class SingletonCachedBizlet<T extends PersistentBean> extends SingletonBizlet<T> {
	private static final long serialVersionUID = -4069583265754912169L;

	/**
	 * Thread-safe map of module/document keys to singleton instances.
	 */
	private static final ConcurrentMap<String, Bean> INSTANCES = new ConcurrentHashMap<>();
	
	/**
	 * Gets a memoised version of the singleton instance (if it is customer or global scoped).
	 */
	@Override
	@SuppressWarnings("unchecked")
	public T newInstance(T bean) throws Exception {
		String bizModule = bean.getBizModule();
		String bizDocument = bean.getBizDocument();
		User u = CORE.getUser();
		DocumentPermissionScope scope = u.getScope(bizModule, bizDocument);
		String key = null;
		if (DocumentPermissionScope.customer.equals(scope)) {
			key = new StringBuilder(128).append(bizModule).append('.').append(bizDocument).append(u.getCustomerName()).append('.').toString();
		}
		else if (DocumentPermissionScope.global.equals(scope)) {
			key = new StringBuilder(128).append(bizModule).append('.').append(bizDocument).toString();
		}
		
		// not customer or global scoped, so no caching
		if (key == null) {
			return super.newInstance(bean);
		}

		Bean result = INSTANCES.get(key);
		if (result == null) {
			result = super.newInstance(bean);
			Util.populateFully(result);
			INSTANCES.putIfAbsent(key, result);
		}
		return (T) result;
	}
	
	/**
	 * Evicts the instance after it has been saved.
	 */
	@Override
	public void postSave(T bean) throws Exception {
		evict(bean);
	}

	/**
	 * Evicts the instance from the cache.
	 * @param bean
	 */
	public synchronized void evict(T bean) {
		INSTANCES.entrySet().removeIf(e -> e.getValue().getBizId().equals(bean.getBizId())); 
	}
}
