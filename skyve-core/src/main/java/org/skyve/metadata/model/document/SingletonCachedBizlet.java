package org.skyve.metadata.model.document;

import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

import org.skyve.CORE;
import org.skyve.domain.PersistentBean;
import org.skyve.metadata.user.DocumentPermissionScope;
import org.skyve.metadata.user.User;

/**
 * A Thread-safe cached singleton Bizlet implementation that memoises the bizId from the super.newInstance() call
 * if the permission is global or customer scoped.
 *
 * @param <T>
 */
public abstract class SingletonCachedBizlet<T extends PersistentBean> extends SingletonBizlet<T> {
	/**
	 * Thread-safe map of module/document keys to singleton instance bizId.
	 */
	private static final ConcurrentMap<String, String> INSTANCES = new ConcurrentHashMap<>();
	
	/**
	 * Gets a memoised version of the singleton instance (if it is customer or global scoped).
	 */
	@Override
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

		String bizId = INSTANCES.get(key);
		T result = null;
		if (bizId == null) {
			result = super.newInstance(bean);
			if (result.isPersisted()) {
				INSTANCES.putIfAbsent(key, result.getBizId());
			}
		}
		else {
			// This is most probably cached
			result = CORE.getPersistence().retrieve(bizModule, bizDocument, bizId);
		}
		return result;
	}
	
	/**
	 * Clear the cached details of the singleton document instance. 
	 */
	public static void clear(String moduleName, String documentName) {
		String key = new StringBuilder(128).append(moduleName).append('.').append(documentName).append(CORE.getCustomer().getName()).append('.').toString();
		INSTANCES.remove(key);
		key = new StringBuilder(128).append(moduleName).append('.').append(documentName).toString();
		INSTANCES.remove(key);
	}
	
	/**
	 * Clear all cached details.
	 */
	public static void dispose() {
		INSTANCES.clear();
	}
}
