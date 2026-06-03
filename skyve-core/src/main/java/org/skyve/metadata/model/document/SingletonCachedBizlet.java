package org.skyve.metadata.model.document;

import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

import org.skyve.CORE;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.messages.DomainException;
import org.skyve.metadata.user.DocumentPermissionScope;
import org.skyve.metadata.user.User;

import jakarta.annotation.Nonnull;

/**
 * Caches singleton document instance identifiers for global/customer-scoped
 * access and reuses them across {@link #newInstance(PersistentBean)} calls.
 *
 * <p>Cache entries are keyed by module/document and, for customer scope, by
 * customer name. If a cached identifier no longer resolves to a persisted
 * instance, the cache is refreshed from {@link SingletonBizlet} fallback
 * creation logic.
 *
 * <p>Threading: thread-safe for cache access via {@link ConcurrentMap};
 * returned document instances are not immutable and follow normal persistence
 * context/thread confinement rules.
 *
 * @param <T> the singleton document bean type
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
		return resolveCachedNewInstance(bean);
	}
	
	private @Nonnull T resolveCachedNewInstance(@Nonnull T bean) throws Exception {
		String bizModule = bean.getBizModule();
		String bizDocument = bean.getBizDocument();
		User u = CORE.getUser();
		DocumentPermissionScope scope = u.getScope(bizModule, bizDocument);
		String key = null;
		if (DocumentPermissionScope.customer.equals(scope)) {
			key = new StringBuilder(128).append(bizModule).append('.').append(bizDocument).append('.').append(u.getCustomerName()).toString();
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
			// Note this is most probably cached in hibernate second level cache
			result = CORE.getPersistence().retrieve(bizModule, bizDocument, bizId);
			if (result == null) { // probably deleted somehow
				result = super.newInstance(bean);
				String newBizId = result.getBizId();
				// replace the cached bizId if a new one exists in the data store
				if (result.isPersisted()) {
					INSTANCES.put(key, result.getBizId());
					LOGGER.warn("Cached instance {}#{} was replaced by {} in the data store.", key, bizId, newBizId);
				}
				// remove from the cache if there is none in the data store
				else {
					INSTANCES.remove(key);
					LOGGER.warn("Cached instance {}#{} was removed and a non-persistent instance {} was returned.", key, bizId, newBizId);
				}
			}
		}
		return result;
	}
	
	/**
	 * Call {@link #newInstance(PersistentBean)} with the given scope.
	 * @param bean	The bean
	 * @param scope	The scope
	 * @return	The singleton bean
	 * @throws Exception
	 */
	@Override
	public @Nonnull T newInstance(@Nonnull T bean, @Nonnull DocumentPermissionScope scope) throws Exception {
		T result = CORE.getPersistence().withDocumentPermissionScopes(scope, p -> {
			try {
				return resolveCachedNewInstance(bean);
			}
			catch (Exception e) {
				throw new DomainException(e);
			}
		});
		if (result == null) {
			throw new IllegalStateException("Singleton cached newInstance returned null for " + bean.getBizModule() + '.' + bean.getBizDocument());
		}
		return result;
	}

	/**
	 * Clear the cached details of the singleton document instance. 
	 */
	public static void clear(@Nonnull String moduleName, @Nonnull String documentName) {
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
