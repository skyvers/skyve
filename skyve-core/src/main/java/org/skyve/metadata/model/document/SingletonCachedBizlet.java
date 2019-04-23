package org.skyve.metadata.model.document;

import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;
import org.skyve.util.Util;

/**
 * A Thread-safe cached singleton Bizlet implementation that memoises the super.newInstance() call.
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
	 * Gets a memoised version of the singleton instance.
	 */
	@Override
	@SuppressWarnings("unchecked")
	public T newInstance(T bean) throws Exception {
		String key = bean.getBizModule() + bean.getBizDocument();
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
		evict(bean.getBizModule(), bean.getBizDocument());
	}

	/**
	 * Evicts the instance from the cache.
	 * @param bizModule
	 * @param bizDocument
	 */
	public static void evict(String bizModule, String bizDocument) {
		INSTANCES.remove(bizModule + bizDocument);
	}
}
