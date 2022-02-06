package org.skyve.metadata.model.document;

import org.skyve.CORE;
import org.skyve.domain.PersistentBean;
import org.skyve.persistence.Persistence;

/**
 * A Bizlet extension that will return a persisted instance for the current user if one exists.
 *
 * @param <T>
 */
public abstract class SingletonBizlet<T extends PersistentBean> extends Bizlet<T> {
	/**
	 * If there is no persisted instance (or the current user cannot see the persisted instance), bean is returned.
	 * @param bean
	 */
	@Override
	public T newInstance(T bean) throws Exception {
		Persistence p = CORE.getPersistence();
		T result = p.newDocumentQuery(bean.getBizModule(), bean.getBizDocument()).beanResult();
		if (result == null) {
			result = bean;
		}
		return result;
	}
}
