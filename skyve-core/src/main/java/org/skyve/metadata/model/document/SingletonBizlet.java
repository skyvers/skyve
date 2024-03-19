package org.skyve.metadata.model.document;

import org.skyve.CORE;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.messages.DomainException;
import org.skyve.metadata.user.DocumentPermissionScope;
import org.skyve.persistence.Persistence;

import jakarta.annotation.Nonnull;

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
		return monomorphicNewInstance(bean);
	}
	
	private @Nonnull T monomorphicNewInstance(@Nonnull T bean) throws Exception {
		Persistence p = CORE.getPersistence();
		T result = p.newDocumentQuery(bean.getBizModule(), bean.getBizDocument()).beanResult();
		if (result == null) {
			// Run the meta-data bizlet if it exists
			result = super.newInstance(bean);
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
	public @Nonnull T newInstance(@Nonnull T bean, @Nonnull DocumentPermissionScope scope) throws Exception {
		return CORE.getPersistence().withDocumentPermissionScopes(scope, p -> {
			try {
				return monomorphicNewInstance(bean);
			}
			catch (Exception e) {
				throw new DomainException(e);
			}
		});
	}
}
