package org.skyve.metadata.model.document;

import org.skyve.CORE;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.messages.DomainException;
import org.skyve.metadata.user.DocumentPermissionScope;
import org.skyve.persistence.Persistence;

import jakarta.annotation.Nonnull;

/**
 * Resolves a singleton document instance by returning the first persisted
 * record visible in the current permission scope.
 *
 * <p>If no persisted instance is visible, resolution falls back to
 * {@link Bizlet#newInstance(PersistentBean)}.
 *
 * <p>Threading: not thread-safe; instances rely on thread-bound
 * {@link CORE}/{@link Persistence} context.
 *
 * @param <T> the singleton document bean type
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
	@SuppressWarnings({"java:S112", "java:S1130"}) // Intentionally permitted exceptions
	public @Nonnull T newInstance(@Nonnull T bean, @Nonnull DocumentPermissionScope scope) throws Exception {
		@SuppressWarnings("null")
		@Nonnull T result = CORE.getPersistence().withDocumentPermissionScopes(scope, p -> {
			try {
				return monomorphicNewInstance(bean);
			}
			catch (Exception e) {
				throw new DomainException(e);
			}
		});
		return result;
	}
}
