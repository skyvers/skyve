package org.skyve.impl.persistence.hibernate;

import org.skyve.content.BeanContent;
import org.skyve.domain.PersistentBean;

import jakarta.annotation.Nonnull;

/**
 * {@link AbstractHibernatePersistence} variant that does not manage a content store,
 * used in contexts where binary attachment storage is not required or is
 * handled externally.
 */
public class HibernateNoContentPersistence extends AbstractHibernatePersistence {
	private static final long serialVersionUID = 2357146148341285353L;

	@Override
	protected void closeContent() {
		// no-op
	}

	@Override
	protected void removeBeanContent(@Nonnull PersistentBean bean) throws Exception {
		// no-op
	}

	@Override
	protected void putBeanContent(@Nonnull BeanContent content) throws Exception {
		// no-op
	}
}
