package org.skyve.impl.persistence.hibernate;

import org.skyve.content.BeanContent;
import org.skyve.domain.PersistentBean;

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
	protected void removeBeanContent(PersistentBean bean) throws Exception {
		// no-op
	}

	@Override
	protected void putBeanContent(BeanContent content) throws Exception {
		// no-op
	}
}
