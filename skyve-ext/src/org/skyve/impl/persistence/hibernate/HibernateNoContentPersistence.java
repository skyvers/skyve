package org.skyve.impl.persistence.hibernate;

import org.skyve.content.BeanContent;
import org.skyve.domain.PersistentBean;
import org.skyve.impl.persistence.hibernate.AbstractHibernatePersistence;

public class HibernateNoContentPersistence extends AbstractHibernatePersistence {
	private static final long serialVersionUID = 2357146148341285353L;

	@Override
	protected void commitContent() {
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

	@Override
	protected void moveBeanContent(BeanContent content,
									String oldBizDataGroupId,
									String oldBizUserId)
	throws Exception {
		// no-op
	}

	@Override
	protected void removeAttachmentContent(String contentId)
	throws Exception {
		// no-op
	}
}
