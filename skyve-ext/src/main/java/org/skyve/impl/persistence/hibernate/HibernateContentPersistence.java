package org.skyve.impl.persistence.hibernate;

import org.skyve.EXT;
import org.skyve.content.BeanContent;
import org.skyve.content.ContentManager;
import org.skyve.domain.PersistentBean;
import org.skyve.impl.persistence.hibernate.AbstractHibernatePersistence;

public class HibernateContentPersistence extends AbstractHibernatePersistence {
	private static final long serialVersionUID = 1433618526097088364L;

	private transient ContentManager cm;
	
	@Override
	protected void closeContent() throws Exception {
		if (cm != null) {
			cm.close();
		}
	}

	@Override
	protected void removeBeanContent(PersistentBean bean) throws Exception {
		if (cm == null) {
			cm = EXT.newContentManager();
		}
		cm.remove(new BeanContent(bean));
	}

	@Override
	protected void putBeanContent(BeanContent content) throws Exception {
		if (cm == null) {
			cm = EXT.newContentManager();
		}
		cm.put(content);
	}

	@Override
	protected void moveBeanContent(BeanContent content,
									String oldBizDataGroupId,
									String oldBizUserId)
	throws Exception {
		// no-op
		// We don't need to move nodes in ES as its not a file system hierarchy 
	}

	@Override
	protected void removeAttachmentContent(String contentId)
	throws Exception {
		if (cm == null) {
			cm = EXT.newContentManager();
		}
		cm.remove(contentId);
	}
}
