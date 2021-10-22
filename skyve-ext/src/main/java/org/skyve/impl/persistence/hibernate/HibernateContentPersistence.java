package org.skyve.impl.persistence.hibernate;

import org.skyve.EXT;
import org.skyve.content.BeanContent;
import org.skyve.content.ContentManager;
import org.skyve.domain.PersistentBean;

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
		cm.removeBean(bean.getBizId());
	}

	@Override
	protected void putBeanContent(BeanContent content) throws Exception {
		if (cm == null) {
			cm = EXT.newContentManager();
		}
		cm.put(content);
	}

	@Override
	protected void removeAttachmentContent(String contentId)
	throws Exception {
		if (cm == null) {
			cm = EXT.newContentManager();
		}
		cm.removeAttachment(contentId);
	}
}
