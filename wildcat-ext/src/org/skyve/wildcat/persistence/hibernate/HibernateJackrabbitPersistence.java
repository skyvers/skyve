package org.skyve.wildcat.persistence.hibernate;

import javax.jcr.RepositoryException;

import org.skyve.domain.PersistentBean;
import org.skyve.wildcat.content.BeanContent;
import org.skyve.wildcat.content.ContentUtil;

public class HibernateJackrabbitPersistence extends AbstractHibernatePersistence {
	private static final long serialVersionUID = 1433618526097088364L;

	private transient javax.jcr.Session jcrSession = null;

	@Override
	protected void commitContent() {
		if (jcrSession != null) {
			try {
				jcrSession.save();
			}
			catch (RepositoryException e) {
				System.err.println("Cannot save content repository changes.");
			}
			finally {
				jcrSession.logout();
				jcrSession = null;
			}
		}
	}

	@Override
	protected void removeBeanContent(PersistentBean bean) throws Exception {
		if (jcrSession == null) {
			jcrSession = ContentUtil.getFullSession(getUser().getCustomerName());
		}
		ContentUtil.remove(jcrSession, bean);
	}

	@Override
	protected void putContent(BeanContent content) throws Exception {
		if (jcrSession == null) {
			jcrSession = ContentUtil.getFullSession(getUser().getCustomerName());
		}
		ContentUtil.put(jcrSession, content);
	}

	@Override
	protected void moveContent(BeanContent content,
								String oldBizDataGroupId,
								String oldBizUserId)
	throws Exception {
		if (jcrSession == null) {
			jcrSession = ContentUtil.getFullSession(getUser().getCustomerName());
		}
		ContentUtil.move(jcrSession, content, oldBizDataGroupId, oldBizUserId);
	}

	@Override
	protected void removeStreamContent(PersistentBean bean, String fieldName)
	throws Exception {
		if (jcrSession == null) {
			jcrSession = ContentUtil.getFullSession(getUser().getCustomerName());
		}
		ContentUtil.remove(jcrSession, bean, fieldName);
	}
}
