package org.skyve.impl.persistence.hibernate;

import org.skyve.EXT;
import org.skyve.content.BeanContent;
import org.skyve.content.ContentManager;
import org.skyve.domain.PersistentBean;

/**
 * {@link AbstractHibernatePersistence} variant that also manages content-store
 * (attachment and bean content) lifecycle alongside the relational data.
 */
public class HibernateContentPersistence extends AbstractHibernatePersistence {
	private static final long serialVersionUID = 1433618526097088364L;

	@SuppressWarnings("resource") // This persistence instance owns the content manager and closes it in closeContent().
	private transient ContentManager cm;

	@SuppressWarnings("resource") // This persistence instance owns the content manager and closes it in closeContent().
	private ContentManager getContentManager() {
		if (cm == null) {
			cm = EXT.newContentManager();
		}
		return cm;
	}
	
	/**
	 * Closes the lazily created content manager for this persistence instance.
	 *
	 * <p>Side effects: releases any underlying content-store resources associated with
	 * the current transaction or request.
	 */
	@Override
	protected void closeContent() throws Exception {
		if (cm != null) {
			cm.close();
		}
	}

	/**
	 * Removes all content-store entries associated with the supplied bean.
	 *
	 * @param bean the bean whose content should be removed
	 */
	@Override
	@SuppressWarnings("resource") // The managed content manager is owned by this persistence instance and closed in closeContent().
	protected void removeBeanContent(PersistentBean bean) throws Exception {
		getContentManager().removeBean(bean.getBizId());
	}

	/**
	 * Stores the supplied bean content through the configured content manager.
	 *
	 * @param content the content payload to persist
	 */
	@Override
	@SuppressWarnings("resource") // The managed content manager is owned by this persistence instance and closed in closeContent().
	protected void putBeanContent(BeanContent content) throws Exception {
		getContentManager().put(content);
	}
}
