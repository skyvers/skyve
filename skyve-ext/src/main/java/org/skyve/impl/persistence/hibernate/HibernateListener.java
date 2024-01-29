package org.skyve.impl.persistence.hibernate;

import java.util.Date;

import org.hibernate.HibernateException;
import org.hibernate.collection.spi.PersistentCollection;
import org.hibernate.event.spi.InitializeCollectionEvent;
import org.hibernate.event.spi.InitializeCollectionEventListener;
import org.hibernate.event.spi.PostDeleteEvent;
import org.hibernate.event.spi.PostDeleteEventListener;
import org.hibernate.event.spi.PostInsertEvent;
import org.hibernate.event.spi.PostInsertEventListener;
import org.hibernate.event.spi.PostLoadEvent;
import org.hibernate.event.spi.PostLoadEventListener;
import org.hibernate.event.spi.PostUpdateEvent;
import org.hibernate.event.spi.PostUpdateEventListener;
import org.hibernate.event.spi.PreDeleteEvent;
import org.hibernate.event.spi.PreDeleteEventListener;
import org.hibernate.event.spi.PreUpdateEvent;
import org.hibernate.event.spi.PreUpdateEventListener;
import org.hibernate.metadata.ClassMetadata;
import org.hibernate.persister.entity.EntityPersister;
import org.hibernate.type.Type;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.messages.SkyveException;
import org.skyve.domain.types.OptimisticLock;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.model.document.CollectionImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.document.Collection;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;

public class HibernateListener implements PostLoadEventListener,
											PreDeleteEventListener,
											PostDeleteEventListener,
											PostInsertEventListener,
											PreUpdateEventListener,
											PostUpdateEventListener,
											InitializeCollectionEventListener {
	private static final long serialVersionUID = -2075261951031625148L;
// TODO Need to replicate these functions for dynamic beans
	
	/**
	 * Call Bizlet.postLoad()
	 */
	@Override
	public void onPostLoad(PostLoadEvent event) {
		PersistentBean eventBean = (PersistentBean) event.getEntity();
		try {
			AbstractPersistence.get().postLoad(eventBean);
		}
		catch (SkyveException e) {
			throw e;
		}
		catch (Exception e) {
			throw new DomainException(e);
		}
	}

	/**
	 * Call Bizlet.preRemove()
	 * Note this is also called when collection elements are removed.
	 */
	@Override
	public boolean onPreDelete(PreDeleteEvent event) {
		PersistentBean eventBean = (PersistentBean) event.getEntity();
		try {
			AbstractPersistence.get().preRemove(eventBean);
		}
		catch (SkyveException e) {
			throw e;
		}
		catch (Exception e) {
			throw new DomainException(e);
		}
		return false;
	}

	/**
	 * Call Bizlet.postRemove()
	 * Note this is called when collection elements are removed.
	 */
	@Override
	public void onPostDelete(PostDeleteEvent event) {
		PersistentBean eventBean = (PersistentBean) event.getEntity();
		try {
			AbstractPersistence.get().postRemove(eventBean);
		}
		catch (SkyveException e) {
			throw e;
		}
		catch (Exception e) {
			throw new DomainException(e);
		}
	}
	
	/**
	 * Inject new bizLock
	 */
	@Override
	public boolean onPreUpdate(PreUpdateEvent event) {
		PersistentBean eventBean = (PersistentBean) event.getEntity();
		EntityPersister ep = event.getPersister();
		Object[] state = event.getState();
		
		AbstractPersistence persistence = AbstractPersistence.get();
		ClassMetadata cmd = ep.getClassMetadata();
		String[] propertyNames = cmd.getPropertyNames();

		for (int i = 0, l = propertyNames.length; i < l; i++) {
			String propertyName = propertyNames[i];
			if (PersistentBean.LOCK_NAME.equals(propertyName)) {
				OptimisticLock bizLock = new OptimisticLock(persistence.getUser().getName(), new Date());
				state[i] = bizLock;
				eventBean.setBizLock(bizLock);
			}
		}

		return false; // do not veto
	}

	/**
	 * Reindex any properties that have changed in the CMS.
	 */
	@Override
	public void onPostInsert(PostInsertEvent event) {
		AbstractHibernatePersistence persistence = (AbstractHibernatePersistence) AbstractPersistence.get();
		PersistentBean eventBean = (PersistentBean) event.getEntity();
		EntityPersister ep = event.getPersister();
		ClassMetadata cmd = ep.getClassMetadata();
		String[] propertyNames = cmd.getPropertyNames();
		Type[] propertyTypes = cmd.getPropertyTypes();
		Object[] state = event.getState();

		// Setup the new hibernate persisted instance returned
		
		// Re-inject
		UtilImpl.inject(eventBean);
		
		// Add back all the dynamic attributes required
		// NB These are set in AbstractHibernatePersistence.replaceTransientProperties() and TODO where are persistent ones added?
		Customer c = persistence.getUser().getCustomer();
		Module m = c.getModule(eventBean.getBizModule());
		DocumentImpl d = (DocumentImpl) m.getDocument(c, eventBean.getBizDocument());
		d.populateDynamicAttributeDefaults(c, eventBean);
		
		try {
			persistence.index(eventBean, propertyNames, propertyTypes, null, state);
		}
		catch (Exception e) {
			// Can't stop now, after all its only the indexing that is screwed
			e.printStackTrace();
		}
	}

	/**
	 * Reindex any properties that have changed in the CMS.
	 */
	@Override
	public void onPostUpdate(PostUpdateEvent event) {
		AbstractHibernatePersistence persistence = (AbstractHibernatePersistence) AbstractPersistence.get();
		PersistentBean eventBean = (PersistentBean) event.getEntity();
		EntityPersister ep = event.getPersister();
		ClassMetadata cmd = ep.getClassMetadata();
		String[] propertyNames = cmd.getPropertyNames();
		Type[] propertyTypes = cmd.getPropertyTypes();
		Object[] oldState = event.getOldState();
		Object[] state = event.getState();

		try {
			persistence.index(eventBean, propertyNames, propertyTypes, oldState, state);
		}
		catch (Exception e) {
			// Can't stop now, after all its only the indexing that is screwed
			e.printStackTrace();
		}
	}

	/**
	 * Sort any collections which have compound bindings in the ordering element.
	 */
	@Override
	public void onInitializeCollection(InitializeCollectionEvent event)
	throws HibernateException {
		try {
			AbstractPersistence persistence = AbstractPersistence.get();
			PersistentBean eventBean = (PersistentBean) event.getAffectedOwnerOrNull();

			PersistentCollection list = event.getCollection();
			Customer customer = persistence.getUser().getCustomer();
			Module module = customer.getModule(eventBean.getBizModule());
			Document document = module.getDocument(customer, eventBean.getBizDocument());
			for (Attribute attribute : document.getAllAttributes(customer)) {
				if (attribute instanceof Collection) {
					CollectionImpl collection = (CollectionImpl) attribute;
					if (collection.isComplexOrdering()) {
						if (BindUtil.get(eventBean, attribute.getName()) == list) {
							BindUtil.sortCollectionByMetaData(eventBean, collection);
							list.clearDirty();
							break;
						}
					}
				}
			}
		}
		catch (Exception e) {
			throw new HibernateException("Could not order the freshly loaded collection", e);
		}
	}

	@Override
	public boolean requiresPostCommitHanding(EntityPersister persister) {
		return false;
	}
}
