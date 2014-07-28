package org.skyve.wildcat.persistence.hibernate;

import java.util.Date;
import java.util.List;

import org.hibernate.HibernateException;
import org.hibernate.event.FlushEntityEvent;
import org.hibernate.event.FlushEntityEventListener;
import org.hibernate.event.InitializeCollectionEvent;
import org.hibernate.event.InitializeCollectionEventListener;
import org.hibernate.event.PostInsertEvent;
import org.hibernate.event.PostInsertEventListener;
import org.hibernate.event.PostUpdateEvent;
import org.hibernate.event.PostUpdateEventListener;
import org.hibernate.event.PreUpdateEvent;
import org.hibernate.event.PreUpdateEventListener;
import org.hibernate.metadata.ClassMetadata;
import org.hibernate.persister.entity.EntityPersister;
import org.hibernate.type.Type;
import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.types.OptimisticLock;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.wildcat.bind.BindUtil;
import org.skyve.wildcat.domain.AbstractPersistentBean;
import org.skyve.wildcat.metadata.model.document.Collection;
import org.skyve.wildcat.persistence.AbstractPersistence;

public class HibernateListener implements PostUpdateEventListener,
											PostInsertEventListener,
											PreUpdateEventListener,
											InitializeCollectionEventListener,
											FlushEntityEventListener {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = -2075261951031625148L;

	/**
	 * Inject new bizLock
	 */
	@Override
	public boolean onPreUpdate(PreUpdateEvent event) {
		AbstractPersistentBean eventBean = (AbstractPersistentBean) event.getEntity();
		EntityPersister ep = event.getPersister();
		Object[] state = event.getState();
		
		AbstractPersistence persistence = AbstractPersistence.get();
		String entityName = persistence.getDocumentEntityName(eventBean.getBizModule(), eventBean.getBizDocument());
		ClassMetadata cmd = ep.getFactory().getClassMetadata(entityName);
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
		AbstractPersistence persistence = AbstractPersistence.get();
		AbstractPersistentBean eventBean = (AbstractPersistentBean) event.getEntity();
		String entityName = persistence.getDocumentEntityName(eventBean.getBizModule(), eventBean.getBizDocument());
		EntityPersister ep = event.getPersister();
		ClassMetadata cmd = ep.getFactory().getClassMetadata(entityName);
		String[] propertyNames = cmd.getPropertyNames();
		Type[] propertyTypes = cmd.getPropertyTypes();
		Object[] state = event.getState();

		try {
			persistence.index(eventBean, propertyNames, propertyTypes, null, state);
		}
		catch (Exception e) {
			// Cant stop now, after all its only the indexing that is screwed
			e.printStackTrace();
		}
	}

	/**
	 * Reindex any properties that have changed in the CMS.
	 */
	@Override
	public void onPostUpdate(PostUpdateEvent event) {
		AbstractPersistence persistence = AbstractPersistence.get();
		AbstractPersistentBean eventBean = (AbstractPersistentBean) event.getEntity();
		String entityName = persistence.getDocumentEntityName(eventBean.getBizModule(), eventBean.getBizDocument());
		EntityPersister ep = event.getPersister();
		ClassMetadata cmd = ep.getFactory().getClassMetadata(entityName);
		String[] propertyNames = cmd.getPropertyNames();
		Type[] propertyTypes = cmd.getPropertyTypes();
		Object[] oldState = event.getOldState();
		Object[] state = event.getState();

		try {
			// Cant stop now, after all its only the indexing that is screwed
			persistence.index(eventBean, propertyNames, propertyTypes, oldState, state);
		}
		catch (Exception e) {
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
			AbstractPersistentBean eventBean = (AbstractPersistentBean) event.getAffectedOwnerOrNull();
			@SuppressWarnings("unchecked")
			List<Bean> list = (List<Bean>) event.getCollection();
			Customer customer = persistence.getUser().getCustomer();
			Module module = customer.getModule(eventBean.getBizModule());
			Document document = module.getDocument(customer, eventBean.getBizDocument());
			for (String referenceName : document.getReferenceNames()) {
				if (BindUtil.get(eventBean, referenceName) == list) {
					Collection collection = (Collection) document.getReferenceByName(referenceName);
					if (collection.isComplexOrdering()) {
						BindUtil.sortCollectionByMetaData(eventBean, collection);
					}
					break;
				}
			}
		}
		catch (Exception e) {
			throw new HibernateException("Could not order the freshly loaded collection", e);
		}
	}

	@Override
	public void onFlushEntity(FlushEntityEvent evt) throws HibernateException {
//		if (((Bean) evt.getEntity()).isChanged()) {
//System.out.println("FLUSH ENTITY " + evt.getEntity());
//for (String key : ((Bean) evt.getEntity()).originalValues().keySet()) {
//	System.out.println("    " + key);
//}
//		}
	}
}
