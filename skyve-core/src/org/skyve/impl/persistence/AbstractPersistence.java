package org.skyve.impl.persistence;

import java.util.List;
import java.util.SortedMap;
import java.util.TreeMap;

import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.domain.AbstractPersistentBean;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.persistence.Persistence;
import org.skyve.impl.persistence.AbstractPersistence;

public abstract class AbstractPersistence implements Persistence {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = -766607064543920926L;

	public static Class<? extends AbstractPersistence> IMPLEMENTATION_CLASS;
	
	public static AbstractPersistence get() {
		return threadLocalPersistence.get();
	}

	protected static final ThreadLocal<AbstractPersistence> threadLocalPersistence = new ThreadLocal<AbstractPersistence>() {
		@Override
		protected synchronized AbstractPersistence initialValue() throws IllegalArgumentException {
			try {
				AbstractPersistence persistence = IMPLEMENTATION_CLASS.newInstance();
				set(persistence);
				return persistence;
			}
			catch (Exception e) {
				throw new IllegalArgumentException(IMPLEMENTATION_CLASS + " was not a good choice.", e);
			}
		}
	};

	protected transient User user;
	// NB We can never keep a reference to the customer as the app coder could change the customer name on their user at any time.
	//protected transient Customer customer;

	/*
	 * A place (thread-local as it's on persistence), where state can be placed for the duration of the conversation.
	 * Bear in mind that this map is serialised and cached in the conversation.
	 */
	private SortedMap<String, Object> stash = new TreeMap<>();
	public SortedMap<String, Object> getStash() {
		return stash;
	}

	/**
	 * When an error occurs, the state of a persistence is indeterminate. 
	 * You will need to chuck away the old one and use a new one.
	 * This is what this method does.
	 */
	public static AbstractPersistence renewPersistence()
	throws MetaDataException {
		// Get old persistence and close
		AbstractPersistence persistence = AbstractPersistence.get();
		User user = persistence.getUser();
		persistence.rollback();
		persistence.commit(true);

		// Get new persistence
		persistence = AbstractPersistence.get();
		persistence.begin();
		persistence.setUser(user);

		return persistence;
	}

	@Override
	public User getUser() {
		return user;
	}

	@SuppressWarnings("unused")
	public void setUser(User user) 
	throws MetaDataException {
		this.user = user;
	}

	public final void setForThread() {
		threadLocalPersistence.set(this);
	}

	@Override
	public final boolean isPersisted(Bean bean) {
		return (bean instanceof AbstractPersistentBean) && (((PersistentBean) bean).getBizVersion() != null);
	}

	public abstract void disposeAllPersistenceInstances() throws MetaDataException;
	public abstract void generateDDL(List<String> drops, List<String> creates, List<String> updates) 
	throws DomainException, MetaDataException;


	public abstract String getDocumentEntityName(String moduleName, String documentName);

	public abstract void postLoad(AbstractPersistentBean bean) throws Exception;

	public abstract void preRemove(AbstractPersistentBean bean) throws Exception;
	public abstract void postRemove(AbstractPersistentBean bean) throws Exception;

	public abstract void replaceTransientProperties(Document document, Bean targetBean, Bean sourceBean) 
	throws DomainException, MetaDataException;

	@Override
	public final <T extends PersistentBean> T save(T bean)
	throws DomainException, MetaDataException {
		Customer customer = user.getCustomer();
		Module module = customer.getModule(bean.getBizModule());
		Document document = module.getDocument(customer, bean.getBizDocument());
		
		return save(document, bean);
	}

	@Override
	public final <T extends PersistentBean> void delete(T bean)
	throws DomainException, MetaDataException {
		Customer customer = user.getCustomer();
		Module module = customer.getModule(bean.getBizModule());
		Document document = module.getDocument(customer, bean.getBizDocument());
		
		delete(document, bean);
	}

	@Override
	public final <T extends Bean> T retrieve(String moduleName,
												String documentName,
												String id,
												boolean forUpdate)
	throws DomainException, MetaDataException {
		Customer customer = user.getCustomer();
		Module module = customer.getModule(moduleName);
		Document document = module.getDocument(customer, documentName);
		
		return retrieve(document, id, forUpdate);
	}
}
