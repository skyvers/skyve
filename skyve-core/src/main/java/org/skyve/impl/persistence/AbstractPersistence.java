package org.skyve.impl.persistence;

import java.util.SortedMap;
import java.util.TreeMap;

import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.persistence.DynamicPersistence;
import org.skyve.persistence.Persistence;

public abstract class AbstractPersistence implements Persistence {
	private static final long serialVersionUID = -766607064543920926L;

	public static Class<? extends AbstractPersistence> IMPLEMENTATION_CLASS;
	public static Class<? extends DynamicPersistence> DYNAMIC_IMPLEMENTATION_CLASS;
	
	public static AbstractPersistence get() {
		return threadLocalPersistence.get();
	}

	protected static final ThreadLocal<AbstractPersistence> threadLocalPersistence = new ThreadLocal<>() {
		@Override
		protected synchronized AbstractPersistence initialValue() throws IllegalArgumentException {
			AbstractPersistence persistence = null;
			try {
				persistence = IMPLEMENTATION_CLASS.getDeclaredConstructor().newInstance();
			}
			catch (Exception e) {
				throw new IllegalArgumentException(IMPLEMENTATION_CLASS + " was not a good choice.", e);
			}
			
			try {
				persistence.dynamicPersistence = DYNAMIC_IMPLEMENTATION_CLASS.getDeclaredConstructor().newInstance();
				persistence.dynamicPersistence.postConstruct(persistence);
			}
			catch (Exception e) {
				throw new IllegalArgumentException(DYNAMIC_IMPLEMENTATION_CLASS + " was not a good choice.", e);
			}

			set(persistence);
			return persistence;
		}
	};

	protected transient User user;
	// NB We can never keep a reference to the customer as the app coder could change the customer name on their user at any time.
	//protected transient Customer customer;

	// indicates if this persistence is running in a job or background task thread 
	protected transient boolean asyncThread = false;
	public boolean isAsyncThread() {
		return asyncThread;
	}
	public void setAsyncThread(boolean asyncThread) {
		this.asyncThread = asyncThread;
	}

	protected DynamicPersistence dynamicPersistence;
	
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
	public static AbstractPersistence renewPersistence() {
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

	public void setUser(User user) {
		this.user = user;
	}

	public final void setForThread() {
		threadLocalPersistence.set(this);
	}

	@Override
	public final boolean isPersisted(Bean bean) {
		return (bean instanceof PersistentBean) && (((PersistentBean) bean).getBizVersion() != null);
	}

	public abstract void disposeAllPersistenceInstances();
	public abstract void generateDDL(String dropDDLFilePath, String createDDLFilePath, String updateDDLFilePath);


	public abstract String getDocumentEntityName(String moduleName, String documentName);

	public abstract void postLoad(PersistentBean bean) throws Exception;

	public abstract void preRemove(PersistentBean bean) throws Exception;
	public abstract void postRemove(PersistentBean bean) throws Exception;

	@Override
	public final <T extends PersistentBean> T save(T bean) {
		Customer customer = user.getCustomer();
		Module module = customer.getModule(bean.getBizModule());
		Document document = module.getDocument(customer, bean.getBizDocument());
		
		return save(document, bean);
	}

	@Override
	public final <T extends PersistentBean> T merge(T bean) {
		Customer customer = user.getCustomer();
		Module module = customer.getModule(bean.getBizModule());
		Document document = module.getDocument(customer, bean.getBizDocument());
		
		return merge(document, bean);
	}

	@Override
	public final <T extends PersistentBean> void delete(T bean) {
		Customer customer = user.getCustomer();
		Module module = customer.getModule(bean.getBizModule());
		Document document = module.getDocument(customer, bean.getBizDocument());
		
		delete(document, bean);
	}

	@Override
	public final <T extends Bean> T retrieve(String moduleName,
												String documentName,
												String id) {
		Customer customer = user.getCustomer();
		Module module = customer.getModule(moduleName);
		Document document = module.getDocument(customer, documentName);
		
		return retrieve(document, id);
	}

	@Override
	public final <T extends Bean> T retrieveAndLock(String moduleName,
														String documentName,
														String id) {
		Customer customer = user.getCustomer();
		Module module = customer.getModule(moduleName);
		Document document = module.getDocument(customer, documentName);
		
		return retrieveAndLock(document, id);
	}
}
