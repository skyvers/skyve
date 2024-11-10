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

import jakarta.annotation.Nonnull;

public abstract class AbstractPersistence implements Persistence {
	private static final long serialVersionUID = -766607064543920926L;

	public static Class<? extends AbstractPersistence> IMPLEMENTATION_CLASS;
	public static Class<? extends DynamicPersistence> DYNAMIC_IMPLEMENTATION_CLASS;
	
	protected static int bizKeyLength = Integer.MIN_VALUE;

	// Holds Persistence instances for each thread - removed by commit(true)
	protected static final ThreadLocal<AbstractPersistence> threadLocalPersistence = new ThreadLocal<>();

	/**
	 * Grab a thread-scoped singleton Persistence instance for the current thread.
	 * If one doesn't exist it is created.
	 */
	public static @Nonnull AbstractPersistence get() {
		AbstractPersistence result = threadLocalPersistence.get();
		if (result == null) {
			result = newInstance();
			threadLocalPersistence.set(result);
		}
		return result;
	}

	/**
	 * Indicates if a thread-scoped singleton Persistence instance is associated
	 * with the current thread of execution.
	 */
	public static boolean isPresent() {
		return (threadLocalPersistence.get() != null);
	}

	/**
	 * Construct a Persistence implementation based on Skyve system configuration.
	 * @throws IllegalArgumentException When the persistence class name obtained through configuration can't be loaded.
	 */
	public static @Nonnull AbstractPersistence newInstance()
	throws IllegalArgumentException {
		AbstractPersistence result = null;
		
		try {
			result = IMPLEMENTATION_CLASS.getDeclaredConstructor().newInstance();
		}
		catch (Exception e) {
			throw new IllegalArgumentException(IMPLEMENTATION_CLASS + " was not a good choice.", e);
		}
		
		try {
			result.dynamicPersistence = DYNAMIC_IMPLEMENTATION_CLASS.getDeclaredConstructor().newInstance();
			result.dynamicPersistence.postConstruct(result);
		}
		catch (Exception e) {
			throw new IllegalArgumentException(DYNAMIC_IMPLEMENTATION_CLASS + " was not a good choice.", e);
		}

		return result;
	}
	
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
	
	/**
	 * The bizKey length as determined from hibernate metadata.
	 * @return	The bizKey length.
	 */
	public static int getBizKeyLength() {
		return bizKeyLength;
	}
	
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
