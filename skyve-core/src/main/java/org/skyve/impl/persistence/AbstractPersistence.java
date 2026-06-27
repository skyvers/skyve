package org.skyve.impl.persistence;

import java.util.List;
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
import jakarta.annotation.Nullable;

/**
 * Thread-confined persistence context that forms the foundation for all Skyve
 * persistence back-end implementations.
 *
 * <p>One instance is associated with the current thread via a {@link ThreadLocal}.
 * It is obtained with {@link #get()} and discarded (with transaction commit or rollback)
 * via the {@link #commit} / {@link #rollback} / {@link #dispose} lifecycle methods.
 *
 * <p>Subclasses (e.g. {@code AbstractHibernatePersistence} in {@code skyve-ext})
 * bind this contract to a concrete ORM session.
 *
 * <p>Threading: thread-confined. Instances must not be shared across threads.
 *
 * @see org.skyve.persistence.Persistence
 */
public abstract class AbstractPersistence implements Persistence {
	private static final long serialVersionUID = -766607064543920926L;

	public static Class<? extends AbstractPersistence> IMPLEMENTATION_CLASS;
	public static Class<? extends DynamicPersistence> DYNAMIC_IMPLEMENTATION_CLASS;
	
	protected static int bizKeyLength = Integer.MIN_VALUE;

	// Holds Persistence instances for each thread - removed by commit(true)
	protected static final ThreadLocal<AbstractPersistence> threadLocalPersistence = new ThreadLocal<>();

	/**
	 * Returns the thread-confined persistence instance for the current thread.
	 *
	 * <p>Side effects: lazily creates and stores a new persistence instance in the
	 * thread-local slot when one is not already bound.
	 *
	 * @return the persistence bound to the current thread; never {@code null}
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
	 * Indicates whether a persistence instance is already bound to this thread.
	 *
	 * @return {@code true} when {@link #get()} would reuse an existing instance
	 */
	public static boolean isPresent() {
		return (threadLocalPersistence.get() != null);
	}

	/**
	 * Construct a Persistence implementation based on Skyve system configuration.
	 *
	 * <p>Side effects: instantiates the configured concrete persistence and
	 * dynamic-persistence implementations, and links them via
	 * {@link DynamicPersistence#postConstruct(AbstractPersistence)}.
	 *
	 * @return a newly constructed persistence instance
	 * @throws IllegalArgumentException if either configured implementation class
	 *             cannot be instantiated
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

	/**
	 * Indicates whether this persistence is executing in an asynchronous thread.
	 *
	 * @return {@code true} for job/background execution threads
	 */
	public boolean isAsyncThread() {
		return asyncThread;
	}

	/**
	 * Marks this persistence as running in synchronous or asynchronous mode.
	 *
	 * <p>Side effects: updates timeout-selection behaviour for query execution.
	 *
	 * @param asyncThread {@code true} for background/job execution threads
	 */
	public void setAsyncThread(boolean asyncThread) {
		this.asyncThread = asyncThread;
	}

	@SuppressWarnings("resource")
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

	/**
	 * Returns the mutable conversation stash associated with this persistence.
	 *
	 * <p>Side effects: callers may mutate returned state that is serialized with
	 * the persistence conversation context.
	 *
	 * @return a non-null, mutable sorted map for conversation-scoped values
	 */
	public @Nonnull SortedMap<String, Object> getStash() {
		return stash;
	}

	/**
	 * Replaces the current thread-bound persistence after an error condition.
	 *
	 * <p>Side effects: rolls back and commits-disposes the current persistence,
	 * creates and binds a fresh instance, begins a new transaction, and restores
	 * the previous user context.
	 *
	 * @return the replacement persistence bound to the current thread
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

	/**
	 * Returns the current value for this property from the wrapped metadata.
	 */
	@Override
	public User getUser() {
		return user;
	}

	/**
	 * Sets the effective user context for subsequent persistence operations.
	 *
	 * @param user the active user; must not be {@code null}
	 */
	public void setUser(@Nonnull User user) {
		this.user = user;
	}

	/**
	 * Binds this persistence instance into the current thread-local slot.
	 *
	 * <p>Side effects: overwrites any previously bound persistence for this thread.
	 */
	public final void setForThread() {
		threadLocalPersistence.set(this);
	}
	
	/**
	 * Indicates whether a bean currently has a persisted identity/version.
	 *
	 * @param bean the bean to test
	 * @return {@code true} when the bean is a persistent bean with non-null version
	 */
	@Override
	public final boolean isPersisted(@Nonnull Bean bean) {
		return (bean instanceof PersistentBean persistentBean) && (persistentBean.getBizVersion() != null);
	}

	/**
	 * Disposes all persistence instances managed by this implementation.
	 */
	public abstract void disposeAllPersistenceInstances();

	/**
	 * Produces dialect-specific DDL fragments for schema migration.
	 *
	 * @param dropDDL receives drop statements when requested; may be {@code null}
	 * @param createDDL receives create statements when requested; may be {@code null}
	 * @param updateDDL receives alter/update statements when requested; may be {@code null}
	 */
	public abstract void generateDDL(@Nullable List<String> dropDDL, @Nullable List<String> createDDL, @Nullable List<String> updateDDL);

	/**
	 * Resolves a document to its persistence entity name.
	 *
	 * @param moduleName owning module name
	 * @param documentName document name within the module
	 * @return the persistence entity name used in generated queries
	 */
	public abstract @Nonnull String getDocumentEntityName(@Nonnull String moduleName, @Nonnull String documentName);

	/**
	 * Executes lifecycle logic after a bean has been loaded.
	 *
	 * @param bean the loaded bean
	 * @throws Exception if lifecycle handling fails
	 */
	public abstract void postLoad(@Nonnull PersistentBean bean) throws Exception;

	/**
	 * Executes lifecycle logic before a bean is removed.
	 *
	 * @param bean the bean pending removal
	 * @throws Exception if lifecycle handling fails
	 */
	public abstract void preRemove(@Nonnull PersistentBean bean) throws Exception;

	/**
	 * Executes lifecycle logic after a bean has been removed.
	 *
	 * @param bean the removed bean
	 * @throws Exception if lifecycle handling fails
	 */
	public abstract void postRemove(@Nonnull PersistentBean bean) throws Exception;

	/**
	 * Saves a bean using metadata-derived module and document definitions.
	 *
	 * @param bean the bean to save
	 * @return the managed bean returned by the concrete persistence implementation
	 */
	@Override
	public final <T extends PersistentBean> T save(T bean) {
		Customer customer = user.getCustomer();
		Module module = customer.getModule(bean.getBizModule());
		Document document = module.getDocument(customer, bean.getBizDocument());
		
		return save(document, bean);
	}

	/**
	 * Merges a bean using metadata-derived module and document definitions.
	 *
	 * @param bean the bean to merge
	 * @return the managed merged bean
	 */
	@Override
	public final <T extends PersistentBean> T merge(T bean) {
		Customer customer = user.getCustomer();
		Module module = customer.getModule(bean.getBizModule());
		Document document = module.getDocument(customer, bean.getBizDocument());
		
		return merge(document, bean);
	}

	/**
	 * Deletes a bean using metadata-derived module and document definitions.
	 *
	 * @param bean the bean to delete
	 */
	@Override
	public final <T extends PersistentBean> void delete(T bean) {
		Customer customer = user.getCustomer();
		Module module = customer.getModule(bean.getBizModule());
		Document document = module.getDocument(customer, bean.getBizDocument());
		
		delete(document, bean);
	}

	/**
	 * Retrieves a bean by module/document name and identifier.
	 *
	 * @param moduleName the owning module name
	 * @param documentName the document name within the module
	 * @param id the bean identifier
	 * @return the retrieved bean, or {@code null} when no row exists
	 */
	@Override
	public final <T extends Bean> T retrieve(String moduleName,
												String documentName,
												String id) {
		Customer customer = user.getCustomer();
		Module module = customer.getModule(moduleName);
		Document document = module.getDocument(customer, documentName);
		
		return retrieve(document, id);
	}

	/**
	 * Retrieves and pessimistically locks a bean by module/document name and id.
	 *
	 * @param moduleName the owning module name
	 * @param documentName the document name within the module
	 * @param id the bean identifier
	 * @return the locked bean, or {@code null} when no row exists
	 */
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
