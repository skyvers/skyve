package org.skyve.impl.metadata.repository;

import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReadWriteLock;
import java.util.concurrent.locks.ReentrantReadWriteLock;
import java.util.function.Consumer;
import java.util.function.Function;

import org.skyve.impl.metadata.repository.behaviour.ActionMetaData;
import org.skyve.impl.metadata.repository.behaviour.BizletMetaData;
import org.skyve.impl.metadata.repository.router.Router;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.view.View;

/**
 * Extends the UnsynchronisedDynamicRepository to add Read/Write locking.
 * 
 * Use the 2 withLock() method variants to mutate the repository in a "critical section".
 * All read methods are protected by read locking.
 * All write methods are not protected and should be executed in the function/consumer argument of the withLock method.
 * 
 * This class should provide adequate performance for frequent reads and infrequent writes and 
 * since it will most probably be a repository in a repository chain anyway, the reduced concurrency won't matter too much.
 * 
 * @author mike
 */
public class LockableDynamicRepository extends UnsynchronisedDynamicRepository {
	// Allow many reads but only 1 thread of execution to mutate the cache. 
	private ReadWriteLock lock = new ReentrantReadWriteLock();
	private Lock read = lock.readLock();
	private Lock write = lock.writeLock();
	
	/**
	 * Executes a repository mutation while holding the write lock.
	 *
	 * <p>Side effects: blocks all concurrent readers and writers until
	 * {@code function} returns or throws.
	 *
	 * @param <R> the function result type
	 * @param function the callback to execute in the critical section
	 * @return the callback result
	 */
	public <R> R withLock(Function<LockableDynamicRepository, R> function) {
		write.lock();
		try {
			return function.apply(this);
		}
		finally {
			write.unlock();
		}
	}

	/**
	 * Executes a repository mutation while holding the write lock.
	 *
	 * <p>Use this overload for mutations that do not produce a return value.
	 *
	 * @param consumer the callback to execute in the critical section
	 */
	public void withLock(Consumer<LockableDynamicRepository> consumer) {
		write.lock();
		try {
			consumer.accept(this);
		}
		finally {
			write.unlock();
		}
	}
	
	/**
	 * Returns the current router under a read lock.
	 *
	 * @return the current router, or {@code null} when none has been configured
	 */
	@Override
	public Router getRouter() {
		read.lock();
		try {
			return super.getRouter();
		}
		finally {
			read.unlock();
		}
	}
	
	/**
	 * Resolves a customer by name under a read lock.
	 *
	 * @param customerName the customer name
	 * @return the customer, or {@code null} if no matching customer is cached
	 */
	@Override
	public Customer getCustomer(String customerName) {
		read.lock();
		try {
			return super.getCustomer(customerName);
		}
		finally {
			read.unlock();
		}
	}
	
	/**
	 * Resolves a module for a customer under a read lock.
	 *
	 * @param customer the owning customer
	 * @param moduleName the module name
	 * @return the module, or {@code null} if no matching module is cached
	 */
	@Override
	public Module getModule(Customer customer, String moduleName) {
		read.lock();
		try {
			return super.getModule(customer, moduleName);
		}
		finally {
			read.unlock();
		}
	}
	
	/**
	 * Resolves a document for a module under a read lock.
	 *
	 * @param customer the owning customer
	 * @param module the owning module
	 * @param documentName the document name
	 * @return the document, or {@code null} if no matching document is cached
	 */
	@Override
	public Document getDocument(Customer customer, Module module, String documentName) {
		read.lock();
		try {
			return super.getDocument(customer, module, documentName);
		}
		finally {
			read.unlock();
		}
	}
	
	/**
	 * Resolves a view definition under a read lock.
	 *
	 * @param uxui the UX/UI variant key
	 * @param customer the owning customer
	 * @param document the owning document
	 * @param name the view name
	 * @return the view, or {@code null} if no matching view is cached
	 */
	@Override
	public View getView(String uxui, Customer customer, Document document, String name) {
		read.lock();
		try {
			return super.getView(uxui, customer, document, name);
		}
		finally {
			read.unlock();
		}
	}
	
	/**
	 * Resolves action metadata for a document action under a read lock.
	 *
	 * @param customer the owning customer
	 * @param document the owning document
	 * @param actionName the action name
	 * @return action metadata, or {@code null} if no matching action is cached
	 */
	@Override
	public ActionMetaData getMetaDataAction(Customer customer, Document document, String actionName) {
		read.lock();
		try {
			return super.getMetaDataAction(customer, document, actionName);
		}
		finally {
			read.unlock();
		}
	}
	
	/**
	 * Resolves Bizlet metadata for a document under a read lock.
	 *
	 * @param customer the owning customer
	 * @param document the owning document
	 * @return Bizlet metadata, or {@code null} if none is cached
	 */
	@Override
	public BizletMetaData getMetaDataBizlet(Customer customer, Document document) {
		read.lock();
		try {
			return super.getMetaDataBizlet(customer, document);
		}
		finally {
			read.unlock();
		}
	}
	
	/**
	 * Resolves a repository key using vtable lookup under a read lock.
	 *
	 * @param customerName the customer name used for override resolution
	 * @param key the logical repository key
	 * @return the resolved key, or {@code null} when no mapping exists
	 */
	@Override
	public String vtable(String customerName, String key) {
		read.lock();
		try {
			return super.vtable(customerName, key);
		}
		finally {
			read.unlock();
		}
	}
}
