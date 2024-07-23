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
	
	public <R> R withLock(Function<LockableDynamicRepository, R> function) {
		write.lock();
		try {
			return function.apply(this);
		}
		finally {
			write.unlock();
		}
	}

	public void withLock(Consumer<LockableDynamicRepository> consumer) {
		write.lock();
		try {
			consumer.accept(this);
		}
		finally {
			write.unlock();
		}
	}
	
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
