package org.skyve.impl.metadata.view;

import java.util.WeakHashMap;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReadWriteLock;
import java.util.concurrent.locks.ReentrantReadWriteLock;

import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.module.ModuleImpl;
import org.skyve.impl.metadata.view.component.Component;
import org.skyve.impl.metadata.view.component.ComponentViewVisitor;
import org.skyve.impl.util.UtilImpl;

/**
 * A Thread-Safe (reentrant) Weak Referenced Map of other views' components to the fragments they point to.
 * 
 * @author mike
 */
class ComponentFragments {
	private ViewImpl owner = null;
	
	public ComponentFragments(ViewImpl owner) {
		this.owner = owner;
	}
	
	/**
	 * A WeakHashMap of cloned fragments of the owner view.
	 * They are garbage collected when the component key is garbage collected (ie the client view is replaced)
	 */
	private WeakHashMap<Component, ViewImpl> fragments = new WeakHashMap<>();
	
	/**
	 * Allow many reads but only 1 thread of execution to mutate the fragments map. 
	 */
	private ReadWriteLock lock = new ReentrantReadWriteLock();
	private Lock read = lock.readLock();
	private Lock write = lock.writeLock();
	
	/**
	 * Get a view fragment for a component.
	 * 
	 * @param c	Customer
	 * @param m	Module
	 * @param d	Document
	 * @param uxui	Ux/UI
	 * @param component	The component the fragment is for
	 * @param generate	Whether to generate the access control list for the fragment
	 * @return	The fragment (as a view)
	 */
	ViewImpl get(CustomerImpl c, ModuleImpl m, DocumentImpl d, String uxui, Component component, boolean generate) {
		ViewImpl result = null;

		read.lock();
		try {
			result = fragments.get(component);
			if (result == null) {
				// Clone and mutate the view via the ComponentViewVisitor
				ViewImpl view = UtilImpl.cloneBySerialization(owner);
		
				String widgetId = component.getWidgetId();
				ComponentViewVisitor visitor = new ComponentViewVisitor(c, m, d, view, uxui, component.getBinding(), component.getNames(), widgetId);
				visitor.visit();
				if (widgetId == null) {
					result = view;
				}
				else {
					result = new ViewImpl();
					view.getContained().addAll(visitor.getContained());
					result.resolve(uxui, c, m, d, generate);
				}
					
				read.unlock();
				write.lock();
				try {
					fragments.put(component, result);
					read.lock(); // down grade to read before releasing write
				}
				finally {
					write.unlock(); // unlock write, still holding read
				}
			}
		}
		finally {
			read.unlock();
		}
		return result;
	}
}
