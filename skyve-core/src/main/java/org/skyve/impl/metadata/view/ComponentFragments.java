package org.skyve.impl.metadata.view;

import java.util.TreeMap;
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
	 * The clones are keyed by requested UIXUI and are distinct shared entries by view.getOverriddenUiXUI().
	 * The default UX/UI is keyed by "" and are shard when appropriate.
	 */
	private WeakHashMap<Component, TreeMap<String, ViewImpl>> fragments = new WeakHashMap<>();
	
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

		read.lock(); // take read lock
		try {
			// Check the fragments to see if 1 exists, under read lock
			TreeMap<String, ViewImpl> viewsByUxUi = fragments.get(component);
			result = (viewsByUxUi == null) ? null : viewsByUxUi.get(uxui);
			if (result == null) { // no fragment exists
				// take the write lock
				read.unlock();
				write.lock();
				try {
					try {
						// Clone and mutate the view via the ComponentViewVisitor
						ViewImpl view = UtilImpl.cloneBySerialization(owner);
	
						String widgetId = component.getWidgetId();
						if (widgetId == null) { // full view clone
							view.resolve(uxui, c, m, d, generate);
							result = view;
							ComponentViewVisitor visitor = new ComponentViewVisitor(c, m, d, result, uxui, component.getBinding(), component.getNames(), widgetId);
							visitor.visit();
						}
						else { // partial vie clone for a widget Id
							// Create a new temp view and add the relevant part as a child
							result = new ViewImpl();
							// Note that visit without resolving first works
							ComponentViewVisitor visitor = new ComponentViewVisitor(c, m, d, view, uxui, component.getBinding(), component.getNames(), widgetId);
							visitor.visit();
							result.getContained().addAll(visitor.getContained());
							// Now resolve the new view
							result.resolve(uxui, c, m, d, generate);
						}
	
						// Establish the UX/UI hashmap if required.
						// NB Don't worry about double read for thread-safety as a wrong read will just discard the old map anyway 
						if (viewsByUxUi == null) {
							viewsByUxUi = new TreeMap<>();
							fragments.put(component, viewsByUxUi);
						}
	
						// Keep the vanilla view under the "" key
						// Any non-overridden UX/UI view will reference the 1 vanilla view under ""
						String viewUxUi = result.getOverriddenUxUiName();
						if (viewUxUi == null) { // not overridden by UX/UI
							// Do we have the vanilla view already
							ViewImpl existingView = viewsByUxUi.get("");
							if (existingView == null) { // no vanilla view
								// Add the vanilla view
								viewsByUxUi.put("", result);
								// Add the overridden as the vanilla view
								viewsByUxUi.put(uxui, result);
							}
							else { // we have the vanilla view
								// Add the overridden as the vanilla view
								viewsByUxUi.put(uxui, existingView);
							}
						}
						else { // overridden by UX/UI
							// Add the overridden view
							viewsByUxUi.put(uxui, result);
						}
					}
					finally {
						read.lock(); // down grade to read before releasing write
					}
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
