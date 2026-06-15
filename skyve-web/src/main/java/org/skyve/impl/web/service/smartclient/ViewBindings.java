package org.skyve.impl.web.service.smartclient;

import java.util.Map;
import java.util.Set;
import java.util.TreeMap;

import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.view.TextOutput.Sanitisation;

/**
 * Implements internal web-module behavior for this Skyve runtime concern.
 */
public class ViewBindings {
	private static final ViewBinding MUTABLE_UNESCAPED_TEXT = new ViewBinding(true, false, Sanitisation.text, false);
	
	private String bindingPrefix;

	// binding to mutable/escape/sanitise indicators
	private Map<String, ViewBinding> bindings = new TreeMap<>();
	
	// binding to child bindings
	private Map<String, ViewBindings> children = new TreeMap<>();
	
	private ViewBindings parent;
	
	/**
	 * Creates view binding metadata seeded with framework-managed bindings for the supplied document.
	 *
	 * @param document the view's backing document
	 */
	public ViewBindings(Document document) {
        bindings.put(Bean.DOCUMENT_ID, MUTABLE_UNESCAPED_TEXT);
        if (document.getPersistent() != null) {
			bindings.put(PersistentBean.LOCK_NAME, MUTABLE_UNESCAPED_TEXT);
        }
    }
	
	/**
	 * Returns the local binding prefix represented by this node.
	 *
	 * @return local binding prefix, or {@code null} at the root
	 */
	public String getBindingPrefix() {
	    return bindingPrefix;
	}
	
	/**
	 * Returns the fully qualified binding prefix formed from this node and its ancestors.
	 *
	 * @return fully qualified binding prefix, or {@code null} at the root
	 */
	public String getFullyQualifiedBindingPrefix() {
		if (parent == null) {
			return null;
		}
		
		String fullyQualifiedParentBindingPrefix = parent.getFullyQualifiedBindingPrefix();
		if (fullyQualifiedParentBindingPrefix != null) {
			return fullyQualifiedParentBindingPrefix + '.' + getBindingPrefix();
		}
		
		return getBindingPrefix();
	}
	
	/**
	 * Registers or merges binding metadata for the supplied field binding.
	 *
	 * @param binding binding name relative to this node
	 * @param mutable whether the binding is mutable
	 * @param escape whether the binding should be escaped
	 * @param sanitise sanitisation strategy to apply
	 * @param instantiate whether missing nested values should be instantiated
	 */
	public void putBinding(String binding, boolean mutable, boolean escape, Sanitisation sanitise, boolean instantiate) {
	    ViewBinding current = bindings.get(binding);
	    if (current == null) {
	        bindings.put(binding, new ViewBinding(mutable, escape, sanitise, instantiate));
	    }
	    else {
	    	current.merge(mutable, escape, sanitise, instantiate);
	    }
	}
	
	/**
	 * Returns all binding names registered for this node.
	 *
	 * @return binding names registered for this node
	 */
	public Set<String> getBindings() {
		return bindings.keySet();
	}

	/**
	 * Returns binding metadata for a named binding.
	 *
	 * @param binding binding name
	 * @return binding metadata, or {@code null} when not registered
	 */
	public ViewBinding getBinding(String binding) {
		return bindings.get(binding);
	}
	
	/**
	 * Returns the parent binding node, if one exists.
	 *
	 * @return parent binding node, or {@code null} at the root
	 */
	public ViewBindings getParent() {
		return parent;
	}

	/**
	 * Returns an existing child binding node or creates one for the supplied relationship binding.
	 *
	 * @param binding relationship binding name
	 * @param childDocument child document backing the relationship
	 * @return existing or newly-created child binding node
	 */
	public ViewBindings putOrGetChild(String binding, Document childDocument) {
	    ViewBindings result = children.get(binding);
	    if (result == null) {
	        result = new ViewBindings(childDocument);
	        result.bindingPrefix = binding;
	        result.parent = this;
	        children.put(binding, result);
	    }
	    
	    return result;
	}

	/**
	 * Returns child relationship binding names under this node.
	 *
	 * @return child relationship binding names
	 */
	public Set<String> getChildren() {
		return children.keySet();
	}
}
