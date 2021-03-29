package org.skyve.impl.web.service.smartclient;

import java.util.Map;
import java.util.Set;
import java.util.TreeMap;

import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;
import org.skyve.impl.metadata.view.TextOutput.Sanitisation;
import org.skyve.metadata.model.document.Document;

public class ViewBindings {
	private static final ViewBinding MUTABLE_UNESCAPED_TEXT = new ViewBinding(true, false, Sanitisation.text);
	
	private String bindingPrefix;

	// binding to mutable/escape/sanitise indicators
	private Map<String, ViewBinding> bindings = new TreeMap<>();
	
	// binding to child bindings
	private Map<String, ViewBindings> children = new TreeMap<>();
	
	private ViewBindings parent;
	
	public ViewBindings(Document document) {
        bindings.put(Bean.DOCUMENT_ID, MUTABLE_UNESCAPED_TEXT);
        if (document.getPersistent() != null) {
			bindings.put(PersistentBean.LOCK_NAME, MUTABLE_UNESCAPED_TEXT);
        }
    }
	
	public String getBindingPrefix() {
	    return bindingPrefix;
	}
	
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
	
	public void putBinding(String binding, boolean mutable, boolean escape, Sanitisation sanitise) {
	    ViewBinding current = bindings.get(binding);
	    if (current == null) {
	        bindings.put(binding, new ViewBinding(mutable, escape, sanitise));
	    }
	    else {
	    	current.merge(mutable, escape, sanitise);
	    }
	}
	
	public Set<String> getBindings() {
		return bindings.keySet();
	}

	public ViewBinding getBinding(String binding) {
		return bindings.get(binding);
	}
	
	public ViewBindings getParent() {
		return parent;
	}

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

	public Set<String> getChildren() {
		return children.keySet();
	}
}
