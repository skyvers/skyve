package org.skyve.impl.web.service.smartclient;

import java.util.Map;
import java.util.Set;
import java.util.TreeMap;

import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;
import org.skyve.metadata.model.document.Document;

public class ViewBindings {
	private String bindingPrefix;

	// binding to writable indicator
	private Map<String, Boolean> bindings = new TreeMap<>();
	
	// binding to child bindings
	private Map<String, ViewBindings> children = new TreeMap<>();
	
	private ViewBindings parent;
	
	public ViewBindings(Document document) {
        bindings.put(Bean.DOCUMENT_ID, Boolean.TRUE);
        if (document.getPersistent() != null) {
			bindings.put(PersistentBean.LOCK_NAME, Boolean.TRUE);
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
	
	public void putBinding(String binding, boolean writable) {
	    Boolean currentlyWritable = bindings.get(binding);
	    if (currentlyWritable == null) {
	        bindings.put(binding, Boolean.valueOf(writable));
	    }
	    else if (writable && (! currentlyWritable.booleanValue())) {
	        bindings.put(binding,  Boolean.TRUE);
	    }
	}
	
	public Set<String> getBindings() {
		return bindings.keySet();
	}

	public boolean isWritable(String binding) {
	    return Boolean.TRUE.equals(bindings.get(binding));
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
