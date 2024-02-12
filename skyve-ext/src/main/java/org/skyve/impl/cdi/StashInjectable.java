package org.skyve.impl.cdi;

import java.io.Serializable;
import java.util.Collection;
import java.util.Map;
import java.util.Set;

import org.skyve.CORE;

import jakarta.enterprise.inject.Alternative;

/**
 * A proxy that can be Serialized with no state and continue to work after deserialization.
 * 
 * @author mike
 */
@Alternative
public class StashInjectable implements Map<String, Object>, Serializable {
	private static final long serialVersionUID = -8415794075289947925L;

	@Override
	public int size() {
		return CORE.getStash().size();
	}

	@Override
	public boolean isEmpty() {
		return CORE.getStash().isEmpty();
	}

	@Override
	public boolean containsKey(Object key) {
		return CORE.getStash().containsKey(key);
	}

	@Override
	public boolean containsValue(Object value) {
		return CORE.getStash().containsValue(value);
	}

	@Override
	public Object get(Object key) {
		return CORE.getStash().get(key);
	}

	@Override
	public Object put(String key, Object value) {
		return CORE.getStash().put(key, value);
	}

	@Override
	public Object remove(Object key) {
		return CORE.getStash().remove(key);
	}

	@Override
	public void putAll(Map<? extends String, ? extends Object> m) {
		CORE.getStash().putAll(m);
	}

	@Override
	public void clear() {
		CORE.getStash().clear();
	}

	@Override
	public Set<String> keySet() {
		return CORE.getStash().keySet();
	}

	@Override
	public Collection<Object> values() {
		return CORE.getStash().values();
	}

	@Override
	public Set<Entry<String, Object>> entrySet() {
		return CORE.getStash().entrySet();
	}
}
