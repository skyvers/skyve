package org.skyve.wildcat.web.faces;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;

import javax.faces.model.SelectItem;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.util.Binder;
import org.skyve.wildcat.util.UtilImpl;
import org.skyve.wildcat.web.faces.actions.GetSelectItemsAction;

public final class BeanMapAdapter<T extends Bean> implements Map<String, Object>, Serializable {
	private static final long serialVersionUID = 3398758718683866619L;

	private T bean;
	private Map<String, Object> delegate = new TreeMap<>();
	
	public BeanMapAdapter(T bean) {
		setBean(bean);
	}
	
	public T getBean() {
		if (UtilImpl.FACES_TRACE) UtilImpl.LOGGER.finest("BeanMapAdapter.getBean " + bean);
		return bean;
	}
	
	public void setBean(T bean) {
		this.bean = bean;
		delegate.clear();
		if (UtilImpl.FACES_TRACE) UtilImpl.LOGGER.finest("BeanMapAdapter.setBean " + bean);
	}
	
	@Override
	public int size() {
		return delegate.size();
	}

	@Override
	public boolean isEmpty() {
		return delegate.isEmpty();
	}

	@Override
	public boolean containsKey(Object key) {
		if (UtilImpl.FACES_TRACE) UtilImpl.LOGGER.finest("BeanMapAdapter.containsKey " + key);
		return delegate.containsKey(key);
	}

	@Override
	public boolean containsValue(Object value) {
		if (UtilImpl.FACES_TRACE) UtilImpl.LOGGER.finest("BeanMapAdapter.containsValue " + value);
		return delegate.containsValue(value);
	}

	/**
	 * The key can be any binding expression or a Message expression with {bindings} in it.
	 */
	@Override
	public Object get(final Object key) {
		return new FacesAction<Object>() {
			@Override
			@SuppressWarnings("synthetic-access")
			public Object callback() throws Exception {
//				if (delegate.containsKey(key)) {
//					result = delegate.get(key);
//				}
//				else {
				String binding = (String) key;
				Object result = null;
				
				int curlyBraceIndex = binding.indexOf('{');
				if (curlyBraceIndex > -1) {
					if (curlyBraceIndex == 0) {
						int lastCharIndex = binding.length() - 1;
						if (binding.charAt(lastCharIndex) == '}') {
							result = Binder.getDisplay(CORE.getUser().getCustomer(), bean, binding.substring(1, lastCharIndex));
						}
						else {
							result = Binder.formatMessage(CORE.getUser().getCustomer(), binding, bean);
						}
					}
					else {
						result = Binder.formatMessage(CORE.getUser().getCustomer(), binding, bean);
					}
				}
				else {
					result = get(binding);
				}
				delegate.put(binding, result);

				if (UtilImpl.FACES_TRACE) UtilImpl.LOGGER.finest("BeanMapAdapter.get " + key + " = " + result);
				return result;
			}
		}.execute();
	}

	@Override
	public Object put(String key, Object value) {
		if (UtilImpl.FACES_TRACE) UtilImpl.LOGGER.finest("BeanMapAdapter.put " + key + " to " + value);
		set(key, value);
		return delegate.put(key, value);
	}

	@Override
	public Object remove(Object key) {
		return delegate.remove(key);
	}

	@Override
	public void putAll(Map<? extends String, ? extends Object> m) {
		delegate.putAll(m);
	}

	@Override
	public void clear() {
		delegate.clear();
	}

	@Override
	public Set<String> keySet() {
		return delegate.keySet();
	}

	@Override
	public Collection<Object> values() {
		return delegate.values();
	}

	@Override
	public Set<Map.Entry<String, Object>> entrySet() {
		return delegate.entrySet();
	}

	private Map<String, List<SelectItem>> lists = new TreeMap<>();
	public List<SelectItem> getSelectItems(String binding, boolean includeEmptyItem) {
		String bizModule = bean.getBizModule();
		String bizDocument = bean.getBizDocument();
		final String key = new StringBuilder(64).append(bizModule).append('.').append(bizDocument).append('.').append(binding).toString();
		if (UtilImpl.FACES_TRACE) UtilImpl.LOGGER.finest("BeanMapAdapter.getSelectItems - key = " + key);
		List<SelectItem> result = lists.get(key);
		if (result == null) {
			result = new GetSelectItemsAction(bean, binding, includeEmptyItem).execute();
			lists.put(key, result);
		}
		
		return result;
	}
	
	private Object get(final String binding) throws Exception {
		Object result = Binder.get(bean, binding);
		
		if (result instanceof Bean) {
			result = new BeanMapAdapter<>((Bean) result);
		}
		else if (result instanceof List<?>) {
			@SuppressWarnings("unchecked")
			List<Bean> childBeans = (List<Bean>) result;
			List<BeanMapAdapter<Bean>> adaptedChildBeans = new ArrayList<>();
			for (Bean childBean : childBeans) {
				adaptedChildBeans.add(new BeanMapAdapter<>(childBean));
			}
			result = adaptedChildBeans;
		}
		
		return result;
	}
	
	private void set(final String binding, final Object value) {
		new FacesAction<Void>() {
			@Override
			@SuppressWarnings("synthetic-access")
			public Void callback() throws Exception {
				Object processedValue = value;
				if (value instanceof BeanMapAdapter<?>) {
					processedValue = ((BeanMapAdapter<?>) value).getBean();
				}
				else if (value instanceof String) {
					processedValue = UtilImpl.processStringValue((String) value);
				}
				Binder.convertAndSet(bean, binding, processedValue);
				
				return null;
			}
		}.execute();
	}
	
	// NB - The next 2 methods are overridden to ensure that faces works as expected.
	// It uses equals to set the selected option for a select/combo for instance.
	
	@Override
	public int hashCode() {
		return bean.hashCode();
	}

	@Override
	public boolean equals(Object obj) {
		if (obj instanceof BeanMapAdapter<?>) {
			return bean.equals(((BeanMapAdapter<?>) obj).getBean());
		}
		return bean.equals(obj);
	}
}
