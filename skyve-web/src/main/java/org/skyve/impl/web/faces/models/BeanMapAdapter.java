package org.skyve.impl.web.faces.models;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.faces.FacesAction;
import org.skyve.impl.web.faces.actions.GetSelectItemsAction;
import org.skyve.metadata.view.TextOutput.Sanitisation;
import org.skyve.util.Binder;
import org.skyve.util.OWASP;
import org.skyve.util.logging.Category;
import org.skyve.web.WebContext;
import org.slf4j.Logger;

import jakarta.faces.model.SelectItem;

public final class BeanMapAdapter implements Map<String, Object>, Serializable {
	private static final long serialVersionUID = 3398758718683866619L;

    private static final Logger FACES_LOGGER = Category.FACES.logger();

	private Bean bean;
	private WebContext webContext;
	private Map<String, Object> delegate = new TreeMap<>();
	
	public BeanMapAdapter(Bean bean, WebContext webContext) {
		setBean(bean);
		this.webContext = webContext;
	}
	
	public Bean getBean() {
		if (UtilImpl.FACES_TRACE) FACES_LOGGER.trace("BeanMapAdapter.getBean {}", bean);
		return bean;
	}
	
	public void setBean(Bean bean) {
		this.bean = bean;
		delegate.clear();
		if (UtilImpl.FACES_TRACE) FACES_LOGGER.trace("BeanMapAdapter.setBean {}", bean);
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
		if (UtilImpl.FACES_TRACE) FACES_LOGGER.trace("BeanMapAdapter.containsKey {}", key);
		return delegate.containsKey(key);
	}

	@Override
	public boolean containsValue(Object value) {
		if (UtilImpl.FACES_TRACE) FACES_LOGGER.trace("BeanMapAdapter.containsValue {}", value);
		return delegate.containsValue(value);
	}

	/**
	 * The key can be any binding expression or a Message expression with {bindings} in it.
	 */
	@Override
	public Object get(final Object key) {
		return get(key, false, Sanitisation.none);
	}
	
	/**
	 * For calling from EL
	 */
	public Object get(final Object key, final boolean escape, final String sanitisationType) {
		final Sanitisation sanitise = (sanitisationType == null) ? null : Sanitisation.valueOf(sanitisationType);
		return get(key, escape, sanitise);
	}
	
	private Object get(final Object key, final boolean escape, final Sanitisation sanitise) {
		return new FacesAction<>() {
			@Override
			public Object callback() throws Exception {
				String binding = (String) key;
				Object result = null;
				
				if (BindUtil.containsSkyveExpressions(binding)) {
					if (escape) {
						result = BindUtil.formatMessage(binding, displayValue -> OWASP.sanitiseAndEscapeHtml(sanitise, displayValue), bean);
					}
					else {
						result = BindUtil.formatMessage(binding, displayValue -> OWASP.sanitise(sanitise, displayValue), bean);
					}
				}
				else {
					result = Binder.get(bean, binding);
					
					if (result instanceof String string) {
						// NB Take care of escaped open curly brace
						string = string.replace("\\{", "{");
						if ((sanitise != null) && (! Sanitisation.none.equals(sanitise))) {
							string = OWASP.sanitise(sanitise, string);
						}
						if (escape) {
							string = OWASP.escapeHtml(string);
						}
						result = string;
					}
					else if (result instanceof Bean b) {
						result = new BeanMapAdapter(b, webContext);
					}
					else if (result instanceof List<?>) {
						@SuppressWarnings("unchecked")
						List<Bean> childBeans = (List<Bean>) result;
						List<BeanMapAdapter> adaptedChildBeans = new ArrayList<>();
						for (Bean childBean : childBeans) {
							adaptedChildBeans.add(new BeanMapAdapter(childBean, webContext));
						}
						result = adaptedChildBeans;
					}
				}
				delegate.put(binding, result);

				if (UtilImpl.FACES_TRACE) FACES_LOGGER.trace("BeanMapAdapter.get {} = {}", key, result);
				return result;
			}
		}.execute();
	}

	@Override
	public Object put(String key, Object value) {
		if (UtilImpl.FACES_TRACE) FACES_LOGGER.trace("BeanMapAdapter.put {} = {}", key, value);
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

	public List<SelectItem> getSelectItems(String binding, boolean includeEmptyItem) {
		String bizModule = bean.getBizModule();
		String bizDocument = bean.getBizDocument();
		final String key = String.format("%s.%s.%s", bizModule, bizDocument, binding);
		if (UtilImpl.FACES_TRACE) FACES_LOGGER.trace("BeanMapAdapter.getSelectItems - key = {}", key);
		return new GetSelectItemsAction(bean, webContext, binding, includeEmptyItem).execute();
	}
	
	private void set(final String binding, final Object value) {
		new FacesAction<Void>() {
			@Override
			public Void callback() throws Exception {
				Object processedValue = value;
				if (value instanceof BeanMapAdapter beanMapAdapter) {
					processedValue = beanMapAdapter.getBean();
				}
				else if (value instanceof String string) {
					String processedStringValue = UtilImpl.processStringValue(string);
					if (processedStringValue != null) {
						processedStringValue = OWASP.unescapeHtmlChars(processedStringValue);
					}
					processedValue = processedStringValue;
				}
				Binder.populateProperty(CORE.getUser(), bean, binding, processedValue, false);
				
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
		if (obj instanceof BeanMapAdapter beanMapAdapter) {
			return bean.equals(beanMapAdapter.getBean());
		}
		return bean.equals(obj);
	}
}
