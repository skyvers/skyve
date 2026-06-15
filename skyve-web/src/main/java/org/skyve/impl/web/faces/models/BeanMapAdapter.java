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

/**
 * Adapts a Skyve {@link Bean} to a mutable map for JSF EL binding resolution.
 *
 * <p>The adapter resolves bindings lazily, caches resolved values in a delegate map, and routes updates
 * through {@link Binder} so conversion and metadata rules remain consistent with framework behaviour.
 */
public final class BeanMapAdapter implements Map<String, Object>, Serializable {
	private static final long serialVersionUID = 3398758718683866619L;

    private static final Logger FACES_LOGGER = Category.FACES.logger();

	private Bean bean;
	private WebContext webContext;
	private Map<String, Object> delegate = new TreeMap<>();
	
	/**
	 * Creates an adapter for the supplied bean and web context.
	 *
	 * @param bean the bean to adapt
	 * @param webContext the active web context
	 */
	public BeanMapAdapter(Bean bean, WebContext webContext) {
		setBean(bean);
		this.webContext = webContext;
	}
	
	/**
	 * Returns the current adapted bean.
	 *
	 * @return the adapted bean
	 */
	public Bean getBean() {
		if (UtilImpl.FACES_TRACE) FACES_LOGGER.trace("BeanMapAdapter.getBean {}", bean);
		return bean;
	}
	
	/**
	 * Replaces the adapted bean and clears resolved binding cache entries.
	 *
	 * @param bean the new adapted bean
	 */
	public void setBean(Bean bean) {
		this.bean = bean;
		delegate.clear();
		if (UtilImpl.FACES_TRACE) FACES_LOGGER.trace("BeanMapAdapter.setBean {}", bean);
	}
	
	/**
	 * Returns the number of cached binding entries currently stored.
	 *
	 * @return the cached entry count
	 */
	@Override
	public int size() {
		return delegate.size();
	}

	/**
	 * Returns whether no bindings are currently cached.
	 *
	 * @return {@code true} when the cache is empty
	 */
	@Override
	public boolean isEmpty() {
		return delegate.isEmpty();
	}

	/**
	 * Returns whether a cached value exists for the supplied key.
	 *
	 * @param key the cache key to test
	 * @return {@code true} when the key exists in the cache
	 */
	@Override
	public boolean containsKey(Object key) {
		if (UtilImpl.FACES_TRACE) FACES_LOGGER.trace("BeanMapAdapter.containsKey {}", key);
		return delegate.containsKey(key);
	}

	/**
	 * Returns whether a cached value equals the supplied value.
	 *
	 * @param value the value to test
	 * @return {@code true} when a cached value matches
	 */
	@Override
	public boolean containsValue(Object value) {
		if (UtilImpl.FACES_TRACE) FACES_LOGGER.trace("BeanMapAdapter.containsValue {}", value);
		return delegate.containsValue(value);
	}

	/**
	 * The key can be any binding expression or a Message expression with {bindings} in it.
	 *
	 * @param key the binding expression key
	 * @return the resolved value
	 */
	@Override
	public Object get(final Object key) {
		return get(key, false, Sanitisation.none);
	}
	
	/**
	 * Resolves a binding value for EL callers with optional escaping and sanitisation.
	 *
	 * @param key the binding expression key
	 * @param escape whether HTML escaping should be applied
	 * @param sanitisationType the sanitisation enum name
	 * @return the resolved value
	 */
	public Object get(final Object key, final boolean escape, final String sanitisationType) {
		final Sanitisation sanitise = (sanitisationType == null) ? null : Sanitisation.valueOf(sanitisationType);
		return get(key, escape, sanitise);
	}
	
	/**
	 * Resolves and caches a binding value with optional escaping and sanitisation.
	 *
	 * Binding resolution intentionally handles multiple value shapes in one place for EL callers.
	 *
	 * @param key the binding expression key
	 * @param escape whether HTML escaping should be applied
	 * @param sanitise the sanitisation strategy
	 * @return the resolved value
	 */
	@SuppressWarnings("java:S3776") // Complexity OK
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

	/**
	 * Sets a binding value on the adapted bean and updates the delegate cache.
	 *
	 * @param key the binding expression key
	 * @param value the value to assign
	 * @return the previous cached value for the key
	 */
	@Override
	public Object put(String key, Object value) {
		if (UtilImpl.FACES_TRACE) FACES_LOGGER.trace("BeanMapAdapter.put {} = {}", key, value);
		set(key, value);
		return delegate.put(key, value);
	}

	/**
	 * Removes and returns a cached binding value for the supplied key.
	 *
	 * @param key the cache key to remove
	 * @return the removed cached value, or {@code null}
	 */
	@Override
	public Object remove(Object key) {
		return delegate.remove(key);
	}

	/**
	 * Adds all entries from the supplied map to the cache.
	 *
	 * @param m the entries to add
	 */
	@Override
	public void putAll(Map<? extends String, ? extends Object> m) {
		delegate.putAll(m);
	}

	/**
	 * Clears all cached binding entries.
	 */
	@Override
	public void clear() {
		delegate.clear();
	}

	/**
	 * Returns the set of cached binding keys.
	 *
	 * @return the cached key set
	 */
	@Override
	public Set<String> keySet() {
		return delegate.keySet();
	}

	/**
	 * Returns the collection of cached binding values.
	 *
	 * @return the cached values collection
	 */
	@Override
	public Collection<Object> values() {
		return delegate.values();
	}

	/**
	 * Returns the cached key-value entry set.
	 *
	 * @return the cached entry set
	 */
	@Override
	public Set<Map.Entry<String, Object>> entrySet() {
		return delegate.entrySet();
	}

	/**
	 * Returns select items for a binding using the same lookup logic as standard Faces actions.
	 *
	 * @param binding the binding expression for the select field
	 * @param includeEmptyItem whether an empty item should be included
	 * @return resolved select items for the binding
	 */
	public List<SelectItem> getSelectItems(String binding, boolean includeEmptyItem) {
		String bizModule = bean.getBizModule();
		String bizDocument = bean.getBizDocument();
		final String key = String.format("%s.%s.%s", bizModule, bizDocument, binding);
		if (UtilImpl.FACES_TRACE) FACES_LOGGER.trace("BeanMapAdapter.getSelectItems - key = {}", key);
		return new GetSelectItemsAction(bean, webContext, binding, includeEmptyItem).execute();
	}
	
	/**
	 * Populates a binding value on the adapted bean after converter-specific preprocessing.
	 *
	 * @param binding the binding expression to populate
	 * @param value the incoming value from JSF
	 */
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

	/**
	 * Compares adapter equality by underlying bean identity equality.
	 *
	 * @param obj the object to compare
	 * @return {@code true} when the compared object represents the same bean
	 */
	@Override
	public boolean equals(Object obj) {
		if (obj instanceof BeanMapAdapter beanMapAdapter) {
			return bean.equals(beanMapAdapter.getBean());
		}
		return bean.equals(obj);
	}
}
