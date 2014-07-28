package org.skyve.wildcat.domain;

import java.util.Map;
import java.util.TreeMap;

import org.apache.commons.beanutils.DynaProperty;
import org.apache.commons.beanutils.LazyDynaMap;
import org.skyve.domain.Bean;
import org.skyve.metadata.MetaDataException;
import org.skyve.persistence.DocumentQuery;
import org.skyve.wildcat.bind.BindUtil;

public final class MapBean extends LazyDynaMap implements Bean {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = 1L;

	public MapBean(String bizModule, String bizDocument, Map<String, Object> properties) {
		super(properties);
		
		values.put(Bean.MODULE_KEY, bizModule);
		values.put(Bean.DOCUMENT_KEY, bizDocument);
	}

	@Override
	public String getBizId() {
		// Cannot cast the actual vaue to a string as in the summary line its the count (a long value)
		Object result = get(Bean.DOCUMENT_ID);
		return (result == null) ? null : result.toString();
	}

	@Override
	public String getBizModule() {
		String result = null;
		Bean bean = (Bean) values.get(DocumentQuery.THIS_ALIAS);
		if (bean != null) {
			// use the polymorphic method to return this
			result = bean.getBizModule();
		}
		else {
			// use what we were given in the constructor
			result = (String) get(Bean.MODULE_KEY);
		}
		return result;
	}

	@Override
	public String getBizDocument() {
		String result = null;
		Bean bean = (Bean) values.get(DocumentQuery.THIS_ALIAS);
		if (bean != null) {
			// use the polymorphic method to return this
			result = bean.getBizDocument();
		}
		else {
			// use what we were given in the constructor
			result = (String) get(Bean.DOCUMENT_KEY);
		}
		return result;
	}

	@Override
	public String getBizCustomer() {
		return (String) get(Bean.CUSTOMER_NAME);
	}

	@Override
	public void setBizCustomer(String bizCustomer) {
		set(Bean.CUSTOMER_NAME, bizCustomer);
	}

	@Override
	public String getBizDataGroupId() {
		return (String) get(Bean.DATA_GROUP_ID);
	}

	@Override
	public void setBizDataGroupId(String bizDataGroupId) {
		set(Bean.DATA_GROUP_ID, bizDataGroupId);
	}

	@Override
	public String getBizUserId() {
		return (String) get(Bean.USER_ID);
	}

	@Override
	public void setBizUserId(String bizUserId) {
		set(Bean.USER_ID, bizUserId);
	}

	@Override
	public boolean evaluateCondition(String conditionName) throws MetaDataException {
		// TODO we could evaluate conditions here I suppose using bean shell
		return false;
	}

	@Override
	public Map<String, Object> originalValues() {
		Bean bean = (Bean) values.get(DocumentQuery.THIS_ALIAS);
		if (bean == null) {
			return new TreeMap<>();
		}

		return bean.originalValues();
	}

	@Override
	public boolean isChanged() {
		Bean bean = (Bean) values.get(DocumentQuery.THIS_ALIAS);
		if (bean == null) {
			return false;
		}

		return bean.isChanged();
	}

	@Override
	public boolean isNotChanged() {
		return (! isChanged());
	}
	
	@Override
	public boolean isPersisted() {
		return false;
	}

	@Override
	public boolean isNotPersisted() {
		return true;
	}

	@Override
	public boolean isCreated() {
		return true;
	}

	@Override
	public boolean isNotCreated() {
		return false;
	}

	public boolean isProperty(String propertyName) {
		return isDynaProperty(propertyName);
	}

	@Override
	public Object get(String propertyName) {
		Object result = null;
		
		if (isDynaProperty(propertyName)) {
			result = values.get(propertyName);
		}
		else {
			Object bean = values.get(DocumentQuery.THIS_ALIAS);
			if (bean == null) {
				throw new IllegalArgumentException("Property name does not exist - " + propertyName);
			}
			try {
				result = BindUtil.get(bean, propertyName);
			}
			catch (Exception e) {
				throw new IllegalArgumentException("Property name does not exist - " + propertyName, e);
			}
		}
		
		return result;
	}

	@Override
	public Object get(String propertyName, int index) {
		Object result = null;
		
		if (isDynaProperty(propertyName)) {
			result = super.get(propertyName, index);
		}
		else {
			Object bean = values.get(DocumentQuery.THIS_ALIAS);
			if (bean == null) {
				throw new IllegalArgumentException("Property name does not exist - " + propertyName);
			}
			try {
				result = BindUtil.get(bean, new StringBuilder(32).append(propertyName).append('[').append(index).append(']').toString());
			}
			catch (Exception e) {
				throw new IllegalArgumentException("Property name does not exist - " + propertyName, e);
			}
		}
		
		return result;
	}

	@Override
	public Object get(String propertyName, String key) {
		Object result = null;
		
		if (isDynaProperty(propertyName)) {
			result = super.get(propertyName, key);
		}
		else {
			Object bean = values.get(DocumentQuery.THIS_ALIAS);
			if (bean == null) {
				throw new IllegalArgumentException("Property name does not exist - " + propertyName);
			}
			try {
				result = BindUtil.get(bean, new StringBuilder(32).append(propertyName).append('(').append(key).append(')').toString());
			}
			catch (Exception e) {
				throw new IllegalArgumentException("Property name does not exist - " + propertyName, e);
			}
		}

		return result;
	}

	@Override
	public void set(String propertyName, Object value) {
		if (isDynaProperty(propertyName)) {
			super.set(propertyName, value);
		}
		else {
			Object bean = values.get(DocumentQuery.THIS_ALIAS);
			if (bean == null) {
				throw new IllegalArgumentException("Property name does not exist - " + propertyName);
			}
			try {
				BindUtil.set(bean, propertyName, value);
			}
			catch (Exception e) {
				throw new IllegalArgumentException("Property name does not exist - " + propertyName, e);
			}
		}
	}

	@Override
	public void set(String propertyName, int index, Object value) {
		if (isDynaProperty(propertyName)) {
			super.set(propertyName, index, value);
		}
		else {
			Object bean = values.get(DocumentQuery.THIS_ALIAS);
			if (bean == null) {
				throw new IllegalArgumentException("Property name does not exist - " + propertyName);
			}
			try {
				BindUtil.set(bean, 
								new StringBuilder(32).append(propertyName).append('[').append(index).append(']').toString(),
								value);
			}
			catch (Exception e) {
				throw new IllegalArgumentException("Property name does not exist - " + propertyName, e);
			}
		}
	}

	@Override
	public void set(String propertyName, String key, Object value) {
		if (isDynaProperty(propertyName)) {
			super.set(propertyName, key, value);
		}
		else {
			Object bean = values.get(DocumentQuery.THIS_ALIAS);
			if (bean == null) {
				throw new IllegalArgumentException("Property name does not exist - " + propertyName);
			}
			try {
				BindUtil.set(bean, 
								new StringBuilder(32).append(propertyName).append('(').append(key).append(')').toString(),
								value);
			}
			catch (Exception e) {
				throw new IllegalArgumentException("Property name does not exist - " + propertyName, e);
			}
		}
	}

	@Override
	public String toString() {
		StringBuilder result = new StringBuilder(128);

		for (DynaProperty property : getDynaProperties()) {
			String propertyName = property.getName();
			result.append(propertyName).append(" = ");
			result.append(get(propertyName)).append(' ');
		}

		return result.toString();
	}

	/**
	 * Compare this bean to another by bizId.
	 */
	@Override
	public int compareTo(Bean other) {
		return AbstractBean.compareTo(this, other);
	}
}
