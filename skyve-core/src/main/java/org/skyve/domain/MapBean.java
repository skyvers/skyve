package org.skyve.domain;

import java.util.Map;
import java.util.TreeMap;

import org.apache.commons.beanutils.DynaProperty;
import org.apache.commons.beanutils.LazyDynaMap;
import org.skyve.CORE;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.domain.AbstractBean;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Condition;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;

public class MapBean extends LazyDynaMap implements Bean {
	private static final long serialVersionUID = 1L;

	public static final String BEAN_PROPERTY_KEY = "bean";
	
	public MapBean(String bizModule, String bizDocument, Map<String, Object> properties) {
		super(properties);
		
		values.put(Bean.MODULE_KEY, bizModule);
		values.put(Bean.DOCUMENT_KEY, bizDocument);
	}

	@Override
	public String getBizId() {
		// Cannot cast the actual value to a string as in the summary line its the count (a long value)
		Object result = get(Bean.DOCUMENT_ID);
		return (result == null) ? null : result.toString();
	}

	@Override
	public String getBizModule() {
		return (String) get(Bean.MODULE_KEY);
	}

	@Override
	public String getBizDocument() {
		return (String) get(Bean.DOCUMENT_KEY);
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
	public String getBizKey() {
		return (String) get(Bean.BIZ_KEY);
	}

	@Override
	public boolean evaluateCondition(String conditionName) {
		// TODO we could evaluate conditions here I suppose using bean shell
		return false;
	}

	@Override
	public Map<String, Object> originalValues() {
		Bean bean = (Bean) values.get(BEAN_PROPERTY_KEY);
		if (bean == null) {
			return new TreeMap<>();
		}

		return bean.originalValues();
	}

	@Override
	public boolean isChanged() {
		Bean bean = (Bean) values.get(BEAN_PROPERTY_KEY);
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
	public boolean hasChanged() {
		Bean bean = (Bean) values.get(BEAN_PROPERTY_KEY);
		if (bean == null) {
			return false;
		}

		return bean.hasChanged();
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
	public Object get(String binding) {
		Object result = null;
		Bean bean = (Bean) values.get(BEAN_PROPERTY_KEY);
		
		// Ensure "bizModule" returns the polymorphic value if appropriate
		if ((bean != null) && Bean.MODULE_KEY.equals(binding)) {
			result = bean.getBizModule();
		}
		// Ensure "bizDocument" returns the polymorphic value if appropriate
		else if ((bean != null) && Bean.DOCUMENT_KEY.equals(binding)) {
			result = bean.getBizDocument();
		}
		else if (isDynaProperty(binding)) {
			result = values.get(binding);
		}
		else {
			if (bean == null) { // there is no "this" bean
				if (Bean.PERSISTED_KEY.equals(binding)) {
					result = AbstractPersistence.get().isPersisted(this) ? Boolean.TRUE : Boolean.FALSE;
				}
				else if (Bean.NOT_PERSISTED_KEY.equals(binding)) {
					result = AbstractPersistence.get().isPersisted(this) ? Boolean.FALSE : Boolean.TRUE;
				}
				else {
					int dotIndex = binding.lastIndexOf('.');
					if (dotIndex < 0) {
						// check if a condition exists
						Customer c = CORE.getCustomer();
						Module m = c.getModule(getBizModule());
						Document d = m.getDocument(c, getBizDocument());
						Condition condition = d.getCondition(binding);
						if (condition != null) {
							result = Boolean.valueOf(BindUtil.evaluateCondition(this, condition.getExpression()));
						}
						else if (Bean.CREATED_KEY.equals(binding)) { // default created condition value
							result = Boolean.TRUE;
						}
						else if (Bean.NOT_CREATED_KEY.equals(binding)) { // default notCreated condition value
							result = Boolean.FALSE;
						}
					}
					else {
						// Check if there is any part of the compound binding present as a dyna property (largest binding first)
						while (dotIndex > 0) { // compound binding
							String bindingPart = binding.substring(0, dotIndex);
							if (isDynaProperty(bindingPart)) {
								try {
									bean = (Bean) values.get(bindingPart);
									if (bean != null) {
										result = BindUtil.get(bean, binding.substring(dotIndex + 1));
									}
									break;
								}
								catch (Exception e) {
									throw new IllegalArgumentException("Binding does not exist - " + binding, e);
								}
							}
							dotIndex = bindingPart.lastIndexOf('.');
						}
						if (dotIndex < 0) { // simple binding
							throw new IllegalArgumentException("Binding does not exist - " + binding);
						}
					}
				}
			}
			else { // try getting the property out of the "this" bean
				try {
					result = BindUtil.get(bean, binding);
				}
				catch (Exception e) {
					throw new IllegalArgumentException("Binding does not exist - " + binding, e);
				}
			}
		}
		
		return result;
	}

	@Override
	public Object get(String binding, int index) {
		Object result = null;
		
		if (isDynaProperty(binding)) {
			result = super.get(binding, index);
		}
		else {
			Object bean = values.get(BEAN_PROPERTY_KEY);
			if (bean == null) {
				throw new IllegalArgumentException("Binding does not exist - " + binding);
			}
			try {
				result = BindUtil.get(bean, new StringBuilder(32).append(binding).append('[').append(index).append(']').toString());
			}
			catch (Exception e) {
				throw new IllegalArgumentException("Binding does not exist - " + binding, e);
			}
		}
		
		return result;
	}

	@Override
	public Object get(String binding, String key) {
		Object result = null;
		
		if (isDynaProperty(binding)) {
			result = super.get(binding, key);
		}
		else {
			Object bean = values.get(BEAN_PROPERTY_KEY);
			if (bean == null) {
				throw new IllegalArgumentException("Binding does not exist - " + binding);
			}
			try {
				result = BindUtil.get(bean, new StringBuilder(32).append(binding).append('(').append(key).append(')').toString());
			}
			catch (Exception e) {
				throw new IllegalArgumentException("Binding does not exist - " + binding, e);
			}
		}

		return result;
	}

	@Override
	public void set(String binding, Object value) {
		if (isDynaProperty(binding)) {
			super.set(binding, value);
		}
		else {
			Object bean = values.get(BEAN_PROPERTY_KEY);
			if (bean == null) {
				throw new IllegalArgumentException("Binding does not exist - " + binding);
			}
			try {
				BindUtil.set(bean, binding, value);
			}
			catch (Exception e) {
				throw new IllegalArgumentException("Binding does not exist - " + binding, e);
			}
		}
	}

	@Override
	public void set(String binding, int index, Object value) {
		if (isDynaProperty(binding)) {
			super.set(binding, index, value);
		}
		else {
			Object bean = values.get(BEAN_PROPERTY_KEY);
			if (bean == null) {
				throw new IllegalArgumentException("Binding does not exist - " + binding);
			}
			try {
				BindUtil.set(bean, 
								new StringBuilder(32).append(binding).append('[').append(index).append(']').toString(),
								value);
			}
			catch (Exception e) {
				throw new IllegalArgumentException("Binding does not exist - " + binding, e);
			}
		}
	}

	@Override
	public void set(String binding, String key, Object value) {
		if (isDynaProperty(binding)) {
			super.set(binding, key, value);
		}
		else {
			Object bean = values.get(BEAN_PROPERTY_KEY);
			if (bean == null) {
				throw new IllegalArgumentException("Binding does not exist - " + binding);
			}
			try {
				BindUtil.set(bean, 
								new StringBuilder(32).append(binding).append('(').append(key).append(')').toString(),
								value);
			}
			catch (Exception e) {
				throw new IllegalArgumentException("Binding does not exist - " + binding, e);
			}
		}
	}

	@Override
	public boolean isDynamic(String attributeName) {
		return isDynaProperty(attributeName);
	}

	@Override
	public Object getDynamic(String simpleBinding) {
		return get(simpleBinding);
	}

	@Override
	public void setDynamic(String simpleBinding, Object value) {
		set(simpleBinding, value);
	}
	
	@Override
	public void setDynamic(Map<String, Object> dynamic) {
		if (dynamic == null) {
			getMap().clear();
		}
		else {
			getMap().putAll(dynamic);
		}
	}
	
	@Override
	public String toString() {
		StringBuilder result = new StringBuilder(128);

		for (DynaProperty property : getDynaProperties()) {
			String propertyName = property.getName();
			result.append(propertyName).append(" = ");
			Object value = get(propertyName);
			// Stop infinite recursion on cyclic graphs
			if (value instanceof MapBean) {
				MapBean bean = (MapBean) value;
				result.append("MapBean:").append(bean.getBizModule()).append(bean.getBizDocument()).append('#').append(bean.getBizId()).append(' ');
			}
			else {
				result.append(value).append(' ');
			}
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
	
	/**
	 * Determine equality by bizId.
	 */
	@Override
	public boolean equals(Object other) {
		if (other instanceof MapBean) {
			String thisBizId = this.getBizId();
			String otherBizId = ((MapBean) other).getBizId();
			return (thisBizId != null) && thisBizId.equals(otherBizId);
		}
		return false;
	}
	
	@Override
	public int hashCode() {
		int result = 0;
		String bizId = this.getBizId();
		if (bizId != null) {
			result = bizId.hashCode();
		}
		return result;
	}
}
