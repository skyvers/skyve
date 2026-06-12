package org.skyve.domain;

import java.beans.Introspector;
import java.io.Serializable;
import java.util.Map;
import java.util.TreeMap;

import org.apache.commons.beanutils.LazyDynaMap;
import org.skyve.CORE;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.domain.AbstractBean;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Condition;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;

/**
 * A schema-less {@link Bean} implementation backed by a {@link LazyDynaMap}.
 *
 * <p>{@code DynamicBean} is used in two distinct scenarios:
 * <ol>
 *   <li><b>Dynamic documents</b> — documents declared as {@code <dynamicDomain/>} in
 *       metadata have no generated Java class; the framework instantiates
 *       {@code DynamicBean} (or one of its subclasses) at runtime and stores attributes
 *       in the underlying {@link LazyDynaMap} property map.
 *   <li><b>Query result carriers</b> — aggregate, projection, and summary queries that
 *       do not map to a single entity class return their rows as {@code DynamicBean}
 *       instances keyed by the column bindings.
 * </ol>
 *
 * <p>Property access delegates first to the dynamic map; if the binding is not in the
 * map, it falls back to an optional wrapped "this" bean stored under the key
 * {@link #BEAN_PROPERTY_KEY}. This allows a {@code DynamicBean} to act as a transparent
 * proxy over a typed persistent bean while adding extra computed or aggregated columns.
 *
 * <p>Binding resolution supports compound (dot-separated) paths, indexed array notation
 * ({@code binding[n]}), and mapped notation ({@code binding(key)}).
 *
 * <p>Threading: not thread-safe; use only within the current thread's request/job context.
 *
 * @see DynamicPersistentBean
 * @see DynamicChildBean
 * @see DynamicHierarchicalBean
 */
public class DynamicBean extends LazyDynaMap implements Bean {
	private static final long serialVersionUID = 1L;

	public static final String BEAN_PROPERTY_KEY = "bean";
	private static final String BINDING_DOES_NOT_EXIST = "Binding does not exist - ";
	
	/**
	 * Creates a new DynamicBean instance.
	 * @param bizModule the bizModule
	 * @param bizDocument the bizDocument
	 * @param properties the properties
	 */
	public DynamicBean(String bizModule, String bizDocument, Map<String, Object> properties) {
		super(properties);
		
		values.put(Bean.MODULE_KEY, bizModule);
		values.put(Bean.DOCUMENT_KEY, bizDocument);
	}

	/**
	 * Returns the bizId.
	 * @return the result
	 */
	@Override
	public String getBizId() {
		// Cannot cast the actual value to a string as in the summary line its the count (a long value)
		Object result = get(Bean.DOCUMENT_ID);
		return (result == null) ? null : result.toString();
	}

	/**
	 * Returns the bizModule.
	 * @return the result
	 */
	@Override
	public String getBizModule() {
		return (String) get(Bean.MODULE_KEY);
	}

	/**
	 * Returns the bizDocument.
	 * @return the result
	 */
	@Override
	public String getBizDocument() {
		return (String) get(Bean.DOCUMENT_KEY);
	}

	/**
	 * Returns the bizCustomer.
	 * @return the result
	 */
	@Override
	public String getBizCustomer() {
		return (String) get(Bean.CUSTOMER_NAME);
	}

	/**
	 * Sets the bizCustomer.
	 * @param bizCustomer the bizCustomer
	 */
	@Override
	public void setBizCustomer(String bizCustomer) {
		set(Bean.CUSTOMER_NAME, bizCustomer);
	}

	/**
	 * Returns the bizDataGroupId.
	 * @return the result
	 */
	@Override
	public String getBizDataGroupId() {
		return (String) get(Bean.DATA_GROUP_ID);
	}

	/**
	 * Sets the bizDataGroupId.
	 * @param bizDataGroupId the bizDataGroupId
	 */
	@Override
	public void setBizDataGroupId(String bizDataGroupId) {
		set(Bean.DATA_GROUP_ID, bizDataGroupId);
	}

	/**
	 * Returns the bizUserId.
	 * @return the result
	 */
	@Override
	public String getBizUserId() {
		return (String) get(Bean.USER_ID);
	}

	/**
	 * Sets the bizUserId.
	 * @param bizUserId the bizUserId
	 */
	@Override
	public void setBizUserId(String bizUserId) {
		set(Bean.USER_ID, bizUserId);
	}

	/**
	 * Returns the bizKey.
	 * @return the result
	 */
	@Override
	public String getBizKey() {
		return (String) get(Bean.BIZ_KEY);
	}

	/**
	 * Executes evaluateCondition.
	 * @param conditionName the conditionName
	 * @return the result
	 */
	@Override
	public boolean evaluateCondition(String conditionName) {
		// TODO we could evaluate conditions here I suppose using bean shell
		return false;
	}

	/**
	 * Executes originalValues.
	 * @return the result
	 */
	@Override
	public Map<String, Serializable> originalValues() {
		Bean bean = (Bean) values.get(BEAN_PROPERTY_KEY);
		if (bean == null) {
			return new TreeMap<>();
		}

		return bean.originalValues();
	}

	/**
	 * Returns whether changed.
	 * @return the result
	 */
	@Override
	public boolean isChanged() {
		Bean bean = (Bean) values.get(BEAN_PROPERTY_KEY);
		if (bean == null) {
			return false;
		}

		return bean.isChanged();
	}

	/**
	 * Returns whether notChanged.
	 * @return the result
	 */
	@Override
	public boolean isNotChanged() {
		return (! isChanged());
	}
	
	/**
	 * Returns whether changed.
	 * @return the result
	 */
	@Override
	public boolean hasChanged() {
		Bean bean = (Bean) values.get(BEAN_PROPERTY_KEY);
		if (bean == null) {
			return false;
		}

		return bean.hasChanged();
	}
	
	
	/**
	 * Returns whether persisted.
	 * @return the result
	 */
	@Override
	public boolean isPersisted() {
		return false;
	}

	/**
	 * Returns whether notPersisted.
	 * @return the result
	 */
	@Override
	public boolean isNotPersisted() {
		return true;
	}

	/**
	 * Returns whether created.
	 * @return the result
	 */
	@Override
	public boolean isCreated() {
		return true;
	}

	/**
	 * Returns whether notCreated.
	 * @return the result
	 */
	@Override
	public boolean isNotCreated() {
		return false;
	}

	/**
	 * Returns whether property.
	 * @param propertyName the propertyName
	 * @return the result
	 */
	public boolean isProperty(String propertyName) {
		return isDynaProperty(propertyName);
	}

	/**
	 * Executes get.
	 * @param binding the binding
	 * @return the result
	 */
	@Override
	@SuppressWarnings("java:S3776") // Complexity OK
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
						// Handle "not" conditions
						String candidateConditionName = binding;
						boolean negated = candidateConditionName.startsWith("not");
						if (negated) {
							candidateConditionName = Introspector.decapitalize(candidateConditionName.substring(3));
						}
						Condition condition = d.getCondition(candidateConditionName);
						if (condition != null) {
							result = Boolean.valueOf(BindUtil.evaluateCondition(this, condition.getExpression()));
							if (negated) {
								if (Boolean.TRUE.equals(result)) {
									result = Boolean.FALSE;
								}
								else if (Boolean.FALSE.equals(result)) {
									result = Boolean.TRUE;
								}
							}
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
									throw new IllegalArgumentException(BINDING_DOES_NOT_EXIST + binding, e);
								}
							}
							dotIndex = bindingPart.lastIndexOf('.');
						}
						if (dotIndex < 0) { // simple binding
							throw new IllegalArgumentException(BINDING_DOES_NOT_EXIST + binding);
						}
					}
				}
			}
			else { // try getting the property out of the "this" bean
				try {
					result = BindUtil.get(bean, binding);
				}
				catch (Exception e) {
					throw new IllegalArgumentException(BINDING_DOES_NOT_EXIST + binding, e);
				}
			}
		}
		
		return result;
	}

	/**
	 * Executes get.
	 * @param binding the binding
	 * @param index the index
	 * @return the result
	 */
	@Override
	public Object get(String binding, int index) {
		Object result = null;
		
		if (isDynaProperty(binding)) {
			result = super.get(binding, index);
		}
		else {
			Object bean = values.get(BEAN_PROPERTY_KEY);
			if (bean == null) {
				throw new IllegalArgumentException(BINDING_DOES_NOT_EXIST + binding);
			}
			try {
				result = BindUtil.get(bean, new StringBuilder(32).append(binding).append('[').append(index).append(']').toString());
			}
			catch (Exception e) {
				throw new IllegalArgumentException(BINDING_DOES_NOT_EXIST + binding, e);
			}
		}
		
		return result;
	}

	/**
	 * Executes get.
	 * @param binding the binding
	 * @param key the key
	 * @return the result
	 */
	@Override
	public Object get(String binding, String key) {
		Object result = null;
		
		if (isDynaProperty(binding)) {
			result = super.get(binding, key);
		}
		else {
			Object bean = values.get(BEAN_PROPERTY_KEY);
			if (bean == null) {
				throw new IllegalArgumentException(BINDING_DOES_NOT_EXIST + binding);
			}
			try {
				result = BindUtil.get(bean, new StringBuilder(32).append(binding).append('(').append(key).append(')').toString());
			}
			catch (Exception e) {
				throw new IllegalArgumentException(BINDING_DOES_NOT_EXIST + binding, e);
			}
		}

		return result;
	}

	/**
	 * Executes set.
	 * @param binding the binding
	 * @param value the value
	 */
	@Override
	public void set(String binding, Object value) {
		if (isDynaProperty(binding)) {
			super.set(binding, value);
		}
		else {
			Object bean = values.get(BEAN_PROPERTY_KEY);
			if (bean == null) {
				throw new IllegalArgumentException(BINDING_DOES_NOT_EXIST + binding);
			}
			try {
				BindUtil.set(bean, binding, value);
			}
			catch (Exception e) {
				throw new IllegalArgumentException(BINDING_DOES_NOT_EXIST + binding, e);
			}
		}
	}

	/**
	 * Executes set.
	 * @param binding the binding
	 * @param index the index
	 * @param value the value
	 */
	@Override
	public void set(String binding, int index, Object value) {
		if (isDynaProperty(binding)) {
			super.set(binding, index, value);
		}
		else {
			Object bean = values.get(BEAN_PROPERTY_KEY);
			if (bean == null) {
				throw new IllegalArgumentException(BINDING_DOES_NOT_EXIST + binding);
			}
			try {
				BindUtil.set(bean, 
								new StringBuilder(32).append(binding).append('[').append(index).append(']').toString(),
								value);
			}
			catch (Exception e) {
				throw new IllegalArgumentException(BINDING_DOES_NOT_EXIST + binding, e);
			}
		}
	}

	/**
	 * Executes set.
	 * @param binding the binding
	 * @param key the key
	 * @param value the value
	 */
	@Override
	public void set(String binding, String key, Object value) {
		if (isDynaProperty(binding)) {
			super.set(binding, key, value);
		}
		else {
			Object bean = values.get(BEAN_PROPERTY_KEY);
			if (bean == null) {
				throw new IllegalArgumentException(BINDING_DOES_NOT_EXIST + binding);
			}
			try {
				BindUtil.set(bean, 
								new StringBuilder(32).append(binding).append('(').append(key).append(')').toString(),
								value);
			}
			catch (Exception e) {
				throw new IllegalArgumentException(BINDING_DOES_NOT_EXIST + binding, e);
			}
		}
	}

	/**
	 * Returns whether dynamic.
	 * @param attributeName the attributeName
	 * @return the result
	 */
	@Override
	public boolean isDynamic(String attributeName) {
		return isDynaProperty(attributeName);
	}

	/**
	 * Returns the dynamic.
	 * @param simpleBinding the simpleBinding
	 * @return the result
	 */
	@Override
	public Object getDynamic(String simpleBinding) {
		if (! isDynaProperty(simpleBinding)) {
			throw new IllegalArgumentException(BINDING_DOES_NOT_EXIST + simpleBinding);
		}
		return get(simpleBinding);
	}

	/**
	 * Sets the dynamic.
	 * @param simpleBinding the simpleBinding
	 * @param value the value
	 */
	@Override
	public void setDynamic(String simpleBinding, Object value) {
		if (! isDynaProperty(simpleBinding)) {
			throw new IllegalArgumentException(BINDING_DOES_NOT_EXIST + simpleBinding);
		}
		set(simpleBinding, value);
	}
	
	/**
	 * Executes putDynamic.
	 * @param simpleBinding the simpleBinding
	 * @param value the value
	 */
	@Override
	public void putDynamic(String simpleBinding, Object value) {
		super.set(simpleBinding, value);
	}
	
	/**
	 * Executes putAllDynamic.
	 * @param dynamic the dynamic
	 */
	@Override
	public void putAllDynamic(Map<String, Object> dynamic) {
		if (dynamic == null) {
			getMap().clear();
		}
		else {
			getMap().putAll(dynamic);
		}
	}
	
	/**
	 * Returns a string representation of this instance.
	 * @return the result
	 */
	@Override
	public String toString() {
		StringBuilder result = new StringBuilder(128);
		result.append("DynamicBean:").append(getBizModule()).append('.').append(getBizDocument()).append('#').append(getBizId());
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
		if (other instanceof DynamicBean otherDynamicBean) {
			String thisBizId = this.getBizId();
			String otherBizId = otherDynamicBean.getBizId();
			return (thisBizId != null) && thisBizId.equals(otherBizId);
		}
		return false;
	}
	
	/**
	 * Returns the hash code.
	 * @return the result
	 */
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
