package org.skyve.wildcat.domain;

import java.beans.PropertyDescriptor;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.apache.commons.beanutils.PropertyUtils;
import org.hibernate.collection.PersistentCollection;
import org.skyve.domain.Bean;
import org.skyve.metadata.MetaDataException;
import org.skyve.wildcat.bind.BindUtil;
import org.skyve.wildcat.persistence.AbstractPersistence;

public abstract class AbstractBean implements Bean {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = -5241897716950549433L;

	// Holds the old (replaced) values when a setter is called.
	private Map<String, Object> originalValues = new TreeMap<>();
	
	/**
	 * Take a copy of the old value before setting a new value.
	 * This method ensures that the value is actually going to change.
	 * 
	 * @param propertyName
	 * @param propertyValue
	 */
	protected final void preset(String propertyName, Object propertyValue) {
		try {
			Object oldValue = BindUtil.get(this, propertyName);
			if (oldValue == null) {
				if (propertyValue != null) {
					if (! originalValues.containsKey(propertyName)) {
						originalValues.put(propertyName,  oldValue);
					}
				}
			}
			else {
				if ((propertyValue == null) || (! oldValue.equals(propertyValue))) {
					if (! originalValues.containsKey(propertyName)) {
						originalValues.put(propertyName,  oldValue);
					}
				}
			}
		}
		catch (Exception e) {
			throw new IllegalStateException("Could not preset for bean " + this + " property " + propertyName, e);
		}
	}
	
	@Override
	public Map<String, Object> originalValues() {
		return originalValues;
	}
	
	@Override
	public final boolean isChanged() {
		boolean result = (! originalValues.isEmpty());
		
		// if unchanged, check the collections to see if they're dirty
		if (! result) {
			for (PropertyDescriptor descriptor : PropertyUtils.getPropertyDescriptors(getClass())) {
				Class<?> propertyType = descriptor.getPropertyType();
				if (Collection.class.isAssignableFrom(propertyType)) {
					try {
						Object collection = BindUtil.get(this, descriptor.getName());
						if (collection instanceof PersistentCollection) { // persistent
							result = ((PersistentCollection) collection).isDirty();
						}
						else { // transient
							result = (! ((Collection<?>) collection).isEmpty());
						}
					}
					catch (Exception e) {
						throw new IllegalStateException("Could not determine if a collection is dirty", e);
					}
				}
			}
		}
		
		return result;
	}
	
	@Override
	public final boolean isNotChanged() {
		return originalValues.isEmpty();
	}
	
	@SuppressWarnings("static-method")
	protected final <T extends Bean> T findElementById(List<T> list, String elementBizId) {
		return BindUtil.findElementInCollection(list, elementBizId);
	}
	
	@Override
	public boolean evaluateCondition(String conditionName) throws MetaDataException {
		boolean result = false;

		try {
			result = (Boolean.TRUE.equals(BindUtil.get(this, conditionName)));
		}
		catch (Exception e) {
			throw new MetaDataException("Condition " + this.getBizDocument()+"."+ conditionName + " is not valid", e);
		}

		return result;
	}

	@SuppressWarnings("static-method") // not static because it is used in the generated beans generating warnings
	public boolean isUserInRole(String moduleName, String roleName) {
		return AbstractPersistence.get().getUser().isInRole(moduleName, roleName);
	}

	/**
	 * Determine if the user is in a certain data group
	 * 
	 * @param dataGroupId
	 *            <code>null</code> for admin group or a specific group name.
	 * @return
	 */
	@SuppressWarnings("static-method") // not static because it is used in the generated beans generating warnings
	public boolean isUserInDataGroup(String dataGroupId) {
		String myDataGroupId = AbstractPersistence.get().getUser().getDataGroupId();
		if (dataGroupId == null) // no data group to check
		{
			return (myDataGroupId == null); // check that user has no data group
		}

		return dataGroupId.equals(myDataGroupId); // check whether in same data group
	}

	@Override
	public final boolean isPersisted() {
		return AbstractPersistence.get().isPersisted(this);
	}

	@Override
	public final boolean isNotPersisted() {
		return (! AbstractPersistence.get().isPersisted(this));
	}

	/**
	 * Indicates if the bean has been created fully or not. Defaults to true -
	 * that is - fully created upon construction.
	 */
	@Override
	public boolean isCreated() {
		return true;
	}

	/**
	 * Indicates if the bean has been created fully or not. Defaults to false -
	 * that is - fully created upon construction.
	 */
	@Override
	public boolean isNotCreated() {
		return false;
	}

	/**
	 * Compare this bean to another by bizId.
	 */
	@Override
	public int compareTo(Bean other) {
		return compareTo(this, other);
	}

	/**
	 * Compare 2 beans by bizId.
	 */
	static int compareTo(Bean thisOne, Bean otherOne) {
		if ((thisOne == null) && (otherOne == null)) {
			return 0;
		}
		else if (thisOne == null) {
			return -1;
		}
		else if (otherOne == null) {
			return 1;
		}
		else {
			return thisOne.getBizId().compareTo(otherOne.getBizId());
		}
	}
}