package org.skyve.domain;

import java.io.Serializable;
import java.util.Map;

/**
 * 
 */
public interface Bean extends Serializable, Comparable<Bean> {
	/**
	 * 
	 */
	public static final String DOCUMENT_ID = "bizId";

	/**
	 * 
	 */
	public static final String CUSTOMER_NAME = "bizCustomer";

	/**
	 * 
	 */
	public static final String DATA_GROUP_ID = "bizDataGroupId";

	/**
	 * 
	 */
	public static final String USER_ID = "bizUserId";

	/**
	 * 
	 */
	public static final String ORDINAL_NAME = "bizOrdinal";
	
	/**
	 * 
	 */
	public static final String MODULE_KEY = "bizModule";
	
	/**
	 * 
	 */
	public static final String DOCUMENT_KEY = "bizDocument";

	/**
	 * 
	 */
	public static final String BIZ_KEY = "bizKey";
	
	/**
	 * 
	 */
	public static final String CHANGED_KEY = "changed";
	
	/**
	 * 
	 */
	public static final String NOT_CHANGED_KEY = "notChanged";
	
	/**
	 * 
	 */
	public static final String PERSISTED_KEY = "persisted";
	
	/**
	 * 
	 */
	public static final String NOT_PERSISTED_KEY = "notPersisted";
	
	/**
	 * 
	 */
	public static final String CREATED_KEY = "created";
	
	/**
	 * 
	 */
	public static final String NOT_CREATED_KEY = "notCreated";

	/*
	 * 
	 */
	public String getBizId();

	/**
	 * 
	 * @return
	 */
	public String getBizModule();

	/**
	 * 
	 * @return
	 */
	public String getBizDocument();

	/**
	 * 
	 * @return
	 */
	public String getBizCustomer();

	/**
	 * 
	 * @param bizCustomer
	 */
	public void setBizCustomer(String bizCustomer);

	/**
	 * 
	 * @return
	 */
	public String getBizDataGroupId();

	/**
	 * 
	 * @param bizDataGroupId
	 */
	public void setBizDataGroupId(String bizDataGroupId);

	/**
	 * 
	 * @return
	 */
	public String getBizUserId();

	/**
	 * 
	 * @param bizUserId
	 */
	public void setBizUserId(String bizUserId);

	/**
	 * 
	 * @return
	 */
	public String getBizKey();
	
	/**
	 * 
	 * @param conditionName
	 * @return
	 */
	public boolean evaluateCondition(String conditionName);

	/**
	 * Returns the original values that have been changed.
	 * If the property value has not been changed then it is not in this map.
	 * NB This does not follow the bean pattern so that it is not treated as a normal property.
	 * 
	 * @return A map of property name to original replaced property value.
	 */
	public Map<String, Object> originalValues();
	
	/**
	 * The bean has been changed - a property value has been set that was not equal to its original value.
	 * @return	<code>true</code> if the bean has been changed, otherwise <code>false</code>.
	 */
	public boolean isChanged();
	
	/**
	 * 
	 * @return
	 */
	public boolean isNotChanged();

	/**
	 * This method recursively walks the graph of this bean's relations determining if any have changed using isChanged().
	 * Note that this method is not a java bean accessor as it is expected that this call is expensive.
	 * @return	<code>true</code> if the bean or any of its relations have been changed, otherwise <code>false</code>.
	 */
	public boolean hasChanged();
	
	/**
	 * 
	 * @return
	 */
	public boolean isPersisted();

	/**
	 * 
	 * @return
	 */
	public boolean isNotPersisted();

	// determines if the create.xml view is invoked or not
	// Note: defaults to true
	/**
	 * 
	 * @return
	 */
	public boolean isCreated();

	/**
	 * 
	 * @return
	 */
	public boolean isNotCreated();
	
	/**
	 * Indicates if the attributeName references a dynamic attribute.
	 * @param attributeName	The attribute name to test
	 * @return	true if a dynamic attribute, otherwise false.
	 */
	public boolean isDynamic(String attributeName);

	/**
	 * Get a dynamic attribute value.
	 * @param simpleBinding	A non-compound binding (can be indexed or mapped).
	 * @return	The value.
	 */
	public Object getDynamic(String simpleBinding);
	
	/**
	 * Set a dynamic attribute value.
	 * @param simpleBinding	A non-compound binding (can be indexed or mapped).
	 * @param value	The value.
	 */
	public void setDynamic(String simpleBinding, Object value);

	/**
	 * Put a dynamic attribute value to create a new property, if one does not exist.
	 * @param simpleBinding	A non-compound binding (can be indexed or mapped).
	 * @param value	The value.
	 */
	public void putDynamic(String simpleBinding, Object value);

	/**
	 * Put the dynamic map if currently null
	 * or merge into the current dynamic map if not
	 * or set to null if parameter is null.
	 * New properties may be created with this operation.
	 * @param dynamic	The values to set or merge, or null to reset all dynamic values.
	 */
	public void putAllDynamic(Map<String, Object> dynamic);
}
