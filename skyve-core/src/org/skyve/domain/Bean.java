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
	 * Perform any post loading/saving activities
	 */
	public void postProcess();
}
