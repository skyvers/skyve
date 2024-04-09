package org.skyve.domain;

/**
 * 
 * @param <T>
 */
public interface ChildBean<T extends Bean> extends Bean {
	/**
	 * 
	 */
	public static final String PARENT_NAME = "parent";
	
	public static final String CHILD_PARENT_NAME_SUFFIX = "." + ChildBean.PARENT_NAME;
	
	public static final String CHILD_PARENT_ID = PARENT_NAME + "_id";

	/**
	 * 
	 * @return
	 */
	public T getParent();
	
	/**
	 * 
	 * @param parent
	 */
	public void setParent(T parent);
	
	/**
	 * 
	 * @return
	 */
	public Integer getBizOrdinal();
	
	/**
	 * 
	 * @param bizOrdinal
	 */
	public void setBizOrdinal(Integer bizOrdinal);
}
