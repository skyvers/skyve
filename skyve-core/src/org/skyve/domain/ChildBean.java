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
