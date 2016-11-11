package org.skyve.domain;

import java.util.List;

/**
 * 
 * @param <T>
 */
public interface HierarchicalBean<T extends Bean> extends Bean {
	/**
	 * 
	 */
	public static final String PARENT_ID = "bizParentId";
	
	/**
	 * 
	 * @return
	 */
	public String getBizParentId();
	
	/**
	 * 
	 * @param parent
	 */
	public void setBizParentId(String bizParentId);
	
	public T getParent();
	public List<T> getChildren();
}
