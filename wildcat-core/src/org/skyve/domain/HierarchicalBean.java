package org.skyve.domain;

/**
 * 
 * @param <T>
 */
public interface HierarchicalBean<T extends Bean> extends Bean {
	/**
	 * 
	 */
	public static final String PARENT_ID = "parentBizId";
	
	/**
	 * 
	 * @return
	 */
	public String getParentBizId();
	
	/**
	 * 
	 * @param parent
	 */
	public void setParentBizId(String parentBizId);
}
