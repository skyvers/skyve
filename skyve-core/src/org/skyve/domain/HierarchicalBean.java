package org.skyve.domain;

import java.util.List;

import org.skyve.domain.messages.DomainException;
import org.skyve.metadata.MetaDataException;

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
	
	public T getParent() throws DomainException, MetaDataException;
	public List<T> getChildren() throws DomainException, MetaDataException;
}
