package org.skyve.metadata.model;

import java.util.List;

import org.skyve.metadata.NamedMetaData;

/**
 * 
 */
public interface Model extends NamedMetaData {
	/**
	 * 
	 * @return
	 */
	public String getOwningModuleName();

	/**
	 * 
	 * @param name
	 * @return
	 */
	public Attribute getAttribute(String name);

	/**
	 * 
	 * @return
	 */
	public List<? extends Attribute> getAttributes();

	/**
	 * 
	 * @return
	 */
	public String getPluralAlias();

	/**
	 * 
	 * @return
	 */
	public String getSingularAlias();

	/**
	 * 
	 * @return
	 */
	public String getShortDescription();
	
	/**
	 * 
	 * @return
	 */
	public Persistent getPersistent();

	/**
	 * 
	 * @return
	 */
	public Extends getExtends();
}
