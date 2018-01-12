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
	public List<? extends Attribute> getAllAttributes();

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
	public String getDescription();
	
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

	/**
	 * Should this document be abstract.
	 *
	 * @return
	 */
	public boolean isAbstract();
	
	/**
	 * Should this document be audited.
	 * 
	 * @return
	 */
	public boolean isAudited();
	
	public String getIcon16x16RelativeFileName();

	public String getIcon32x32RelativeFileName();

	public String getIconStyleClass();
}
