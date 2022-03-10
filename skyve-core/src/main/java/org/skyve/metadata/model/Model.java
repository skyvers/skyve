package org.skyve.metadata.model;

import java.util.List;

import org.skyve.metadata.NamedMetaData;
import org.skyve.metadata.model.document.Interface;
import org.skyve.util.Util;

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
	public List<? extends Interface> getInterfaces();

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

	public default String getLocalisedPluralAlias() {
		return Util.i18n(getPluralAlias());
	}
	
	/**
	 * 
	 * @return
	 */
	public String getSingularAlias();

	public default String getLocalisedSingularAlias() {
		return Util.i18n(getSingularAlias());
	}
	
	/**
	 * 
	 * @return
	 */
	public String getDescription();
	
	public default String getLocalisedDescription() {
		return Util.i18n(getDescription());
	}
	
	/**
	 * 
	 * @return
	 */
	public Persistent getPersistent();

	/**
	 * This document is dynamic.
	 * 
	 * @return
	 */
	public boolean isDynamic();

	/**
	 * This document is dynamic or has a dynamic attribute or a relation to a document 
	 * that is dynamic or has a dynamic attribute (recursively evaluated).
	 */
	public boolean hasDynamic();
	
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
