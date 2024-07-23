package org.skyve.metadata.model;

import java.util.List;

import org.skyve.metadata.NamedMetaData;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Interface;
import org.skyve.util.Util;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * 
 */
public interface Model extends NamedMetaData {
	/**
	 * 
	 * @return
	 */
	@Nonnull String getOwningModuleName();

	/**
	 * 
	 * @param name
	 * @return
	 */
	@Nullable Attribute getAttribute(@Nonnull String name);
	
	@Nullable Attribute getPolymorphicAttribute(@Nonnull Customer customer, @Nonnull String name);

	/**
	 *
	 * @return
	 */
	@Nonnull List<? extends Interface> getInterfaces();

	/**
	 * 
	 * @return
	 */
	@Nonnull List<? extends Attribute> getAttributes();

	/**
	 * Get the attributes for this document and any super-documents for the given customer.
	 * @param customer	The given customer
	 * @return	All the attributes.
	 */
	@Nonnull List<? extends Attribute> getAllAttributes(Customer customer);

	/**
	 * 
	 * @return
	 */
	String getPluralAlias();

	default String getLocalisedPluralAlias() {
		return Util.i18n(getPluralAlias());
	}
	
	/**
	 * 
	 * @return
	 */
	String getSingularAlias();

	default String getLocalisedSingularAlias() {
		return Util.i18n(getSingularAlias());
	}
	
	/**
	 * 
	 * @return
	 */
	String getDescription();
	
	default String getLocalisedDescription() {
		return Util.i18n(getDescription());
	}
	
	/**
	 * Get the persistent settings for this document.
	 * @return	The settings or null if not defined.
	 */
	@Nullable Persistent getPersistent();

	/**
	 * Indicates if this document is directly persistent (not mapped).
	 * That is it has a data store name (identifier).
	 * @return	true if persistent settings are defined with a name, otherwise false.
	 */
	default boolean isPersistable() {
		Persistent p = getPersistent();
		return ((p != null) && (p.getName() != null));
	}
	
	/**
	 * This document is dynamic.
	 * 
	 * @return
	 */
	boolean isDynamic();

	/**
	 * This document is dynamic or has a dynamic attribute or a relation to a document 
	 * that is dynamic or has a dynamic attribute (recursively evaluated).
	 */
	boolean hasDynamic();
	
	/**
	 * Get the dynamic settings.
	 * @return	The settings.
	 */
	Dynamic getDynamism();
	
	/**
	 * 
	 * @return
	 */
	Extends getExtends();

	/**
	 * Should this document be abstract.
	 *
	 * @return
	 */
	boolean isAbstract();
	
	/**
	 * Should this document be audited.
	 * 
	 * @return
	 */
	boolean isAudited();
	
	String getIcon16x16RelativeFileName();

	String getIcon32x32RelativeFileName();

	String getIconStyleClass();
}
