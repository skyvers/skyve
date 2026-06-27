package org.skyve.metadata.model;

import java.util.List;

import org.skyve.metadata.NamedMetaData;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Interface;
import org.skyve.util.Util;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Common metadata contract for any Skyve model that carries typed attributes.
 *
 * <p>{@code Model} is the shared ancestor of {@link org.skyve.metadata.model.document.Document},
 * report models, and list models. It provides access to the attribute list, persistent
 * settings, display aliases, and document inheritance/dynamism metadata.
 *
 * <p>The attribute accessor family has three levels:
 * <ol>
 *   <li>{@link #getAttribute(String)} — looks up an attribute declared directly on this model.</li>
 *   <li>{@link #getPolymorphicAttribute(Customer, String)} — resolves an attribute taking
 *       customer overrides and polymorphic inheritance into account.</li>
 *   <li>{@link #getAllAttributes(Customer)} — returns all attributes including those inherited
 *       from parent documents.</li>
 * </ol>
 *
 * @see org.skyve.metadata.model.document.Document
 */
public interface Model extends NamedMetaData {
	/**
	 * Returns the name of the module that owns this model.
	 *
	 * @return the owning module name; never {@code null}
	 */
	@Nonnull String getOwningModuleName();

	/**
	 * Returns the attribute with the given name declared directly on this model.
	 *
	 * @param name  the attribute name; must not be {@code null}
	 * @return the attribute, or {@code null} if not found
	 */
	@Nullable Attribute getAttribute(@Nonnull String name);
	
	/**
	 * Returns the attribute with the given name, resolving customer-specific overrides
	 * and polymorphic inheritance.
	 *
	 * @param customer  the customer context for override resolution; must not be {@code null}
	 * @param name      the attribute name; must not be {@code null}
	 * @return the resolved attribute, or {@code null} if not found
	 */
	@Nullable Attribute getPolymorphicAttribute(@Nonnull Customer customer, @Nonnull String name);

	/**
	 * Returns the Java interfaces that this model's generated domain class must implement.
	 *
	 * @return an ordered list of interface contracts; never {@code null}
	 */
	@Nonnull List<? extends Interface> getInterfaces();

	/**
	 * Returns the attributes declared directly on this model, in declaration order.
	 *
	 * @return an ordered list of attributes; never {@code null}
	 */
	@Nonnull List<? extends Attribute> getAttributes();

	/**
	 * Get the attributes for this document and any super-documents for the given customer.
	 * @param customer	The given customer
	 * @return	All the attributes.
	 */
	@Nonnull List<? extends Attribute> getAllAttributes(@Nonnull Customer customer);

	/**
	 * Returns the i18n resource key (or literal string) for the plural display name of this model.
	 *
	 * @return the plural alias; may be {@code null}
	 * @see #getLocalisedPluralAlias()
	 */
	String getPluralAlias();

	default String getLocalisedPluralAlias() {
		return Util.i18n(getPluralAlias());
	}
	
	/**
	 * Returns the i18n resource key (or literal string) for the singular display name of this model.
	 *
	 * @return the singular alias; may be {@code null}
	 * @see #getLocalisedSingularAlias()
	 */
	String getSingularAlias();

	default String getLocalisedSingularAlias() {
		return Util.i18n(getSingularAlias());
	}
	
	/**
	 * Returns the i18n resource key (or literal string) for the model description.
	 *
	 * @return the description; may be {@code null}
	 * @see #getLocalisedDescription()
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
	 * Indicates if this document is directly persistent (not mapped without a persistent name).
	 * That is it has a persistent name (identifier).
	 * @return	true if persistent settings are defined with a name, otherwise false.
	 */
	default boolean isPersistable() {
		Persistent p = getPersistent();
		return ((p != null) && (p.getName() != null));
	}
	
	/**
	 * Returns whether this model declares dynamic class loading for its Bizlet, actions,
	 * or other collaborators.
	 *
	 * @return {@code true} if the model uses dynamic class loading
	 */
	boolean isDynamic();

	/**
	 * This document is dynamic or has a dynamic attribute or a relation to a document 
	 * that is dynamic or has a dynamic attribute (recursively evaluated).
	 */
	boolean hasDynamic();
	
	/**
	 * Returns the dynamic class-name overrides for this model.
	 *
	 * @return the dynamic settings, or {@code null} if not configured
	 */
	Dynamic getDynamism();
	
	/**
	 * Returns the parent document descriptor when this model uses document inheritance.
	 *
	 * @return the extends descriptor, or {@code null} if this model has no parent
	 */
	Extends getExtends();

	/**
	 * Returns whether the generated domain class for this model should be declared abstract.
	 *
	 * @return {@code true} if the generated class is abstract
	 */
	boolean isAbstract();
	
	/**
	 * Returns whether changes to bean instances of this model are written to the audit log.
	 *
	 * @return {@code true} if mutations are audited
	 */
	boolean isAudited();;
	
	String getIcon16x16RelativeFileName();

	String getIcon32x32RelativeFileName();

	String getIconStyleClass();
}
