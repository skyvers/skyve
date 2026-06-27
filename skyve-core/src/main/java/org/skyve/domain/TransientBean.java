package org.skyve.domain;

/**
 * Marker interface for Skyve domain objects that are never persisted to the database.
 *
 * <p>Documents declared with {@code <transient/>} in their metadata XML will have their
 * generated domain class implement this interface instead of {@link PersistentBean}.
 * Transient beans exist only in memory for the duration of a conversation or request;
 * they support the full view binding, validation, and action pipeline but cannot be
 * saved, queried, or deleted through {@link org.skyve.persistence.Persistence}.
 *
 * <p>Common uses include:
 * <ul>
 *   <li>Wizard-step documents that gather input before processing.
 *   <li>Report parameter screens.
 *   <li>Virtual documents that aggregate data from multiple sources.
 * </ul>
 *
 * @see Bean
 * @see PersistentBean
 */
public interface TransientBean extends Bean {
	// tagging interface
}