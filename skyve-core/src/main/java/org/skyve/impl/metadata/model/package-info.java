/**
 * Internal implementations of the core metadata model contracts
 * ({@link org.skyve.metadata.model} and related interfaces).
 *
 * <p>Contains:
 * <ul>
 *   <li>{@code ModelImpl} — implements {@link org.skyve.metadata.model.Model}, the
 *       base type shared by modules and documents.
 *   <li>{@code AbstractAttribute} — base class for all document attribute
 *       implementations; carries name, display name, description, documentation,
 *       required flag, domain value lookup, change-tracking flag, and audit flag.
 *   <li>{@code InterfaceImpl} — runtime representation of a Java interface declared
 *       on a document, used during domain class generation.
 * </ul>
 *
 * @see org.skyve.metadata.model
 * @see org.skyve.impl.metadata.model.document
 */
package org.skyve.impl.metadata.model;
