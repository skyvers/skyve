/**
 * Internal implementations of the
 * {@link org.skyve.metadata.model.document.Document} contract and its
 * relationship/condition types.
 *
 * <p>Contains:
 * <ul>
 *   <li>{@code DocumentImpl} — full runtime document model; holds attributes,
 *       conditions, actions, bizlets, view overrides, and persistence mapping.
 *       This is the primary object created when the repository loads a document XML
 *       file.
 *   <li>{@code AssociationImpl} — implements
 *       {@link org.skyve.metadata.model.document.Association}; represents a many-to-one
 *       or one-to-one relationship to another document.
 *   <li>{@code CollectionImpl} — implements
 *       {@link org.skyve.metadata.model.document.Collection}; represents a one-to-many
 *       relationship.
 *   <li>{@code AbstractInverse} — shared base for inverse relationship descriptors.
 *   <li>{@code ConditionImpl} — implements
 *       {@link org.skyve.metadata.model.document.Condition}; a named boolean expression
 *       evaluated against a bean instance.
 * </ul>
 *
 * @see org.skyve.metadata.model.document
 */
package org.skyve.impl.metadata.model.document;
