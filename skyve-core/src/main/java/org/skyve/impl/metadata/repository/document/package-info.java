/**
 * JAXB-annotated metadata classes for deserialising document XML files.
 *
 * <p>A Skyve document is described by a {@code <documentName>.xml} file in the module's
 * document directory. The classes in this package form the JAXB binding tree for that
 * file:
 * <ul>
 *   <li>{@code DocumentMetaData} — JAXB root; maps to {@code <document>}; holds the
 *       attribute list, conditions, associations, collections, actions, Bizlet class
 *       reference, persistence settings, and unique constraints.
 *   <li>{@code ConditionMetaData} — one named condition declaration ({@code <condition>}).
 *   <li>{@code UniqueConstraint} — a multi-attribute uniqueness constraint
 *       ({@code <uniqueConstraint>}).
 *   <li>{@code BizKey} — the BizKey expression or class reference
 *       ({@code <bizKey>}).
 *   <li>{@code ParentDocument} — optional declaration of a parent document for a
 *       child/hierarchical document ({@code <parentDocument>}).
 *   <li>{@code FieldReference} — a typed reference to another attribute, used in
 *       constraint and condition declarations.
 *   <li>{@code DynamicClassMapType} / {@code DynamicClassMapEntryType} /
 *       {@code DynamicClassMapAdapter} — JAXB handling for dynamic-class override maps.
 * </ul>
 *
 * @see org.skyve.impl.metadata.model.document
 */
package org.skyve.impl.metadata.repository.document;
