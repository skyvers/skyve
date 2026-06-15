/**
 * Core domain model metadata contracts — attributes, persistence settings, and document hierarchy.
 *
 * <p>This package defines the structural contracts for Skyve's document model layer,
 * sitting between the module and the individual document attribute definitions.
 *
 * <h2>Key types</h2>
 * <ul>
 *   <li>{@link org.skyve.metadata.model.Model} — the common contract for any named entity
 *       that carries typed attributes: documents, report models, and list models all
 *       implement this interface.</li>
 *   <li>{@link org.skyve.metadata.model.Attribute} — describes a single typed field or
 *       relation on a document. Carries the {@link org.skyve.metadata.model.Attribute.AttributeType}
 *       (text, integer, association, collection, etc.), usage, sensitivity, and persistence flags.</li>
 *   <li>{@link org.skyve.metadata.model.Persistent} — describes how a document maps to an
 *       RDBMS table, including table name, optional catalog/schema qualification, and the
 *       inheritance strategy ({@link org.skyve.metadata.model.Persistent.ExtensionStrategy}).</li>
 *   <li>{@link org.skyve.metadata.model.Extends} — records the parent document name when a
 *       document inherits from another, enabling Skyve's document inheritance model.</li>
 *   <li>{@link org.skyve.metadata.model.Dynamic} — carries optional dynamic class name
 *       overrides for Bizlets, actions, images, and models in documents that support
 *       runtime-loaded implementations.</li>
 * </ul>
 *
 * <h2>Sub-package</h2>
 * {@code org.skyve.metadata.model.document} contains the full {@code Document} contract
 * and all document-scoped types (Bizlet, Condition, UniqueConstraint, relations, etc.).
 */
package org.skyve.metadata.model;
