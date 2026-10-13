package org.skyve.metadata.module.fluent;

import org.skyve.impl.metadata.repository.module.ModuleDocumentMetaData;
import org.skyve.metadata.module.Module.DocumentRef;

import jakarta.annotation.Nonnull;

/**
 * Builds module-to-document references.
 */
public class FluentModuleDocument {
	private ModuleDocumentMetaData document = null;
	
	/**
	 * Creates a fluent wrapper with a new metadata instance.
	 */
	public FluentModuleDocument() {
		document = new ModuleDocumentMetaData();
	}

	/**
	 * Creates a fluent wrapper around an existing metadata instance.
	 *
	 * @param document The metadata to mutate.
	 */
	public FluentModuleDocument(ModuleDocumentMetaData document) {
		this.document = document;
	}

	/**
	 * Copies module reference details from an existing document reference entry.
	 *
	 * @param documentName The target document name key.
	 * @param ref The source module document reference.
	 * @return this fluent instance.
	 */
	public FluentModuleDocument from(String documentName, DocumentRef ref) {
		moduleRef(ref.getReferencedModuleName());
		ref(documentName);
		defaultQueryName(ref.getDefaultQueryName());
		ref.getProperties().entrySet().forEach(p -> putProperty(p.getKey(), p.getValue()));

		return this;
	}
	
	/**
	 * Sets the referenced module name for this document entry.
	 *
	 * @param moduleName The referenced module name.
	 * @return this fluent instance.
	 */
	public FluentModuleDocument moduleRef(String moduleName) {
		document.setModuleRef(moduleName);
		return this;
	}

	/**
	 * Sets the referenced document name.
	 *
	 * @param documentName The referenced document name.
	 * @return this fluent instance.
	 */
	public FluentModuleDocument ref(String documentName) {
		document.setRef(documentName);
		return this;
	}
	
	/**
	 * Sets the default query name used for this document.
	 *
	 * @param queryName The default query name.
	 * @return this fluent instance.
	 */
	public FluentModuleDocument defaultQueryName(String queryName) {
		document.setDefaultQueryName(queryName);
		return this;
	}

	/**
	 * Adds or replaces a custom property on the wrapped metadata.
	 * Properties keep the order in which they were first added.
	 *
	 * @param k the property name
	 * @param v the property value
	 * @return this builder
	 */
	public @Nonnull FluentModuleDocument putProperty(@Nonnull String k, @Nonnull String v) {
		document.getProperties().put(k, v);
		return this;
	}

	/**
	 * Returns the mutable metadata backing this fluent wrapper.
	 *
	 * @return The module document metadata instance.
	 */
	public ModuleDocumentMetaData get() {
		return document;
	}
}
