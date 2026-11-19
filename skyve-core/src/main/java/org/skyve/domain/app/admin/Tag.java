package org.skyve.domain.app.admin;

import org.skyve.domain.PersistentBean;
import org.skyve.metadata.model.document.ModuleDocument;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Domain contract for reusable tagging records applied to business documents.
 */
public interface Tag extends PersistentBean {
	long countDocument(@Nullable String moduleName, @Nullable String documentName);

	/**
	 * Counts the beans of the referenced document tagged with this tag; a typed shortcut for
	 * {@link #countDocument(String, String)}.
	 *
	 * @param document	the compile-time document reference
	 * @return the number of the document's beans tagged with this tag
	 */
	default long countDocument(@Nonnull ModuleDocument document) {
		return countDocument(document.moduleName(), document.documentName());
	}
	
	void setName(@Nullable String name);
	void setVisible(@Nullable Boolean visible);
}
