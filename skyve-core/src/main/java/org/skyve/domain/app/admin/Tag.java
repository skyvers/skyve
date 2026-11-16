package org.skyve.domain.app.admin;

import org.skyve.domain.PersistentBean;

import jakarta.annotation.Nullable;

/**
 * Domain contract for reusable tagging records applied to business documents.
 */
public interface Tag extends PersistentBean {
	long countDocument(@Nullable String moduleName, @Nullable String documentName);
	
	void setName(@Nullable String name);
	void setVisible(@Nullable Boolean visible);
}
