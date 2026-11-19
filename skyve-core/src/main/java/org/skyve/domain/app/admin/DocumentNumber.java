package org.skyve.domain.app.admin;

import org.skyve.domain.PersistentBean;

import jakarta.annotation.Nullable;

/**
 * Domain contract for document numbering sequences managed by the admin
 * module.
 */
public interface DocumentNumber extends PersistentBean {
	@Nullable String getDocumentNumber();

	void setModuleName(@Nullable String moduleName);
	void setDocumentName(@Nullable String documentName);
	void setSequenceName(@Nullable String fieldName);
	void setDocumentNumber(@Nullable String nextNumber);

}
