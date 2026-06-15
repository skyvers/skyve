package org.skyve.metadata.controller;

import org.skyve.bizport.BizPortWorkbook;
import org.skyve.domain.messages.UploadException;
import org.skyve.metadata.MetaData;

/**
 * Consumes a {@link BizPortWorkbook} and imports rows into the domain model.
 *
 * <p>Implementations are registered in the document's {@code <bizImport>} metadata element
 * and invoked when the user uploads a spreadsheet via the
 * {@link ImplicitActionName#BizImport BizImport} action. Validation errors must be
 * accumulated into the supplied {@link UploadException} rather than thrown directly, so
 * that Skyve can display all problems to the user at once.
 *
 * <p>Implementations must be stateless or request-scoped; Skyve creates a new instance
 * per invocation.
 *
 * @see BizExportAction
 * @see ImplicitActionName#BizImport
 */
public abstract class BizImportAction implements MetaData {
	/**
	 * Processes the uploaded workbook and accumulates any validation problems.
	 *
	 * @param bizPortable  the uploaded workbook to import
	 * @param problems     accumulates row-level and field-level validation problems;
	 *                     callers will throw the exception if it is non-empty after return
	 * @throws Exception if the import cannot proceed at all (fatal errors may be thrown;
	 *                   non-fatal row errors should be added to {@code problems} instead)
	 */
	public abstract void bizImport(BizPortWorkbook bizPortable, UploadException problems) throws Exception;
}
