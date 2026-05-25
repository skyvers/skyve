package org.skyve.metadata.controller;

import org.skyve.bizport.BizPortWorkbook;
import org.skyve.metadata.MetaData;
import org.skyve.web.WebContext;

/**
 * Produces a {@link BizPortWorkbook} for spreadsheet-based bulk data export.
 *
 * <p>Implementations are registered in the document's {@code <bizExport>} metadata element
 * and invoked when the user activates the {@link ImplicitActionName#BizExport BizExport}
 * action. The returned workbook is streamed to the browser as a downloadable spreadsheet.
 *
 * <p>Implementations must be stateless or request-scoped; Skyve creates a new instance
 * per invocation.
 *
 * @see BizImportAction
 * @see ImplicitActionName#BizExport
 */
public abstract class BizExportAction implements MetaData {
	/**
	 * Builds and returns the workbook to export.
	 *
	 * @param webContext  the current web context providing access to the conversation
	 *                    and session state
	 * @return the populated workbook; never {@code null}
	 * @throws Exception if the export cannot be completed
	 */
	public abstract BizPortWorkbook bizExport(WebContext webContext) throws Exception;
}
