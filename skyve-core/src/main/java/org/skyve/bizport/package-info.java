/**
 * BizPort spreadsheet import/export API.
 *
 * <p>BizPort is Skyve's mechanism for bulk data exchange via Excel spreadsheets.
 * The model follows a workbook/sheet/column hierarchy:
 * <ul>
 *   <li>{@link org.skyve.bizport.BizPortWorkbook} — the root workbook, which may be
 *       read from an uploaded file or generated from a document structure.</li>
 *   <li>{@link org.skyve.bizport.BizPortSheet} — a single sheet with a typed column
 *       schema and a row cursor for sequential reading or writing.</li>
 *   <li>{@link org.skyve.bizport.BizPortColumn} — metadata for one column: title,
 *       type, validation hints, and optional FK reference to another sheet.</li>
 *   <li>{@link org.skyve.bizport.SheetKey} — a composite key identifying a sheet
 *       by module and document, used for FK relationships between sheets.</li>
 * </ul>
 *
 * <p>To implement an import, use {@link org.skyve.metadata.controller.BizImportAction};
 * to implement an export, use {@link org.skyve.metadata.controller.BizExportAction}.
 *
 * @see org.skyve.metadata.controller.BizImportAction
 * @see org.skyve.metadata.controller.BizExportAction
 */
package org.skyve.bizport;
