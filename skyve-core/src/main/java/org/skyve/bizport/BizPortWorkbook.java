package org.skyve.bizport;

import java.io.IOException;
import java.io.OutputStream;
import java.util.Set;

/**
 * Abstracts an Excel workbook used for BizPort bulk import and export.
 *
 * <p>A workbook holds a collection of {@link BizPortSheet} instances keyed by
 * {@link SheetKey} (module + document). The typical lifecycle is:
 * <ol>
 *   <li>Obtain a workbook from the implementation (via
 *       {@link org.skyve.impl.bizport}) or upload.</li>
 *   <li>Add sheets with {@link #addSheet}; define columns on each sheet.</li>
 *   <li>Call {@link #materialise()} to populate the backing Excel structure with
 *       data rows and validation constraints (dropdown lists, number formats).</li>
 *   <li>Call {@link #write(java.io.OutputStream)} to serialise the workbook to
 *       an {@code .xls} or {@code .xlsx} byte stream for download.</li>
 * </ol>
 *
 * <p>For import, obtain the workbook from the uploaded stream, iterate sheets
 * using the cursor methods on each {@link BizPortSheet}, and read typed values
 * with {@link BizPortSheet#getValue}.
 *
 * @see BizPortSheet
 * @see SheetKey
 * @see org.skyve.metadata.controller.BizImportAction
 * @see org.skyve.metadata.controller.BizExportAction
 */
public interface BizPortWorkbook {
	/**
	 * Supported Excel output formats for {@link BizPortWorkbook#write}.
	 */
	@SuppressWarnings("java:S115") // Enum names are serialized workbook format codes.
	public static enum BizPortFormat {
		/** Legacy Excel 97-2003 binary format. */
		xls,
		/** Modern Office Open XML format (Excel 2007+). */
		xlsx
	}

	/**
	 * Returns the sheet associated with the given key, or {@code null} if no such sheet exists.
	 *
	 * @param key the module/document key identifying the sheet
	 * @return the matching sheet, or {@code null} when the workbook does not contain the key
	 */
	public BizPortSheet getSheet(SheetKey key);
	
	/**
	 * Registers a sheet in this workbook under the given key.
	 *
	 * @param key   the module/document key identifying the sheet
	 * @param sheet the sheet to register; must not be {@code null}
	 *
	 * @implSpec If a sheet already exists for {@code key}, implementations should replace
	 * the existing sheet with the supplied instance.
	 */
	public void addSheet(SheetKey key, BizPortSheet sheet);
	
	/**
	 * Removes the sheet with the given key and returns it, or {@code null} if not found.
	 *
	 * @param key the module/document key identifying the sheet
	 * @return the removed sheet, or {@code null} if no sheet exists for {@code key}
	 */
	public BizPortSheet removeSheet(SheetKey key);
	
	/**
	 * Returns the set of all sheet keys currently registered in this workbook.
	 *
	 * @return the registered sheet keys
	 */
	public Set<SheetKey> getSheetKeys();
	
	/**
	 * Populates the backing Excel data structure from the in-memory sheet and column
	 * definitions, applying data validation, column types, and FK dropdown constraints.
	 * Must be called before {@link #write} on a newly constructed workbook.
	 *
	 * <p>Side effects: mutates workbook state by creating or refreshing backing spreadsheet
	 * structures for all currently registered sheets.
	 */
	public void materialise();

	/**
	 * Serialises the workbook to the given output stream in the workbook's native format.
	 * The format is determined by {@link #getFormat()}.
	 *
	 * @param out the target stream; the caller is responsible for closing it
	 * @throws IOException if an I/O error occurs during serialisation
	 */
	public void write(OutputStream out) throws IOException;
	
	/**
	 * Returns the output format ({@code xls} or {@code xlsx}) for this workbook.
	 *
	 * @return the workbook serialisation format
	 */
	public BizPortFormat getFormat();
}
