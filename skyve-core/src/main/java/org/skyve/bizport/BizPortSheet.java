package org.skyve.bizport;

import java.util.Set;

import org.skyve.domain.messages.UploadException;
import org.skyve.metadata.model.Attribute.AttributeType;

/**
 * Defines the row/column cursor contract for a BizPort import/export sheet.
 *
 * <p>A sheet exposes a column metadata map plus cursor-based row operations.
 * Data operations ({@link #moveToRow(Object...)}, {@link #addRow(Object...)},
 * {@link #getValue(String, AttributeType, UploadException)},
 * {@link #setValue(String, Object)}) operate against the current cursor row.
 *
 * <p>Implementations are typically mutable and thread-confined to a single
 * import/export execution.
 */
public interface BizPortSheet {
	/**
	 * Add a column definition to this sheet.
	 * The sheet is not changed by this method - this is an in-memory map mutation only.
	 * 
	 * @param columnBinding	The column binding (the key to the column metadata).
	 * @param column	The column metadata.
	 * @return	The column metadata added, or the previous metadata for the same binding if replaced.
	 */
	public BizPortColumn addColumn(String columnBinding, BizPortColumn column);
	
	/**
	 * Remove a column definition from this sheet.
	 * 
	 * @param columnBinding	The column binding (the key to the column metadata).
	 * @return	The column metadata removed, or {@code null} if the binding was not present.
	 */
	public BizPortColumn removeColumn(String columnBinding);

	/**
	 * Get the column metadata by the binding.
	 * 
	 * @param columnBinding	The binding.
	 * @return	The column metadata, or {@code null} if no column exists for the binding.
	 */
	public BizPortColumn getColumn(String columnBinding);
	
	/**
	 * Get a set of the column bindings defined in this sheet.
	 * 
	 * @return	The current set of column bindings.
	 */
	public Set<String> getColumnBindings();
	
	/**
	 * Move to a row in this sheet identified by rowKey.
	 * This is a data operation and so the sheet (and workbook) must be materialized.
	 * 
	 * @param rowKey	The components that make up the key of the row, in key-column order.
	 * @return true if a row with the given rowKey exists.
	 */
	public boolean moveToRow(Object... rowKey);
	
	/**
	 * Add a new row to this sheet identified by rowKey.
	 * This is a data operation and so the sheet (and workbook) must be materialized.
	 *
	 * @param rowKey	The components that make up the key of the row, in key-column order.
	 */
	public void addRow(Object... rowKey);
	
	/**
	 * Move the cursor to the next row in the sheet.
	 * 
	 * @return	true if there was a next row, otherwise false.
	 */
	public boolean nextRow();
	
	/**
	 * Wind the cursor back to the start of the sheet.
	 */
	public void resetRow();
	
	/**
	 * Get the value from the current row (of the cursor) and coerce the cell value
	 * to attribute type given.
	 * 
	 * @param <T>	The inferred java type of the value.
	 * @param columnBinding	The binding of the column within this sheet.
	 * @param attributeType	The attribute type to coerce the cell value to.
	 * @param problems	Collector that receives validation or coercion failures for the current row.
	 * 
	 * @return	The coerced cell value from the current cursor position and column, or {@code null}
	 * 			if the source cell is blank or cannot be represented.
	 */
	public <T> T getValue(String columnBinding, 
							AttributeType attributeType,
							UploadException problems);
	
	/**
	 * Set the value in the Excel sheet in the column 
	 * with the given binding for the current cursor row.
	 * 
	 * @param columnBinding	The column binding.
	 * @param value	The value to place in the cell; {@code null} clears the cell.
	 */
	public void setValue(String columnBinding, Object value);

	/**
	 * Returns the title displayed in the workbook tab.
	 *
	 * @return	The unique sheet title.
	 */
	public String getTitle();

	/**
	 * Title must be unique.
	 *
	 * @param title	The title to set in the backing Excel sheet.
	 */
	public void setTitle(String title);
	
	/**
	 * Adds a row-level validation error for the current cursor position.
	 *
	 * @param problems	Collector that receives the error.
	 * @param column	The column the problem relates to.
	 * @param message	Human-readable error detail.
	 */
	public void addErrorAtCurrentRow(UploadException problems, 
										BizPortColumn column, 
										String message);

	/**
	 * Adds a row-level validation warning for the current cursor position.
	 *
	 * @param problems	Collector that receives the warning.
	 * @param column	The column the warning relates to.
	 * @param message	Human-readable warning detail.
	 */
	public void addWarningAtCurrentRow(UploadException problems, 
										BizPortColumn column, 
										String message);
}
