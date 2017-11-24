package org.skyve.bizport;

import java.util.Set;

import org.skyve.domain.messages.UploadException;
import org.skyve.metadata.model.Attribute.AttributeType;

/**
 * Sets up a sheet with a number of columns in it.
 * Rows can be added to the sheet and rows are added and moved using a composite key structure.
 * Usually (unless overridden) the key is of the form "module.document.bizId".
 * The sheet has a cursor which represents the current row.
 */
public interface BizPortSheet {
	/**
	 * Add a column definition to this sheet.
	 * The sheet is not changed by this method - this is an in-memory map mutation only.
	 * 
	 * @param columnBinding	The column binding (the key to the column metadata).
	 * @param column	The column metadata.
	 * @return	The column metadata added.
	 */
	public BizPortColumn addColumn(String columnBinding, BizPortColumn column);
	
	/**
	 * Remove a column definition from this sheet.
	 * 
	 * @param columnBinding	The column binding (the key to the column metadata).
	 * @return	The column metadata removed.
	 */
	public BizPortColumn removeColumn(String columnBinding);

	/**
	 * Get the column metadata by the binding.
	 * 
	 * @param columnBinding	The binding.
	 * @return	The column metadata.
	 */
	public BizPortColumn getColumn(String columnBinding);
	
	/**
	 * Get a set of the column bindings defined in this sheet.
	 * 
	 * @return	A set of the column bindings.
	 */
	public Set<String> getColumnBindings();
	
	/**
	 * Move to a row in this sheet identified by rowKey.
	 * This is a data operation and so the sheet (and workbook) must be materialized.
	 * 
	 * @param rowKey	The components that make up the key of the row.
	 * @return true if a row with the given rowKey exists.
	 */
	public boolean moveToRow(Object... rowKey);
	
	/**
	 * Add a new row to this sheet identified by rowKey.
	 * This is a data operation and so the sheet (and workbook) must be materialized.
	 *
	 * @param rowKey	The components that make up the key of the row.
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
	 * 
	 * @return	The coerced cell value from the current cursor position and column.
	 */
	public <T> T getValue(String columnBinding, 
							AttributeType attributeType,
							UploadException problems);
	
	/**
	 * Set the value in the Excel sheet in the column 
	 * with the given binding for the current cursor row.
	 * 
	 * @param columnBinding	The column binding.
	 * @param value	The value to place in the cell.
	 */
	public void setValue(String columnBinding, Object value);

	/**
	 * @return	The title of the sheet
	 */
	public String getTitle();

	/**
	 * Title must be unique.
	 * @param title	The title to set in the backing Excel sheet.
	 */
	public void setTitle(String title);
	
	/**
	 * 
	 * @param problems
	 * @param column
	 * @param message
	 */
	public void addErrorAtCurrentRow(UploadException problems, 
										BizPortColumn column, 
										String message);

	/**
	 * 
	 * @param problems
	 * @param column
	 * @param message
	 */
	public void addWarningAtCurrentRow(UploadException problems, 
										BizPortColumn column, 
										String message);
}
