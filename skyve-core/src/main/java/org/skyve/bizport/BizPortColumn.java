package org.skyve.bizport;

import org.skyve.metadata.model.Attribute.AttributeType;

/**
 * Defines metadata and validation constraints for a single
 * {@link BizPortSheet} column.
 *
 * <p>Column metadata includes presentation ({@link #getTitle()},
 * {@link #getComment()}), type coercion ({@link #getType()}), optional
 * allowable values/ranges, and optional cross-sheet reference metadata.
 *
 * <p>The column index is assigned by sheet assembly code and identifies the
 * physical position in the backing workbook.
 *
 * <p>Threading: not thread-safe; intended for mutation during sheet
 * construction, then read during import/export execution.
 */
public final class BizPortColumn {
	/**
	 * The column title.  Displayed to the user.
	 */
	private String title;

	/**
	 * A comment used as a tooltip/hint.
	 */
	private String comment;
	
	/**
	 * The type of the column, translated to Excel types and masks.
	 */
	private AttributeType type;

	/**
	 * The column index within the sheet.
	 */
	private int index;
	
	/**
	 * The minimum value in a range validation.
	 */
	private Object minValue;
	
	/**
	 * The maximum value in a range definition.
	 */
	private Object maxValue;
	
	/**
	 * A range values constraint - produces a dropdown in Excel.
	 */
	private String[] rangeValues;
	
	/**
	 * The key of the target sheet to which this column references if it is a foreign key column.
	 */
	private SheetKey referencedSheet;
	
	/**
	 * Constructor used when generating a spreadsheet.
	 * 
	 * @param title	The column title.  Displayed to the user.
	 * @param comment	A comment used as a tooltip/hint.
	 * @param type	The type of the column.
	 */
	public BizPortColumn(String title, String comment, AttributeType type) {
		this.title = title;
		this.comment = comment;
		this.type = type;
	}
	
	/**
	 * Constructor used when loading a spreadsheet.
	 * 
	 * @param title	The column title.  Displayed to the user.
	 * @param comment	A comment used as a tooltip/hint.
	 */
	public BizPortColumn(String title, String comment) {
		this.title = title;
		this.comment = comment;
	}

	/**
	 * Returns the user-visible column heading.
	 *
	 * @return the display title shown in the sheet header; may be {@code null} while a
	 *         column definition is being assembled
	 */
	public String getTitle() {
		return title;
	}
	
	/**
	 * Sets the user-visible column heading.
	 *
	 * @param title the display title to use in the sheet header
	 */
	public void setTitle(String title) {
		this.title = title;
	}
	
	/**
	 * Returns the optional column help text.
	 *
	 * @return the tooltip/comment text shown to spreadsheet users, or {@code null} when
	 *         no hint is configured
	 */
	public String getComment() {
		return comment;
	}
	
	/**
	 * Sets optional help text for this column.
	 *
	 * @param comment the tooltip/comment text to attach to the header cell
	 */
	public void setComment(String comment) {
		this.comment = comment;
	}
	
	/**
	 * Returns the Skyve attribute type used to coerce cell values.
	 *
	 * @return the configured attribute type, or {@code null} when type resolution is
	 *         deferred to import parsing
	 */
	public AttributeType getType() {
		return type;
	}
	
	/**
	 * Sets the Skyve attribute type used for value coercion.
	 *
	 * @param type the attribute type for this column
	 */
	public void setType(AttributeType type) {
		this.type = type;
	}
	
	/**
	 * Returns the fixed set of allowed values for this column.
	 *
	 * @return the dropdown/range values, or {@code null} when unrestricted
	 */
	public String[] getRangeValues() {
		return rangeValues;
	}
	
	/**
	 * Sets the fixed set of allowed values for this column.
	 *
	 * <p>When present, implementations typically project these values into Excel data
	 * validation lists.
	 *
	 * @param rangeValues allowable values, in display order; {@code null} removes the
	 *        restriction
	 */
	public void setRangeValues(String[] rangeValues) {
		this.rangeValues = rangeValues;
	}
	
	/**
	 * Returns the minimum allowed value for range validation.
	 *
	 * @return the inclusive lower bound, or {@code null} when no minimum is configured
	 */
	public Object getMinValue() {
		return minValue;
	}
	
	/**
	 * Sets the minimum allowed value for range validation.
	 *
	 * @param minValue the inclusive lower bound, expressed in the column's logical type;
	 *        {@code null} removes the lower-bound constraint
	 */
	public void setMinValue(Object minValue) {
		this.minValue = minValue;
	}
	
	/**
	 * Returns the maximum allowed value for range validation.
	 *
	 * @return the inclusive upper bound, or {@code null} when no maximum is configured
	 */
	public Object getMaxValue() {
		return maxValue;
	}
	
	/**
	 * Sets the maximum allowed value for range validation.
	 *
	 * @param maxValue the inclusive upper bound, expressed in the column's logical type;
	 *        {@code null} removes the upper-bound constraint
	 */
	public void setMaxValue(Object maxValue) {
		this.maxValue = maxValue;
	}

	/**
	 * Returns the referenced sheet key for foreign-key style columns.
	 *
	 * @return the target sheet key used for lookup validation, or {@code null} when this
	 *         column is not a reference
	 */
	public SheetKey getReferencedSheet() {
		return referencedSheet;
	}

	/**
	 * Sets the referenced sheet key for foreign-key style columns.
	 *
	 * @param referencedSheet the target sheet key, or {@code null} to clear reference
	 *        semantics
	 */
	public void setReferencedSheet(SheetKey referencedSheet) {
		this.referencedSheet = referencedSheet;
	}

	/**
	 * Returns the physical column index within the sheet.
	 *
	 * @return zero-based column index in the backing workbook
	 */
	public int getIndex() {
		return index;
	}
	
	/**
	 * Sets the physical column index within the sheet.
	 *
	 * @param index zero-based column index assigned during sheet assembly
	 */
	public void setIndex(int index) {
		this.index = index;
	}
}
