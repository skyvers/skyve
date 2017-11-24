package org.skyve.bizport;

import org.skyve.metadata.model.Attribute.AttributeType;

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
	 * 
	 * @return
	 */
	public String getTitle() {
		return title;
	}
	
	/**
	 * 
	 * @param title
	 */
	public void setTitle(String title) {
		this.title = title;
	}
	
	/**
	 * 
	 * @return
	 */
	public String getComment() {
		return comment;
	}
	
	/**
	 * 
	 * @param comment
	 */
	public void setComment(String comment) {
		this.comment = comment;
	}
	
	/**
	 * 
	 * @return
	 */
	public AttributeType getType() {
		return type;
	}
	
	/**
	 * 
	 * @param type
	 */
	public void setType(AttributeType type) {
		this.type = type;
	}
	
	/**
	 * 
	 * @return
	 */
	public String[] getRangeValues() {
		return rangeValues;
	}
	
	/**
	 * 
	 * @param rangeValues
	 */
	public void setRangeValues(String[] rangeValues) {
		this.rangeValues = rangeValues;
	}
	
	/**
	 * 
	 * @return
	 */
	public Object getMinValue() {
		return minValue;
	}
	
	/**
	 * 
	 * @param minValue
	 */
	public void setMinValue(Object minValue) {
		this.minValue = minValue;
	}
	
	/**
	 * 
	 * @return
	 */
	public Object getMaxValue() {
		return maxValue;
	}
	
	/**
	 * 
	 * @param maxValue
	 */
	public void setMaxValue(Object maxValue) {
		this.maxValue = maxValue;
	}

	/**
	 * 
	 * @return
	 */
	public SheetKey getReferencedSheet() {
		return referencedSheet;
	}

	/**
	 * 
	 * @param referencedSheet
	 */
	public void setReferencedSheet(SheetKey referencedSheet) {
		this.referencedSheet = referencedSheet;
	}

	/**
	 * 
	 * @return
	 */
	public int getIndex() {
		return index;
	}
	
	/**
	 * 
	 * @param index
	 */
	public void setIndex(int index) {
		this.index = index;
	}
}
