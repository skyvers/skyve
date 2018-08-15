package org.skyve.metadata.module.query;

/**
 * A column that has a binding or expression that projects a value in the document query.
 * @author mike
 */
public interface MetaDataQueryProjectedColumn extends MetaDataQueryColumn {
	public String getExpression();

	/**
	 *  Indicates whether the column is selected - appears in the projection.
	 */
	public boolean isProjected();

	/**
	 * Indicates if the user can sort this column in list view
	 */
	public boolean isSortable();

	/**
	 *  Indicates if the user can filter this column in list view
	 */
	public boolean isFilterable();

	/**
	 *  Indicates if the user can edit the values in this column in list view
	 */
	public boolean isEditable();
}
