package org.skyve.metadata.module.query;

import org.skyve.metadata.view.FormattedText;

/**
 * A {@link MetaDataQueryColumn} that binds to a document attribute or an arbitrary
 * expression to project a scalar value in the list-view result set.
 *
 * <p>When {@link #isProjected()} is {@code false} the column is still declared in the
 * metadata (e.g. for filter or sort support) but is not included in the SQL SELECT
 * clause. This allows invisible filter anchors and pre-sorted columns without widening
 * the result set.
 *
 * <p>The interaction flags {@link #isSortable()}, {@link #isFilterable()}, and
 * {@link #isEditable()} gate the list-view capabilities available to the user. Editable
 * columns support inline editing directly within the list grid, subject to
 * document-level permission checks.
 *
 * @see MetaDataQueryColumn
 * @see MetaDataQueryContentColumn
 */
public interface MetaDataQueryProjectedColumn extends MetaDataQueryColumn, FormattedText {
	/**
	 * Returns an optional SQL or BizQL expression that computes the column value.
	 *
	 * <p>When non-{@code null} the expression overrides the binding-based projection.
	 * The expression is embedded directly in the generated SELECT clause.
	 *
	 * @return the projection expression, or {@code null} if the column uses its binding
	 */
	public String getExpression();

	/**
	 * Returns whether this column is included in the SELECT projection.
	 *
	 * <p>A {@code false} value means the column is defined for metadata purposes
	 * (e.g. to support a default filter) but does not appear in the result set.
	 *
	 * @return {@code true} if the column value is selected; {@code false} if omitted
	 */
	public boolean isProjected();

	/**
	 * Returns whether the user can sort the list by this column.
	 *
	 * @return {@code true} if interactive column sorting is enabled
	 */
	public boolean isSortable();

	/**
	 * Returns whether the user can filter the list by this column.
	 *
	 * @return {@code true} if interactive column filtering is enabled
	 */
	public boolean isFilterable();

	/**
	 * Returns whether the user can edit cell values for this column inline in the list view.
	 *
	 * <p>Inline editing is subject to document-level write permissions and bizlet
	 * validation; this flag is a UI enablement hint only.
	 *
	 * @return {@code true} if inline editing is enabled for this column
	 */
	public boolean isEditable();
}
