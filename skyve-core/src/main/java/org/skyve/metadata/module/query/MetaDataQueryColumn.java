package org.skyve.metadata.module.query;

import org.skyve.impl.metadata.view.HorizontalAlignment;
import org.skyve.metadata.DecoratedMetaData;
import org.skyve.metadata.FilterOperator;
import org.skyve.metadata.NamedMetaData;
import org.skyve.metadata.SortDirection;
import org.skyve.metadata.view.TextOutput.Sanitisation;
import org.skyve.util.Util;

/**
 * Declares a column within a {@link MetaDataQueryDefinition}.
 *
 * <p>A {@code MetaDataQueryColumn} describes how a single field or expression appears
 * in a list-view result set. It carries display metadata (display name, alignment,
 * pixel width, visibility) as well as list-view interaction metadata (default filter
 * operator and expression, default sort direction, HTML escape and sanitise settings).
 *
 * <p>Concrete subtypes add richer projection or content-display semantics:
 * <ul>
 *   <li>{@link MetaDataQueryProjectedColumn} — a bound or expression-based projected column</li>
 *   <li>{@link MetaDataQueryContentColumn} — a content/attachment thumbnail or link column</li>
 * </ul>
 *
 * @see MetaDataQueryDefinition
 * @see MetaDataQueryProjectedColumn
 * @see MetaDataQueryContentColumn
 */
public interface MetaDataQueryColumn extends NamedMetaData, DecoratedMetaData {
	/**
	 * Returns the column header label displayed in the list view.
	 *
	 * <p>For a localised version use {@link #getLocalisedDisplayName()}.
	 *
	 * @return the display name; may be {@code null} if the column inherits its label
	 *         from the document attribute
	 */
	public String getDisplayName();
	
	/**
	 * Returns the localised display name for the current user locale.
	 *
	 * <p>Delegates to {@link Util#i18n(String)} using {@link #getDisplayName()} as
	 * the resource key.
	 *
	 * @return a non-{@code null} localised display name
	 */
	public default String getLocalisedDisplayName() {
		return Util.i18n(getDisplayName());
	}
	
	/**
	 * Returns the document attribute binding path for this column.
	 *
	 * <p>The binding is a dot-separated expression resolved against the driving document,
	 * e.g. {@code "contact.name"}. For expression columns see
	 * {@link MetaDataQueryProjectedColumn#getExpression()}.
	 *
	 * @return the binding path; may be {@code null} for expression-only columns
	 */
	public String getBinding();
	
	/**
	 * Returns the default filter operator applied to this column in list views.
	 *
	 * <p>The operator determines how the user-supplied filter value is compared against
	 * the column value (e.g. contains, equals, starts with).
	 *
	 * @return the default filter operator, or {@code null} to use the type-based default
	 */
	public FilterOperator getFilterOperator();
	
	/**
	 * Returns the default filter expression pre-populated in the column filter field.
	 *
	 * <p>When non-{@code null} this expression is applied automatically when the
	 * list view first loads, before any user interaction.
	 *
	 * @return the default filter expression, or {@code null} if none
	 */
	public String getFilterExpression();
	
	/**
	 * Returns the default sort direction for this column.
	 *
	 * <p>When non-{@code null} the list view initially sorts by this column in the
	 * given direction.
	 *
	 * @return the default sort direction, or {@code null} for no default sort
	 */
	public SortDirection getSortOrder();
	
	/**
	 * Returns whether this column is hidden in the list view.
	 *
	 * <p>Hidden columns do not appear in the rendered grid but may still participate
	 * in filtering or sorting operations.
	 *
	 * @return {@code true} if the column is hidden; {@code false} if it is visible
	 */
	public boolean isHidden();
	
	/**
	 * Returns the explicit pixel width for this column, or {@code null} to use
	 * the list-view default.
	 *
	 * @return the column width in pixels, or {@code null}
	 */
	public Integer getPixelWidth();
	
	/**
	 * Returns the horizontal alignment for cell content in this column.
	 *
	 * @return the alignment; may be {@code null} to use the type-based default
	 */
	public HorizontalAlignment getAlignment();
	
	/**
	 * Returns whether HTML special characters in cell values should be escaped.
	 *
	 * <p>When {@code true}, characters such as {@code <} and {@code >} are converted
	 * to HTML entities before rendering, preventing XSS via stored content.
	 *
	 * @return {@code true} if HTML escaping is enabled
	 */
	public boolean isEscape();

	/**
	 * Returns the sanitisation mode applied to cell values before rendering.
	 *
	 * <p>Sanitisation strips or neutralises potentially unsafe HTML/CSS from rich-text
	 * values. The mode determines which elements and attributes are permitted.
	 *
	 * @return the sanitisation mode; may be {@code null} for no sanitisation
	 */
	public Sanitisation getSanitise();
}
