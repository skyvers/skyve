package org.skyve.metadata.module.query;

import java.util.List;

import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.module.Module;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.DocumentQuery.AggregateFunction;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * A document-driven query definition that backs list views, calendar views, map views,
 * and tree views in Skyve.
 *
 * <p>A {@code MetaDataQueryDefinition} selects instances of a target document and exposes
 * a set of column projections ({@link MetaDataQueryColumn}) that control which attributes
 * appear in the rendered list. The query is assembled at runtime into a
 * {@link DocumentQuery} via {@link #constructDocumentQuery} using optional SQL-fragment
 * overlays ({@code FROM}, {@code WHERE}, {@code GROUP BY}, {@code ORDER BY}).
 *
 * <p>The driving document is resolved lazily per customer through
 * {@link #getDocumentModule} so that customer-specific document overrides are honoured.
 *
 * <p>Most list views in a Skyve application are backed by a named
 * {@code MetaDataQueryDefinition} declared in module XML. Anonymous (generated) queries
 * are created by the framework when no explicit query is specified.
 *
 * @see QueryDefinition
 * @see MetaDataQueryColumn
 * @see org.skyve.persistence.DocumentQuery
 */
public interface MetaDataQueryDefinition extends QueryDefinition {
	/**
	 * Returns the name of the driving document for this query.
	 *
	 * <p>The document must be accessible in the customer context. To resolve the
	 * owning module use {@link #getDocumentModule}.
	 *
	 * @return the driving document name; never {@code null}
	 */
	@Nonnull String getDocumentName();
	
	/**
	 * Returns the polymorphic loading override for this query.
	 *
	 * <p>When {@code true}, each result row causes the framework to load the full
	 * domain bean so that polymorphic method calls (e.g. bizlet overrides) execute
	 * correctly. When {@code false} or {@code null}, the default Skyve behaviour
	 * applies (polymorphism is resolved through document hierarchy metadata without
	 * fetching the full bean).
	 *
	 * @return {@code Boolean.TRUE} to force full bean loading; {@code Boolean.FALSE}
	 *         to suppress it; {@code null} to use the Skyve default
	 */
	@Nullable Boolean getPolymorphic();

	/**
	 * Returns whether this is an aggregate (summary) query.
	 *
	 * <p>When {@code true} the query omits the built-in {@code biz...} system columns
	 * from the SELECT clause, which allows SQL {@code GROUP BY} and aggregate functions
	 * (SUM, COUNT, etc.) to be applied via {@link #getGroupClause()}. List-grid
	 * implementations must respond by disabling zooming, per-row actions, summary rows,
	 * inline filtering, and other features that depend on individual row identity.
	 *
	 * @return {@code true} if this is an aggregate query; {@code false} otherwise
	 */
	boolean isAggregate();

	/**
	 * Returns the module that owns the driving document, resolved for the given customer.
	 *
	 * <p>The owning module may differ from {@link #getOwningModule()} when the document
	 * is defined in one module but the query belongs to another (cross-module query).
	 * Customer-specific document overrides are taken into account.
	 *
	 * @param customer  the customer context for module resolution; must not be {@code null}
	 * @return the module that owns the driving document; never {@code null}
	 */
	@Nonnull Module getDocumentModule(@Nonnull Customer customer);
	
	/**
	 * Returns an optional SQL {@code FROM} clause fragment that supplements the default
	 * entity join produced by the ORM layer.
	 *
	 * <p>The fragment is appended after the auto-generated {@code FROM} source, allowing
	 * additional tables or subqueries to be joined.
	 *
	 * @return an additional {@code FROM} clause fragment, or {@code null} if none
	 */
	@Nullable String getFromClause();
	
	/**
	 * Returns an optional SQL {@code WHERE} clause fragment that is ANDed with any
	 * runtime filter conditions applied by the list view or API caller.
	 *
	 * @return an additional filter clause fragment, or {@code null} if none
	 */
	@Nullable String getFilterClause();
	
	/**
	 * Returns an optional SQL {@code GROUP BY} clause fragment.
	 *
	 * <p>A non-{@code null} value is most meaningful when {@link #isAggregate()} is
	 * {@code true}.
	 *
	 * @return a {@code GROUP BY} clause fragment, or {@code null} if none
	 */
	@Nullable String getGroupClause();
	
	/**
	 * Returns an optional SQL {@code ORDER BY} clause fragment that defines the default
	 * sort order for the query result.
	 *
	 * <p>The framework may override or extend this ordering when the user applies
	 * interactive column sorting in the list view.
	 *
	 * @return an {@code ORDER BY} clause fragment, or {@code null} to use the default
	 */
	@Nullable String getOrderClause();

	/**
	 * Returns the ordered list of column definitions for this query.
	 *
	 * <p>The list reflects the declaration order from module metadata and determines
	 * the left-to-right column order in list views. Columns may be hidden
	 * ({@link MetaDataQueryColumn#isHidden()}) but still participate in filtering or
	 * sorting.
	 *
	 * @return an ordered list of columns; never {@code null}
	 */
	@Nonnull List<MetaDataQueryColumn> getColumns();
	
	/**
	 * Constructs the executable {@link DocumentQuery} for this definition.
	 *
	 * <p>The returned query incorporates all declared columns, SQL clause overlays,
	 * and optionally a summary aggregate function and a tag filter. Callers may further
	 * refine the query (e.g. add additional filter criteria) before executing it.
	 *
	 * @param summaryType  an aggregate function to apply to numeric columns for summary
	 *                     rows, or {@code null} for a normal data query
	 * @param tagId        a tag identifier to restrict results to tagged instances,
	 *                     or {@code null} for no tag filter
	 * @return the constructed document query; never {@code null}
	 */
	@Nonnull DocumentQuery constructDocumentQuery(@Nullable AggregateFunction summaryType, @Nullable String tagId);
}
