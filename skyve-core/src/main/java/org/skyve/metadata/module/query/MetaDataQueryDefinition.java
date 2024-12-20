package org.skyve.metadata.module.query;

import java.util.List;

import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.module.Module;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.DocumentQuery.AggregateFunction;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * 
 */
public interface MetaDataQueryDefinition extends QueryDefinition {
	/**
	 * 
	 * @return
	 */
	@Nonnull String getDocumentName();
	
	/**
	 * Overrides the default skyve behaviour.
	 * If true, the query will load the driving document's bean to enable polymorphic method calls.
	 * @return
	 */
	@Nullable Boolean getPolymorphic();

	/**
	 * If true, the query will omit projections for the in-built biz... columns enabling 
	 * a summary result set with grouping and query aggregate functions to be constructed for the rows.
	 * The list grid implementations should respond by disabling all zooming, summary rows, filtering etc.
	 * @return	true if agregate, otherwise false.
	 */
	boolean isAggregate();

	/**
	 * 
	 * @param customer
	 * @return
	 */
	@Nonnull Module getDocumentModule(@Nonnull Customer customer);
	
	/**
	 * 
	 * @return
	 */
	@Nullable String getFromClause();
	
	/**
	 * 
	 * @return
	 */
	@Nullable String getFilterClause();
	
	/**
	 * 
	 * @return
	 */
	@Nullable String getGroupClause();
	
	/**
	 * 
	 * @return
	 */
	@Nullable String getOrderClause();

	/**
	 * 
	 * @return
	 */
	@Nonnull List<MetaDataQueryColumn> getColumns();
	
	/**
	 * 
	 * @param summaryType
	 * @param tagId
	 * @return
	 */
	@Nonnull DocumentQuery constructDocumentQuery(@Nullable AggregateFunction summaryType, @Nullable String tagId);
}
