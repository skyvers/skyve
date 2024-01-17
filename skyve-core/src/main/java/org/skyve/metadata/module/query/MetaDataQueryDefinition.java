package org.skyve.metadata.module.query;

import java.util.List;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.module.Module;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.DocumentQuery.AggregateFunction;

/**
 * 
 */
public interface MetaDataQueryDefinition extends QueryDefinition {
	/**
	 * 
	 * @return
	 */
	public String getDocumentName();
	
	/**
	 * Overrides the default skyve behaviour.
	 * If true, the query will load the driving document's bean to enable polymorphic method calls.
	 * @return
	 */
	public Boolean getPolymorphic();

	/**
	 * If true, the query will omit projections for the in-built biz... columns enabling 
	 * a summary result set with grouping and query aggregate functions to be constructed for the rows.
	 * The list grid implementations should respond by disabling all zooming, summary rows, filtering etc.
	 * @return	true if agregate, otherwise false.
	 */
	public boolean isAggregate();

	/**
	 * 
	 * @param customer
	 * @return
	 */
	public Module getDocumentModule(@Nonnull Customer customer);
	
	/**
	 * 
	 * @return
	 */
	public String getFromClause();
	
	/**
	 * 
	 * @return
	 */
	public String getFilterClause();
	
	/**
	 * 
	 * @return
	 */
	public List<MetaDataQueryColumn> getColumns();
	
	/**
	 * 
	 * @param summaryType
	 * @param tagId
	 * @return
	 */
	public DocumentQuery constructDocumentQuery(@Nullable AggregateFunction summaryType, @Nullable String tagId);
}
