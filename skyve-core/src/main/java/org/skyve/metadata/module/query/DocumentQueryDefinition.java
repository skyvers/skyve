package org.skyve.metadata.module.query;

import java.util.List;

import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.module.Module;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.DocumentQuery.AggregateFunction;

/**
 * 
 */
public interface DocumentQueryDefinition extends QueryDefinition {
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
	 * 
	 * @param customer
	 * @return
	 */
	public Module getDocumentModule(Customer customer);
	
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
	public List<QueryColumn> getColumns();
	
	/**
	 * 
	 * @param summaryType
	 * @param tagId
	 * @return
	 */
	public DocumentQuery constructDocumentQuery(AggregateFunction summaryType, String tagId);
}
