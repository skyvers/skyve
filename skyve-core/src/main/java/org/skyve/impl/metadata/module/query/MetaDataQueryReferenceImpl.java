package org.skyve.impl.metadata.module.query;

import java.util.List;

import org.skyve.CORE;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.MetaDataQueryColumn;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.DocumentQuery.AggregateFunction;

public class MetaDataQueryReferenceImpl extends QueryReferenceImpl implements MetaDataQueryDefinition {
	private static final long serialVersionUID = 7292447476649591811L;

	public MetaDataQueryReferenceImpl(String name, String moduleRef, String ref) {
		super(name, moduleRef, ref);
	}

	@Override
	public String getDocumentName() {
		return getTarget().getDocumentName();
	}

	@Override
	public Boolean getPolymorphic() {
		return getTarget().getPolymorphic();
	}

	@Override
	public boolean isAggregate() {
		return getTarget().isAggregate();
	}

	@Override
	public Module getDocumentModule(Customer customer) {
		return getTarget().getDocumentModule(customer);
	}

	@Override
	public String getFromClause() {
		return getTarget().getFromClause();
	}

	@Override
	public String getFilterClause() {
		return getTarget().getFilterClause();
	}

	@Override
	public String getGroupClause() {
		return getTarget().getGroupClause();
	}

	@Override
	public String getOrderClause() {
		return getTarget().getOrderClause();
	}

	@Override
	public List<MetaDataQueryColumn> getColumns() {
		return getTarget().getColumns();
	}

	@Override
	public DocumentQuery constructDocumentQuery(AggregateFunction summaryType, String tagId) {
		return getTarget().constructDocumentQuery(summaryType, tagId);
	}

	@Override
	@SuppressWarnings("unchecked")
	protected MetaDataQueryDefinition getTarget() {
		MetaDataQueryDefinition result = CORE.getCustomer().getModule(moduleRef).getMetaDataQuery(ref);
		if (result == null) {
			throw new MetaDataException("Imported query " + moduleRef + '.' + ref + " in module " + 
											getOwningModule().getName() + " is not defined.");
		}
		return result;
	}
}
