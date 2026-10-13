package org.skyve.impl.metadata.module.query;

import java.util.List;

import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.MetaDataQueryColumn;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.DocumentQuery.AggregateFunction;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * A query reference that points to an existing {@link MetaDataQueryDefinition}
 * by name.
 *
 * <p>Used in widget definitions to reuse a named metadata query from the same
 * module without duplicating its column declarations.  The named query is
 * resolved from the module at runtime.
 *
 * <p>Threading: not thread-safe.  Instances are populated during metadata loading
 * and are read-only once placed in the repository cache.
 *
 * @see QueryReferenceImpl
 * @see MetaDataQueryDefinition
 */
public class MetaDataQueryReferenceImpl extends QueryReferenceImpl implements MetaDataQueryDefinition {
	private static final long serialVersionUID = 7292447476649591811L;

	public MetaDataQueryReferenceImpl(@Nullable String name, @Nonnull String moduleRef, @Nonnull String ref) {
		super(name, moduleRef, ref);
	}

	@Override
	public @Nonnull String getDocumentName() {
		return getTarget().getDocumentName();
	}

	@Override
	public @Nullable Boolean getPolymorphic() {
		return getTarget().getPolymorphic();
	}

	@Override
	public boolean isAggregate() {
		return getTarget().isAggregate();
	}

	@Override
	public @Nonnull Module getDocumentModule(@Nonnull Customer customer) {
		return getTarget().getDocumentModule(customer);
	}

	@Override
	public @Nullable String getFromClause() {
		return getTarget().getFromClause();
	}

	@Override
	public @Nullable String getFilterClause() {
		return getTarget().getFilterClause();
	}

	@Override
	public @Nullable String getGroupClause() {
		return getTarget().getGroupClause();
	}

	@Override
	public @Nullable String getOrderClause() {
		return getTarget().getOrderClause();
	}

	@Override
	public @Nonnull List<MetaDataQueryColumn> getColumns() {
		return getTarget().getColumns();
	}

	@Override
	public @Nonnull DocumentQuery constructDocumentQuery(@Nullable AggregateFunction summaryType, @Nullable String tagId) {
		return getTarget().constructDocumentQuery(summaryType, tagId);
	}

	@Override
	@SuppressWarnings("unchecked")
	protected @Nonnull MetaDataQueryDefinition getTarget() {
		// TODO this should use the current customer (can't get customer during generate domain)
		MetaDataQueryDefinition result = ProvidedRepositoryFactory.get().getModule(null, moduleRef).getMetaDataQuery(ref);
		if (result == null) {
			throw new MetaDataException("Imported query " + moduleRef + '.' + ref + " in module " + 
											getOwningModule().getName() + " is not defined.");
		}
		return result;
	}
}
