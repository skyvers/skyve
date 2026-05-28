package modules.admin.Snapshot;

import java.util.List;

import org.skyve.CORE;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.persistence.DocumentFilter;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.DocumentQuery.AggregateFunction;
import org.skyve.persistence.Persistence;
import org.skyve.web.WebContext;

import jakarta.inject.Inject;
import modules.admin.domain.Snapshot;

/**
 * Implements Snapshot document lifecycle behavior and dynamic domain support.
 */
public class SnapshotBizlet extends Bizlet<Snapshot> {
	@Inject
	@SuppressWarnings("java:S6813") // allow member injection
	private transient SnapshotService snapshotService;

	/**
	 * Max + 1 the snapshot ordinal to place a new snapshot at the bottom of the list.
	 * No data store locking required here as the uniqueness of the number derived is not critical.
	 *
	 * @param bean the snapshot being persisted
	 * @throws Exception if ordinal lookup fails
	 */
	@Override
	public void preSave(Snapshot bean) throws Exception {
		if (bean.isNotPersisted()) {
			Integer ordinal = bean.getOrdinal();
			if (ordinal == null) {
				String moduleName = bean.getModuleName();
				String queryName = bean.getQueryName();
				if ((moduleName == null) || (queryName == null)) {
					bean.setOrdinal(Integer.valueOf(0));
				} else {
					Persistence p = CORE.getPersistence();
					DocumentQuery q = p.newDocumentQuery(Snapshot.MODULE_NAME, Snapshot.DOCUMENT_NAME);
					q.addAggregateProjection(AggregateFunction.Max, Snapshot.ordinalPropertyName, "maxOrdinal");
					DocumentFilter f = q.getFilter();
					f.addEquals(Snapshot.moduleNamePropertyName, moduleName);
					f.addEquals(Snapshot.queryNamePropertyName, queryName);
					Number maxOrdinal = q.scalarResult(Number.class);
					if (maxOrdinal == null) {
						bean.setOrdinal(Integer.valueOf(0));
					} else {
						bean.setOrdinal(Integer.valueOf(maxOrdinal.intValue() + 1));
					}
				}
			}
		}
	}

	/**
	 * Get variant domain values for the module name attribute.
	 *
	 * @param attributeName the attribute requesting variant domain values
	 * @return module domain values when requested, otherwise superclass values
	 * @throws Exception if domain-value retrieval fails
	 */
	@Override
	public List<DomainValue> getVariantDomainValues(String attributeName) throws Exception {
		if (Snapshot.moduleNamePropertyName.equals(attributeName)) {
			return snapshotService.getModuleDomainValues();
		}
		return super.getVariantDomainValues(attributeName);
	}

	/**
	 * Get dynamic domain values for the query name attribute based on the selected module.
	 *
	 * @param attributeName the attribute requesting dynamic values
	 * @param bean the current snapshot bean
	 * @return query domain values scoped to the selected module
	 * @throws Exception if dynamic-value retrieval fails
	 */
	@Override
	public List<DomainValue> getDynamicDomainValues(String attributeName, Snapshot bean) throws Exception {
		if (Snapshot.queryNamePropertyName.equals(attributeName)) {
			return snapshotService.getQueryDomainValues(bean.getModuleName());
		}
		return super.getDynamicDomainValues(attributeName, bean);
	}

	/**
	 * Clears query selection when the source module changes.
	 *
	 * @param source the triggering attribute binding
	 * @param bean the current snapshot bean
	 * @param webContext the current web context
	 * @throws Exception if rerender preparation fails
	 */
	@Override
	public void preRerender(String source, Snapshot bean, WebContext webContext) throws Exception {
		if (Snapshot.moduleNamePropertyName.equals(source)) {
			bean.setQueryName(null);
		}
	}
}
