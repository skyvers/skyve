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

public class SnapshotBizlet extends Bizlet<Snapshot> {
	@Inject
	private transient SnapshotService snapshotService;

	/**
	 * Max + 1 the snapshot ordinal to place a new snapshot at the bottom of the list.
	 * No data store locking required here as the uniqueness of the number derived is not critical.
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
	 */
	@Override
	public List<DomainValue> getDynamicDomainValues(String attributeName, Snapshot bean) throws Exception {
		if (Snapshot.queryNamePropertyName.equals(attributeName)) {
			return snapshotService.getQueryDomainValues(bean.getModuleName());
		}
		return super.getDynamicDomainValues(attributeName, bean);
	}

	/**
	 * Null out queryName on moduleName change
	 */
	@Override
	public void preRerender(String source, Snapshot bean, WebContext webContext) throws Exception {
		if (Snapshot.moduleNamePropertyName.equals(source)) {
			bean.setQueryName(null);
		}
	}
}
