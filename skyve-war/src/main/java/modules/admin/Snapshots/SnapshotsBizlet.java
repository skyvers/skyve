package modules.admin.Snapshots;

import java.util.List;

import org.skyve.CORE;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.persistence.DocumentFilter;
import org.skyve.persistence.DocumentQuery;
import org.skyve.web.WebContext;

import jakarta.inject.Inject;
import modules.admin.Snapshot.SnapshotService;
import modules.admin.domain.Snapshot;
import modules.admin.domain.Snapshots;

/**
 * Provides domain values and UI refresh behaviour for the Snapshots document.
 */
public class SnapshotsBizlet extends Bizlet<Snapshots> {
	@Inject
	private transient SnapshotService snapshotService;

	@Override
	/**
	 * Returns available modules for snapshot configuration.
	 */
	public List<DomainValue> getVariantDomainValues(String attributeName) throws Exception {
		if (Snapshots.moduleNamePropertyName.equals(attributeName)) {
			return snapshotService.getModuleDomainValues();
		}
		return super.getVariantDomainValues(attributeName);
	}

	@Override
	/**
	 * Returns queries for the selected module.
	 */
	public List<DomainValue> getDynamicDomainValues(String attributeName, Snapshots bean) throws Exception {
		if (Snapshots.queryNamePropertyName.equals(attributeName)) {
			return snapshotService.getQueryDomainValues(bean.getModuleName());
		}
		return super.getDynamicDomainValues(attributeName, bean);
	}

	@Override
	/**
	 * Updates dependent fields when the module or query changes.
	 */
	public void preRerender(String source, Snapshots bean, WebContext webContext) throws Exception {
		if (Snapshots.moduleNamePropertyName.equals(source)) {
			bean.setQueryName(null);
		} else if (Snapshots.queryNamePropertyName.equals(source)) {
			List<Snapshot> snapshotsToReorder = bean.getSnapshotsToReorder();
			snapshotsToReorder.clear();
			DocumentQuery q = CORE.getPersistence().newDocumentQuery(Snapshot.MODULE_NAME, Snapshot.DOCUMENT_NAME);
			DocumentFilter f = q.getFilter();
			f.addEquals(Snapshot.moduleNamePropertyName, bean.getModuleName());
			f.addEquals(Snapshot.queryNamePropertyName, bean.getQueryName());
			snapshotsToReorder.addAll(q.beanResults());
		}
	}
}
