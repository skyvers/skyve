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
	@SuppressWarnings("java:S6813") // allow member injection
	private transient SnapshotService snapshotService;

	/**
	 * Returns available modules for snapshot configuration.
	 *
	 * @param attributeName the attribute requesting values
	 * @return module domain values when requested, otherwise superclass values
	 * @throws Exception if domain lookup fails
	 */
	@Override
	public List<DomainValue> getVariantDomainValues(String attributeName) throws Exception {
		if (Snapshots.moduleNamePropertyName.equals(attributeName)) {
			return snapshotService.getModuleDomainValues();
		}
		return super.getVariantDomainValues(attributeName);
	}

	/**
	 * Returns queries for the selected module.
	 *
	 * @param attributeName the attribute requesting values
	 * @param bean the current snapshots bean
	 * @return query values for the selected module
	 * @throws Exception if dynamic lookup fails
	 */
	@Override
	public List<DomainValue> getDynamicDomainValues(String attributeName, Snapshots bean) throws Exception {
		if (Snapshots.queryNamePropertyName.equals(attributeName)) {
			return snapshotService.getQueryDomainValues(bean.getModuleName());
		}
		return super.getDynamicDomainValues(attributeName, bean);
	}

	/**
	 * Updates dependent fields when the module or query changes.
	 *
	 * @param source the triggering binding
	 * @param bean the current snapshots bean
	 * @param webContext the current web context
	 * @throws Exception if rerender preparation fails
	 */
	@Override
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
