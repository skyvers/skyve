package modules.admin.Snapshots;

import java.util.List;

import org.skyve.CORE;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.persistence.DocumentFilter;
import org.skyve.persistence.DocumentQuery;
import org.skyve.web.WebContext;

import modules.admin.Snapshot.SnapshotBizlet;
import modules.admin.domain.Snapshot;
import modules.admin.domain.Snapshots;

public class SnapshotsBizlet extends Bizlet<Snapshots> {
	@Override
	public List<DomainValue> getVariantDomainValues(String attributeName) throws Exception {
		if (Snapshots.moduleNamePropertyName.equals(attributeName)) {
			return SnapshotBizlet.getModuleDomainValues();
		}
		return super.getVariantDomainValues(attributeName);
	}

	@Override
	public List<DomainValue> getDynamicDomainValues(String attributeName, Snapshots bean) throws Exception {
		if (Snapshots.queryNamePropertyName.equals(attributeName)) {
			return SnapshotBizlet.getQueryDomainValues(bean.getModuleName());
		}
		return super.getDynamicDomainValues(attributeName, bean);
	}
	
	@Override
	public void preRerender(String source, Snapshots bean, WebContext webContext) throws Exception {
		if (Snapshots.moduleNamePropertyName.equals(source)) {
			bean.setQueryName(null);
		}
		else if (Snapshots.queryNamePropertyName.equals(source)) {
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
