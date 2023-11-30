package modules.admin.Audit;

import java.util.ArrayList;
import java.util.List;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.metadata.SortDirection;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.persistence.DocumentFilter;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;
import org.skyve.util.Binder;
import org.skyve.web.WebContext;

import modules.admin.Audit.actions.SourceVersionChanged;
import modules.admin.domain.Audit;
import modules.admin.domain.Audit.Operation;

public class AuditBizlet extends Bizlet<Audit> {
	@Override
	public Audit preExecute(ImplicitActionName actionName,
								Audit bean,
								Bean parentBean,
								WebContext webContext)
	throws Exception {
		if (ImplicitActionName.Edit.equals(actionName)) {
			bean.setSourceVersion(bean);
			SourceVersionChanged.sourceVersionChanged(bean);
		}
		
		return bean;
	}

	@Override
	public List<DomainValue> getDynamicDomainValues(String attributeName, Audit bean)
	throws Exception {
		if (Audit.sourceVersionPropertyName.equals(attributeName)) {
			bean.originalValues().clear();
			return getVersions(bean, false);
		}
		else if (Audit.comparisonVersionPropertyName.equals(attributeName) && 
					(bean.getSourceVersion() != null)) {
			return getVersions(bean.getSourceVersion(), true);
		}
		
		return null;
	}
	
	public static List<DomainValue> getVersions(Audit audit, boolean forComparison)
	throws Exception {
		List<DomainValue> result = null;
		
		if (forComparison && (! Operation.update.equals(audit.getOperation()))) {
			result = new ArrayList<>();
		}
		else {
			Persistence p = CORE.getPersistence();
			
			DocumentQuery q = p.newDocumentQuery(Audit.MODULE_NAME, Audit.DOCUMENT_NAME);
			q.addBoundProjection(Bean.DOCUMENT_ID);
			q.addBoundProjection(Bean.BIZ_KEY);
			
			DocumentFilter f = q.getFilter();
			f.addEquals(Audit.auditBizIdPropertyName, audit.getAuditBizId());
			if (forComparison) {
				f.addLessThan(Audit.millisPropertyName, audit.getMillis());
			}
			q.addBoundOrdering(Audit.millisPropertyName, SortDirection.descending);
			
			q.setFirstResult(0).setMaxResults(100);
			List<Bean> versions = q.projectedResults();
			result = new ArrayList<>(versions.size());
			for (Bean version : versions) {
				result.add(new DomainValue((String) Binder.get(version, Bean.DOCUMENT_ID),
											(String) Binder.get(version, Bean.BIZ_KEY)));
			}
		}
		
		return result;
	}
}
