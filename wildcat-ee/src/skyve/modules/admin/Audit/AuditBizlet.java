package modules.admin.Audit;

import java.util.ArrayList;
import java.util.List;

import modules.admin.Audit.actions.SourceVersionChanged;
import modules.admin.domain.Audit;
import modules.admin.domain.Audit.Operation;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;
import org.skyve.metadata.SortDirection;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.persistence.DocumentFilter;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;
import org.skyve.util.Binder;
import org.skyve.web.WebContext;

public class AuditBizlet extends Bizlet<Audit> {
	private static final long serialVersionUID = -1421062280134070819L;

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
			return getVersions(bean, false);
		}
		else if (Audit.comparisonVersionPropertyName.equals(attributeName) && 
					(bean.getSourceVersion() != null)) {
			return getVersions(bean.getSourceVersion(), true);
		}
		
		return null;
	}
	
	public static List<DomainValue> getVersions(Audit audit, boolean lessThanVersions)
	throws Exception {
		Persistence p = CORE.getPersistence();
		
		DocumentQuery q = p.newDocumentQuery(Audit.MODULE_NAME, Audit.DOCUMENT_NAME);
		q.addBoundProjection(Bean.DOCUMENT_ID);
		q.addBoundProjection(Bean.BIZ_KEY);
		
		DocumentFilter f = q.getFilter();
		f.addEquals(Audit.auditModuleNamePropertyName, audit.getAuditModuleName());
		f.addEquals(Audit.auditDocumentNamePropertyName, audit.getAuditDocumentName());
		f.addEquals(Audit.auditBizIdPropertyName, audit.getAuditBizId());
		if (lessThanVersions) {
			f.addLessThan(Audit.auditBizVersionPropertyName, audit.getAuditBizVersion());
		}
		q.addOrdering(PersistentBean.VERSION_NAME, SortDirection.descending);
		
		List<Bean> versions = p.retrieve(q, Integer.valueOf(0), Integer.valueOf(100));
		List<DomainValue> result = new ArrayList<>(versions.size());
		for (Bean version : versions) {
			result.add(new DomainValue((String) Binder.get(version, Bean.DOCUMENT_ID),
										(String) Binder.get(version, Bean.BIZ_KEY)));
		}

		return result;
	}
}
