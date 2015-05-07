package modules.admin.Audit;

import java.util.ArrayList;
import java.util.List;

import modules.admin.domain.Audit;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.persistence.DocumentFilter;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;
import org.skyve.util.Binder;

public class AuditBizlet extends Bizlet<Audit> {
	private static final long serialVersionUID = -1421062280134070819L;

	@Override
	public void postLoad(Audit bean) throws Exception {
		bean.setMe(bean);
	}

	@Override
	public List<DomainValue> getDynamicDomainValues(String attributeName, Audit bean)
	throws Exception {
		if (Audit.comparisonVersionPropertyName.equals(attributeName)) {
			Persistence p = CORE.getPersistence();
			Customer c = p.getUser().getCustomer();
			
			DocumentQuery q = p.newDocumentQuery(Audit.MODULE_NAME, Audit.DOCUMENT_NAME);
			q.addBoundProjection(Bean.DOCUMENT_ID);
			q.addBoundProjection(Bean.BIZ_KEY);
			DocumentFilter f = q.getFilter();
			f.addEquals(Audit.auditModuleNamePropertyName, bean.getAuditModuleName());
			f.addEquals(Audit.auditDocumentNamePropertyName, bean.getAuditDocumentName());
			f.addEquals(Audit.auditBizIdPropertyName, bean.getAuditBizId());
			q.addOrdering(PersistentBean.VERSION_NAME);
			
			List<Bean> versions = p.retrieve(q);
			List<DomainValue> result = new ArrayList<>(versions.size());
			for (Bean version : versions) {
				result.add(new DomainValue((String) Binder.get(version, Bean.DOCUMENT_ID),
											(String) Binder.get(version, Bean.BIZ_KEY)));
			}
			result.add(new DomainValue("C", "Current"));
			return result;
		}
		
		return null;
	}
}
