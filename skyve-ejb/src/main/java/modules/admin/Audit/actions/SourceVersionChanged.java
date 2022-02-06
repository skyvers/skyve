package modules.admin.Audit.actions;

import java.util.List;

import org.skyve.CORE;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.web.WebContext;

import modules.admin.Audit.AuditBizlet;
import modules.admin.domain.Audit;
import modules.admin.domain.Audit.Operation;

public class SourceVersionChanged implements ServerSideAction<Audit> {
	@Override
	public ServerSideActionResult<Audit> execute(Audit bean, WebContext webContext) throws Exception {
		sourceVersionChanged(bean);
		return new ServerSideActionResult<>(bean);
	}
	
	public static void sourceVersionChanged(Audit bean) throws Exception {
		Audit source = bean.getSourceVersion();
		bean.setMe(bean);
		if (Operation.update.equals(source.getOperation())) {
			List<DomainValue> lesserVersions = AuditBizlet.getVersions(source, true);
			if (lesserVersions.isEmpty()) {
				bean.setComparisonVersion(null);
			}
			else {
				Audit comparison = CORE.getPersistence().retrieve(Audit.MODULE_NAME, 
																	Audit.DOCUMENT_NAME, 
																	lesserVersions.get(0).getCode());
				bean.setComparisonVersion(comparison);
			}
		}
		else {
			bean.setComparisonVersion(null);
		}
	}
}
