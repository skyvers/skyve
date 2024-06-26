package modules.admin.Audit.actions;

import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;

import jakarta.inject.Inject;
import modules.admin.Audit.AuditBizlet;
import modules.admin.domain.Audit;

public class SourceVersionChanged implements ServerSideAction<Audit> {

    @Inject
    private AuditBizlet auditBizlet;

	@Override
	public ServerSideActionResult<Audit> execute(Audit bean, WebContext webContext) throws Exception {
	    auditBizlet.sourceVersionChanged(bean);
		return new ServerSideActionResult<>(bean);
	}


}
