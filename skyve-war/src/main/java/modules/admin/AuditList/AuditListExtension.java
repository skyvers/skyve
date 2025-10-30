package modules.admin.AuditList;

import jakarta.inject.Inject;
import modules.admin.Audit.AuditService;
import modules.admin.domain.AuditList;

public class AuditListExtension extends AuditList {
	private static final long serialVersionUID = -2480022026425411282L;

	@Inject
	private transient AuditService auditService;

	@Override
	public boolean isShowArchived() {

		return auditService.auditDocConfig().isPresent();
	}
}
