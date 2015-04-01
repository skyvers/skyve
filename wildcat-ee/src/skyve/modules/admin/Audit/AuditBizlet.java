package modules.admin.Audit;

import modules.admin.domain.Audit;

import org.skyve.metadata.model.document.Bizlet;

public class AuditBizlet extends Bizlet<Audit> {
	private static final long serialVersionUID = -1421062280134070819L;

	@Override
	public void postLoad(Audit bean) throws Exception {
		bean.setCurrent(bean);
	}
}
