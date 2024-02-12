package modules.test.DeleteDuringPostDelete;

import org.skyve.CORE;
import org.skyve.metadata.model.document.Bizlet;

import modules.test.domain.DeleteDuringPostDelete;

public class DeleteDuringPostDeleteBizlet extends Bizlet<DeleteDuringPostDelete> {
	@Override
	public void postDelete(DeleteDuringPostDelete bean) throws Exception {
		CORE.getPersistence().delete(bean.getAggregatedAssociation());
	}
}
