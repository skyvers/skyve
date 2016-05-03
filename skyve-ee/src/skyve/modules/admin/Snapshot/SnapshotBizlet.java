package modules.admin.Snapshot;

import modules.admin.domain.Snapshot;

import org.skyve.domain.Bean;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.web.WebContext;

public class SnapshotBizlet extends Bizlet<Snapshot> {

	/**
	 * 
	 */
	private static final long serialVersionUID = -6729897461641136277L;

	@Override
	public Snapshot preExecute(ImplicitActionName actionName, Snapshot bean, Bean parentBean, WebContext webContext) throws Exception {

		if(ImplicitActionName.Edit.equals(actionName)){
			bean.setCopyToUserSnapshotName(bean.getName());
		}
		
		return super.preExecute(actionName, bean, parentBean, webContext);
	}

}