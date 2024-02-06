package modules.test.AllAttributesDynamicPersistent;

import org.skyve.domain.DynamicPersistentBean;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;

public class ServerSideAction implements org.skyve.metadata.controller.ServerSideAction<DynamicPersistentBean> {

	@Override
	public ServerSideActionResult<DynamicPersistentBean> execute(DynamicPersistentBean bean, WebContext webContext)
	throws Exception {
		return new ServerSideActionResult<>(bean);
	}
}
