package modules.test.AllAttributesDynamicPersistent;

import org.skyve.domain.DynamicPersistentBean;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;

/**
 * Implements a no-op server-side action used by dynamic-persistent action tests.
 */
public class TestServerSideAction implements org.skyve.metadata.controller.ServerSideAction<DynamicPersistentBean> {
	/**
	 * Returns the supplied bean unchanged.
	 *
	 * @param bean the bean supplied by the framework
	 * @param webContext the active web context
	 * @return a successful action result wrapping the same bean instance
	 * @throws Exception if action execution fails
	 */
	@Override
	public ServerSideActionResult<DynamicPersistentBean> execute(DynamicPersistentBean bean, WebContext webContext)
	throws Exception {
		return new ServerSideActionResult<>(bean);
	}
}
