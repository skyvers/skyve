package modules.admin.DataMaintenance.actions;

import org.skyve.impl.backup.ContentChecker;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;

import modules.admin.domain.DataMaintenance;

/**
 * Validates content repository references and reports invalid content links.
 */
public class CheckContent implements ServerSideAction<DataMaintenance> {
	/**
	 * Performs the execute operation.
	 * @param bean the bean value
	 * @param webContext the webContext value
	 * @return the operation result
	 * @throws Exception if the operation fails
	 */
	@Override
	public ServerSideActionResult<DataMaintenance> execute(DataMaintenance bean, WebContext webContext) throws Exception {
		new ContentChecker().checkContent();
		
		return new ServerSideActionResult<>(bean);
	}
}
