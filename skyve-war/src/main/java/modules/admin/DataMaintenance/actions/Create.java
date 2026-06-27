package modules.admin.DataMaintenance.actions;

import org.skyve.impl.backup.DDL;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;

import modules.admin.domain.DataMaintenance;

/**
 * Creates selected maintenance structures required for runtime data support.
 */
public class Create implements ServerSideAction<DataMaintenance> {
	/**
	 * Performs the execute operation.
	 * @param bean the bean value
	 * @param webContext the webContext value
	 * @return the operation result
	 * @throws Exception if the operation fails
	 */
	@Override
	public ServerSideActionResult<DataMaintenance> execute(DataMaintenance bean, WebContext webContext)
	throws Exception {
		StringBuilder result = new StringBuilder(2048);
		for (String create : DDL.create(null, false)) {
			result.append(create).append(';').append("<br/>");
		}
		
		bean.setDdlScript(result.toString());
		return new ServerSideActionResult<>(bean);
	}
}
