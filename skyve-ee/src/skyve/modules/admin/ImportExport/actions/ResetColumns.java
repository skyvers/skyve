package modules.admin.ImportExport.actions;

import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;

import modules.admin.ImportExport.ImportExportBizlet;
import modules.admin.domain.ImportExport;

public class ResetColumns implements ServerSideAction<ImportExport> {

	/**
	 * 
	 */
	private static final long serialVersionUID = -3906500305260248349L;

	@Override
	public ServerSideActionResult<ImportExport> execute(ImportExport bean, WebContext webContext)
			throws Exception {

		ImportExportBizlet.updateColumns(ImportExport.documentNamePropertyName, bean);

		return new ServerSideActionResult<>(bean);
	}
}
