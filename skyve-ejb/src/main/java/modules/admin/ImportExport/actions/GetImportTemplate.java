package modules.admin.ImportExport.actions;

import org.skyve.metadata.controller.Download;
import org.skyve.metadata.controller.DownloadAction;
import org.skyve.web.WebContext;

import modules.admin.ImportExport.ImportExportBizlet;
import modules.admin.ImportExport.ImportExportExtension;

public class GetImportTemplate extends DownloadAction<ImportExportExtension> {
	private static final long serialVersionUID = -2929289166233711854L;

	@Override
	public void prepare(ImportExportExtension bean, WebContext webContext) throws Exception {
		// TODO Auto-generated method stub

	}

	@Override
	public Download download(ImportExportExtension bean, WebContext webContext)
			throws Exception {
		return RunExport.generateDownload(bean, ImportExportBizlet.generateColumns(bean), Boolean.TRUE, Boolean.TRUE);
	}
}
