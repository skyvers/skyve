package modules.admin.ImportExport.actions;

import org.skyve.metadata.controller.Download;
import org.skyve.metadata.controller.DownloadAction;
import org.skyve.web.WebContext;

import modules.admin.ImportExport.ImportExportBizlet;
import modules.admin.ImportExport.ImportExportExtension;

public class GetImportTemplate extends DownloadAction<ImportExportExtension> {
	@Override
	public void prepare(ImportExportExtension bean, WebContext webContext) throws Exception {
		// Nothing to see here
	}

	@Override
	public Download download(ImportExportExtension bean, WebContext webContext)
			throws Exception {
		return RunExport.generateDownload(bean, ImportExportBizlet.generateColumns(bean), Boolean.TRUE, Boolean.TRUE);
	}
}
