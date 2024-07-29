package modules.admin.DataMaintenance.actions;

import org.skyve.EXT;
import org.skyve.bizport.BizPortWorkbook;
import org.skyve.domain.messages.UploadException;
import org.skyve.metadata.controller.BizImportAction;

public class DataMaintenanceImportAction extends BizImportAction {
	@Override
	public void bizImport(BizPortWorkbook workbook, UploadException problems) throws Exception {
		EXT.standardBizImport(workbook, problems);
	}
}
