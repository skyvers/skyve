package modules.admin.DataMaintenance.actions;

import org.skyve.bizport.BizPortWorkbook;
import org.skyve.domain.messages.UploadException;
import org.skyve.metadata.controller.BizImportAction;

import modules.admin.ModulesUtil;

public class DataMaintenanceImportAction extends BizImportAction {
	@Override
	public void bizImport(BizPortWorkbook workbook, UploadException problems) throws Exception {
		ModulesUtil.standardBeanBizImport(workbook, problems);
	}
}
