package modules.admin.DataMaintenance.actions;

import modules.ModulesUtil;

import org.skyve.bizport.BizPortWorkbook;
import org.skyve.domain.messages.UploadException;
import org.skyve.metadata.controller.BizImportAction;

public class DataMaintenanceImportAction extends BizImportAction {
	private static final long serialVersionUID = -2428009836833577948L;

	@Override
	public void bizImport(BizPortWorkbook workbook, UploadException problems) throws Exception {
		ModulesUtil.standardBeanBizImport(workbook, problems);
	}
}
