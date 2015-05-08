package modules.admin.DataMaintenance.actions;

import org.skyve.app.ModulesUtil;
import org.skyve.bizport.BizPortException;
import org.skyve.bizport.BizPortWorkbook;
import org.skyve.metadata.controller.BizImportAction;

public class DataMaintenanceImportAction extends BizImportAction {
	private static final long serialVersionUID = -2428009836833577948L;

	@Override
	public void bizImport(BizPortWorkbook workbook, BizPortException problems) throws Exception {
		ModulesUtil.standardBeanBizImport(workbook, problems);
	}
}
