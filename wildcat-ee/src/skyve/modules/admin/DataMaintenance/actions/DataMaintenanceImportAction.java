package modules.admin.DataMaintenance.actions;

import org.skyve.bizport.BizPortException;
import org.skyve.bizport.BizPortWorkbook;
import org.skyve.metadata.controller.BizImportAction;

import modules.ModulesUtil;
import modules.admin.AdminUtil;
import modules.admin.domain.DataMaintenance;

public class DataMaintenanceImportAction extends BizImportAction {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = -2428009836833577948L;

	@Override
	public void bizImport(BizPortWorkbook workbook, BizPortException problems) throws Exception {
		DataMaintenance d = AdminUtil.getDataMaintenance();
		String[] refs = d.getModDocName().split("\\.");
		ModulesUtil.standardBeanBizImport(workbook, problems, refs[0]);
	}
}
