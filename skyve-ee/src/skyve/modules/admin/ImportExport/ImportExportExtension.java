package modules.admin.ImportExport;

import modules.admin.domain.ImportExport;
import modules.admin.domain.ImportExportColumn;

public class ImportExportExtension extends ImportExport{

	/**
	 * 
	 */
	private static final long serialVersionUID = -4914725314222700513L;
	
	public Boolean anyColumnHasExpression() {
		for(ImportExportColumn c: this.getImportExportColumns()) {
			if(c.isShowExpression()) {
				return Boolean.TRUE;
			}
		}
		return Boolean.FALSE;
	}
}
