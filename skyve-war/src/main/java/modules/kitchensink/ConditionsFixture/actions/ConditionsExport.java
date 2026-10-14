package modules.kitchensink.ConditionsFixture.actions;

import org.skyve.EXT;
import org.skyve.bizport.BizPortWorkbook;
import org.skyve.metadata.controller.BizExportAction;
import org.skyve.web.WebContext;

/**
 * Exports an empty workbook, which is all the conditions fixture needs to put a condition on an export action.
 */
public class ConditionsExport extends BizExportAction {
	@Override
	public BizPortWorkbook bizExport(WebContext webContext) throws Exception {
		BizPortWorkbook result = EXT.newBizPortWorkbook(false);
		result.materialise();
		return result;
	}
}
