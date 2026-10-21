package modules.kitchensink.ActionsFixture.actions;

import org.skyve.EXT;
import org.skyve.bizport.BizPortWorkbook;
import org.skyve.metadata.controller.BizExportAction;
import org.skyve.web.WebContext;

/**
 * Exports an empty workbook, which is all the actions fixture needs to put a condition on an export action.
 */
public class ActionsExport extends BizExportAction {
	/**
	 * Returns an empty workbook.
	 *
	 * @param webContext	The current web context.
	 * @return	The workbook.
	 */
	@Override
	public BizPortWorkbook bizExport(WebContext webContext) {
		BizPortWorkbook result = EXT.newBizPortWorkbook(false);
		result.materialise();
		return result;
	}
}
