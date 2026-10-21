package modules.kitchensink.ActionsFixtureRow.actions;

import org.skyve.EXT;
import org.skyve.bizport.BizPortWorkbook;
import org.skyve.metadata.controller.BizExportAction;
import org.skyve.web.WebContext;

/**
 * Exports an empty workbook from the row view, so that an export can be requested while zoomed in on a row of the
 * actions fixture, whose own view offers a different export.
 */
public class ExportFromRow extends BizExportAction {
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
