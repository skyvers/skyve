package modules.kitchensink.ActionsFixture.actions;

import org.skyve.bizport.BizPortWorkbook;
import org.skyve.domain.messages.UploadException;
import org.skyve.domain.messages.UploadException.Problem;
import org.skyve.metadata.controller.BizImportAction;

/**
 * Imports nothing, and adds a warning that shows it ran, which is all the actions fixture needs to put a condition
 * on an import action.
 */
public class ActionsImport extends BizImportAction {
	/**
	 * Adds a warning that the import ran.
	 *
	 * @param bizPortable	The uploaded workbook.
	 * @param problems	The problems of the import, which the warning is added to.
	 */
	@Override
	public void bizImport(BizPortWorkbook bizPortable, UploadException problems) {
		problems.addWarning(new Problem("Actions Import ran", null));
	}
}
