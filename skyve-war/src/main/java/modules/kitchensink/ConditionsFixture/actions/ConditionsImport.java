package modules.kitchensink.ConditionsFixture.actions;

import org.skyve.bizport.BizPortWorkbook;
import org.skyve.domain.messages.UploadException;
import org.skyve.metadata.controller.BizImportAction;

/**
 * Imports nothing, which is all the conditions fixture needs to put a condition on an import action.
 */
public class ConditionsImport extends BizImportAction {
	@Override
	public void bizImport(BizPortWorkbook bizPortable, UploadException problems) throws Exception {
		// nothing to import
	}
}
