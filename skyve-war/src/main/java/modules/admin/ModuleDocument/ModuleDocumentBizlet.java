package modules.admin.ModuleDocument;

import org.skyve.domain.Bean;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.web.WebContext;

import modules.admin.ControlPanel.ControlPanelExtension;
import modules.admin.domain.ModuleDocument;

/**
 * Bizlet used when resolving module documents from the Control Panel.
 */
public class ModuleDocumentBizlet extends Bizlet<ModuleDocument> {

        /**
         * Resolve the requested document, adding it to the current Control Panel
         * session when applicable.
         */
        @Override
        public ModuleDocument resolve(String bizId, Bean conversationBean, WebContext webContext) throws Exception {
		if (conversationBean instanceof ControlPanelExtension controlPanelExtension) {
			ControlPanelExtension controlPanel = controlPanelExtension;
			return controlPanel.addDocumentToCreate(bizId);
		}
		return super.resolve(bizId, conversationBean, webContext);
	}
}
