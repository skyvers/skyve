package modules.admin.ModuleDocument;

import org.skyve.domain.Bean;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.web.WebContext;

import modules.admin.ControlPanel.ControlPanelExtension;
import modules.admin.domain.ModuleDocument;

public class ModuleDocumentBizlet extends Bizlet<ModuleDocument> {

	@Override
	public ModuleDocument resolve(String bizId, Bean conversationBean, WebContext webContext) throws Exception {
		if (conversationBean instanceof ControlPanelExtension) {
			ControlPanelExtension controlPanel = (ControlPanelExtension) conversationBean;
			return controlPanel.addDocumentToCreate(bizId);
		}
		return super.resolve(bizId, conversationBean, webContext);
	}
}
