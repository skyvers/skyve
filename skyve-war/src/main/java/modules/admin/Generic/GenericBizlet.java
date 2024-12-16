package modules.admin.Generic;

import org.skyve.domain.Bean;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.web.WebContext;

import modules.admin.ControlPanel.ControlPanelExtension;
import modules.admin.domain.Generic;

public class GenericBizlet extends Bizlet<Generic> {

	@Override
	public Generic resolve(String bizId, Bean conversationBean, WebContext webContext) throws Exception {
		if (conversationBean instanceof ControlPanelExtension) {
			ControlPanelExtension controlPanel = (ControlPanelExtension) conversationBean;
			return controlPanel.addSystemDocumentationModule(bizId);
		}
		return super.resolve(bizId, conversationBean, webContext);
	}
	
}
