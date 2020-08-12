package modules.admin.DocumentName;

import org.skyve.domain.Bean;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.web.WebContext;

import modules.admin.ControlPanel.ControlPanelExtension;
import modules.admin.domain.DocumentName;

public class DocumentNameBizlet extends Bizlet<DocumentName> {

	private static final long serialVersionUID = 7301874108011571642L;

	@Override
	public DocumentName resolve(String bizId, Bean conversationBean, WebContext webContext) throws Exception {
		if (conversationBean instanceof ControlPanelExtension) {
			ControlPanelExtension controlPanel = (ControlPanelExtension) conversationBean;
			return controlPanel.addDocumentToCreate(bizId);
		}
		return super.resolve(bizId, conversationBean, webContext);
	}
}
