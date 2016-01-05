package modules.admin.DataMaintenance.actions;

import modules.admin.domain.DataMaintenance;

import org.skyve.EXT;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;
import org.skyve.wildcat.content.AttachmentContent;
import org.skyve.wildcat.content.ContentManager;

public class Select implements ServerSideAction<DataMaintenance> {
	private static final long serialVersionUID = 8136709192590507528L;

	// http://localhost:8080/wildcat/content?_n=AVCtdewOwOwKtS9t5EJ7&_doc=whosin.Staff&_b=contact.image&_ctim=1446019200611
	@Override
	public ServerSideActionResult execute(DataMaintenance bean, WebContext webContext)
	throws Exception {
/*
		String contentId = bean.getSelectedBizId();
		try (ContentManager cm = EXT.newContentManager()) {
			AttachmentContent ac = cm.get(contentId);
		}
*/
		return new ServerSideActionResult(bean);
	}
}
