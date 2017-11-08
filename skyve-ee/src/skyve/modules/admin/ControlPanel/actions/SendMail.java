package modules.admin.ControlPanel.actions;

import org.skyve.content.MimeType;
import org.skyve.impl.util.MailUtil;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;

import modules.admin.ControlPanel.ControlPanelExtension;

public class SendMail implements ServerSideAction<ControlPanelExtension> {
	private static final long serialVersionUID = -4439819451806091821L;

	@Override
	public ServerSideActionResult<ControlPanelExtension> execute(ControlPanelExtension bean, WebContext webContext) 
	throws Exception {
		try {
			MailUtil.sendMail(new String[] {bean.getEmailTo()},
								null, 
								null, 
								bean.getEmailFrom(), 
								bean.getEmailSubject(),
								"<html><head/><body>" + bean.getEmailContent() + "</body></html>",
								MimeType.html, 
								null, 
								null, 
								null);
		}
		catch (Exception e) {
			bean.trapException(e);
		}
		return new ServerSideActionResult<>(bean);
	}
}
