package modules.admin.ControlPanel.actions;

import org.skyve.EXT;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.util.Mail;
import org.skyve.web.WebContext;

import modules.admin.ControlPanel.ControlPanelExtension;
import modules.admin.domain.Contact;

public class SendMail implements ServerSideAction<ControlPanelExtension> {
	private static final long serialVersionUID = -4439819451806091821L;

	@Override
	public ServerSideActionResult<ControlPanelExtension> execute(ControlPanelExtension bean, WebContext webContext) 
	throws Exception {
		bean.setTabIndex(null);
		try {
			String emailTo = bean.getEmailTo();
			Contact emailToContact = bean.getEmailToContact();
			if ((emailTo == null) && (emailToContact != null)) {
				emailTo = emailToContact.getEmail1();
			}
			if (emailTo != null) {
				EXT.sendMail(new Mail()
									.addTo(emailTo)
									.from(bean.getEmailFrom())
									.subject(bean.getEmailSubject())
									.body("<html><head/><body>" + bean.getEmailContent() + "</body></html>"));
			}
		}
		catch (Exception e) {
			bean.trapException(e);
		}
		return new ServerSideActionResult<>(bean);
	}
}
