package modules.admin.Configuration.actions;

import org.skyve.EXT;
import org.skyve.domain.messages.MessageSeverity;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.util.Mail;
import org.skyve.web.WebContext;

import modules.admin.Configuration.ConfigurationExtension;
import modules.admin.domain.Contact;

public class SendMail implements ServerSideAction<ConfigurationExtension> {

	@Override
	public ServerSideActionResult<ConfigurationExtension> execute(ConfigurationExtension bean, WebContext webContext)
	throws Exception {
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
			webContext.growl(MessageSeverity.error, String.format("There was an error sending the test email: %s", e.getMessage()));
		}
		return new ServerSideActionResult<>(bean);
	}
}
