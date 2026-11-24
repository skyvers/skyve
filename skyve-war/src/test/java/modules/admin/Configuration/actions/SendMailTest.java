package modules.admin.Configuration.actions;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.mail.CaptureMailService;
import org.skyve.impl.mail.MailServiceStaticSingleton;
import org.skyve.impl.util.UtilImpl;
import org.skyve.util.MailService;
import org.skyve.web.WebContext;

import modules.admin.Contact.ContactExtension;
import modules.admin.Configuration.ConfigurationExtension;

class SendMailTest {
	private final CaptureMailService capture = new CaptureMailService();

	private MailService originalMailService;
	private String originalSmtpTestRecipient;
	private boolean originalSmtpTestBogusSend;

	@BeforeEach
	void beforeEach() {
		MailServiceStaticSingleton.setDefault();
		originalMailService = MailServiceStaticSingleton.get();
		originalSmtpTestRecipient = UtilImpl.SMTP_TEST_RECIPIENT;
		originalSmtpTestBogusSend = UtilImpl.SMTP_TEST_BOGUS_SEND;

		MailServiceStaticSingleton.set(capture);
		UtilImpl.SMTP_TEST_RECIPIENT = null;
		UtilImpl.SMTP_TEST_BOGUS_SEND = false;
	}

	@AfterEach
	void afterEach() {
		MailServiceStaticSingleton.set(originalMailService);
		UtilImpl.SMTP_TEST_RECIPIENT = originalSmtpTestRecipient;
		UtilImpl.SMTP_TEST_BOGUS_SEND = originalSmtpTestBogusSend;
	}

	@Test
	void testExecuteUsesExtMailService() throws Exception {
		ConfigurationExtension bean = mock(ConfigurationExtension.class);
		ContactExtension contact = mock(ContactExtension.class);
		WebContext webContext = mock(WebContext.class);

		when(bean.getEmailTo()).thenReturn(null);
		when(bean.getEmailToContact()).thenReturn(contact);
		when(contact.getEmail1()).thenReturn("to@skyve.org");
		when(bean.getEmailFrom()).thenReturn("from@skyve.org");
		when(bean.getEmailSubject()).thenReturn("Subject");
		when(bean.getEmailContent()).thenReturn("Body");

		new SendMail().execute(bean, webContext);

		assertEquals(1, capture.getSent().size());
		assertTrue(capture.getLastSent().getRecipientEmailAddresses().contains("to@skyve.org"));
		assertThat(capture.getLastSent().getSenderEmailAddress(), is("from@skyve.org"));
		assertThat(capture.getLastSent().getSubject(), is("Subject"));
		assertThat(capture.getLastSent().getBody(), is("<html><head/><body>Body</body></html>"));
		verifyNoInteractions(webContext);
	}
}
