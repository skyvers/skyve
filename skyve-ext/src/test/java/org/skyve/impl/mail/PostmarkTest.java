package org.skyve.impl.mail;

import static org.junit.Assert.assertThrows;

import java.io.ByteArrayOutputStream;
import java.util.Arrays;

import org.junit.Test;
import org.skyve.util.Mail;

@SuppressWarnings("static-method")
public class PostmarkTest {

	@Test
	public void sendMailThrowsIllegalState() {
		Postmark svc = new Postmark();
		Mail mail = new Mail();
		assertThrows(IllegalStateException.class, () -> svc.sendMail(mail));
	}

	@Test
	public void sendBulkMailThrowsIllegalState() {
		Postmark svc = new Postmark();
		Mail mail = new Mail();
		java.util.List<Mail> mails = Arrays.asList(mail);
		assertThrows(IllegalStateException.class, () -> svc.sendBulkMail(mails));
	}

	@Test
	public void writeMailThrowsIllegalState() {
		Postmark svc = new Postmark();
		Mail mail = new Mail();
		java.io.OutputStream out = new ByteArrayOutputStream();
		assertThrows(IllegalStateException.class, () -> svc.writeMail(mail, out));
	}
}
