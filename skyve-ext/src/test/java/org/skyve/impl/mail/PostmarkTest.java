package org.skyve.impl.mail;

import static org.junit.jupiter.api.Assertions.assertThrows;

import java.io.ByteArrayOutputStream;
import java.util.Arrays;

import org.junit.jupiter.api.Test;
import org.skyve.util.Mail;

@SuppressWarnings("static-method")
class PostmarkTest {

	@Test
	void sendMailThrowsIllegalState() {
		Postmark svc = new Postmark();
		Mail mail = new Mail();
		assertThrows(IllegalStateException.class, () -> svc.sendMail(mail));
	}

	@Test
	void sendBulkMailThrowsIllegalState() {
		Postmark svc = new Postmark();
		Mail mail = new Mail();
		java.util.List<Mail> mails = Arrays.asList(mail);
		assertThrows(IllegalStateException.class, () -> svc.sendBulkMail(mails));
	}

	@Test
	void writeMailThrowsIllegalState() {
		Postmark svc = new Postmark();
		Mail mail = new Mail();
		java.io.OutputStream out = new ByteArrayOutputStream();
		assertThrows(IllegalStateException.class, () -> svc.writeMail(mail, out));
	}
}
