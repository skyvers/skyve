package org.skyve.impl.mail;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

import java.io.ByteArrayOutputStream;
import java.util.Arrays;

import org.junit.jupiter.api.Test;
import org.skyve.util.Mail;
import org.skyve.util.MailDispatchOutcome;

class NoOpMailServiceTest {

	@SuppressWarnings("boxing")
	@Test
	void testNoOpMailServiceIgnoresAllMailOperations() {
		NoOpMailService service = new NoOpMailService();
		Mail mail = new Mail().from("sender@skyve.org")
								.addTo("to@skyve.org")
								.subject("Subject")
								.body("Body");

		ByteArrayOutputStream out = new ByteArrayOutputStream();
		service.writeMail(mail, out);
		service.sendMail(mail);
		service.sendBulkMail(Arrays.asList(mail));

		MailDispatchOutcome singleOutcome = service.dispatchMail(mail);
		MailDispatchOutcome bulkOutcome = service.dispatchBulkMail(Arrays.asList(mail));

		assertThat(out.size(), is(0));
		assertThat(singleOutcome.getStatus(), is(MailDispatchOutcome.DispatchStatus.SKIPPED));
		assertThat(singleOutcome.getProvider(), is("noop"));
		assertThat(singleOutcome.getRelayDetail(), is("NoOp mail service"));
		assertThat(bulkOutcome.getStatus(), is(MailDispatchOutcome.DispatchStatus.SKIPPED));
		assertThat(bulkOutcome.getProvider(), is("noop"));
		assertThat(bulkOutcome.getRelayDetail(), is("NoOp mail service"));
	}
}
