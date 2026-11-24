package org.skyve.impl.mail;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.contains;
import static org.hamcrest.Matchers.empty;
import static org.hamcrest.Matchers.instanceOf;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;
import static org.hamcrest.Matchers.sameInstance;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.io.ByteArrayOutputStream;
import java.lang.reflect.Field;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.util.UtilImpl;
import org.skyve.util.Mail;
import org.skyve.util.MailService;

@SuppressWarnings("static-method")
class CaptureMailServiceTest {
	private MailService originalService;
	private boolean originalTestBogusSend;
	private String originalTestRecipient;

	@BeforeEach
	void saveState() throws Exception {
		originalService = (MailService) configuredInstanceField().get(null);
		originalTestBogusSend = UtilImpl.SMTP_TEST_BOGUS_SEND;
		originalTestRecipient = UtilImpl.SMTP_TEST_RECIPIENT;
	}

	@AfterEach
	void restoreState() throws Exception {
		if (originalService == null) {
			configuredInstanceField().set(null, null);
		}
		else {
			MailServiceStaticSingleton.set(originalService);
		}
		UtilImpl.SMTP_TEST_BOGUS_SEND = originalTestBogusSend;
		UtilImpl.SMTP_TEST_RECIPIENT = originalTestRecipient;
	}

	private static Field configuredInstanceField() throws Exception {
		Field result = MailServiceStaticSingleton.class.getDeclaredField("configuredInstance");
		result.setAccessible(true);
		return result;
	}

	@Test
	void recordsSentBulkAndWrittenMailSeparatelyInOrder() {
		CaptureMailService capture = new CaptureMailService();
		Mail first = new Mail();
		Mail second = new Mail();
		Mail written = new Mail();
		List<Mail> bulk = List.of(new Mail(), new Mail());

		capture.sendMail(first);
		capture.dispatchMail(second);
		capture.sendBulkMail(bulk);
		capture.writeMail(written, new ByteArrayOutputStream());

		assertThat(capture.getSent(), contains(first, second));
		assertThat(capture.getLastSent(), is(sameInstance(second)));
		assertThat(capture.getBulkSent(), contains(bulk));
		assertThat(capture.getLastBulkSent(), is(sameInstance(bulk)));
		assertThat(capture.getWritten(), contains(written));
		assertThat(capture.getLastWritten(), is(sameInstance(written)));
	}

	@Test
	void writingWritesNothingToTheStream() {
		CaptureMailService capture = new CaptureMailService();
		ByteArrayOutputStream out = new ByteArrayOutputStream();

		capture.writeMail(new Mail(), out);

		assertThat(Integer.valueOf(out.size()), is(Integer.valueOf(0)));
	}

	@Test
	void recordedListsAreUnmodifiable() {
		CaptureMailService capture = new CaptureMailService();

		assertThrows(UnsupportedOperationException.class, () -> capture.getSent().add(new Mail()));
		assertThrows(UnsupportedOperationException.class, () -> capture.getBulkSent().add(List.of()));
		assertThrows(UnsupportedOperationException.class, () -> capture.getWritten().add(new Mail()));
	}

	@Test
	void lastThrowsWhenNothingWasRecorded() {
		CaptureMailService capture = new CaptureMailService();

		assertThat(capture.getSent(), is(empty()));
		assertThrows(IllegalStateException.class, capture::getLastSent);
		assertThrows(IllegalStateException.class, capture::getLastBulkSent);
		assertThrows(IllegalStateException.class, capture::getLastWritten);
	}

	@Test
	void installCapturesWithRealRecipientsAndUninstallPutsEverythingBack() {
		MailService previous = new NoOpMailService();
		MailServiceStaticSingleton.set(previous);
		UtilImpl.SMTP_TEST_BOGUS_SEND = true;
		UtilImpl.SMTP_TEST_RECIPIENT = "redirect@skyve.org";
		CaptureMailService capture = new CaptureMailService();

		capture.install();

		assertThat(MailServiceStaticSingleton.get(), is(sameInstance(capture)));
		assertThat(Boolean.valueOf(UtilImpl.SMTP_TEST_BOGUS_SEND), is(Boolean.FALSE));
		assertThat(UtilImpl.SMTP_TEST_RECIPIENT, is(nullValue()));

		capture.uninstall();

		assertThat(MailServiceStaticSingleton.get(), is(sameInstance(previous)));
		assertThat(Boolean.valueOf(UtilImpl.SMTP_TEST_BOGUS_SEND), is(Boolean.TRUE));
		assertThat(UtilImpl.SMTP_TEST_RECIPIENT, is("redirect@skyve.org"));
	}

	@Test
	void uninstallConfiguresNoOpWhenNothingWasConfiguredBefore() throws Exception {
		configuredInstanceField().set(null, null);
		CaptureMailService capture = new CaptureMailService();

		capture.install();
		capture.uninstall();

		assertThat(MailServiceStaticSingleton.get(), is(instanceOf(NoOpMailService.class)));
	}

	@Test
	void installTwiceThrows() {
		CaptureMailService capture = new CaptureMailService();
		capture.install();

		assertThrows(IllegalStateException.class, capture::install);
	}

	@Test
	void uninstallWithoutInstallChangesNothing() {
		MailService previous = new NoOpMailService();
		MailServiceStaticSingleton.set(previous);
		UtilImpl.SMTP_TEST_RECIPIENT = "redirect@skyve.org";

		new CaptureMailService().uninstall();

		assertThat(MailServiceStaticSingleton.get(), is(sameInstance(previous)));
		assertThat(UtilImpl.SMTP_TEST_RECIPIENT, is("redirect@skyve.org"));
	}
}
