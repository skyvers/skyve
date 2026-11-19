package org.skyve.impl.mail;

import java.io.OutputStream;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.skyve.impl.util.UtilImpl;
import org.skyve.util.Mail;
import org.skyve.util.MailService;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * A mail service for tests that records mail instead of sending it, so a test can assert on what would have been sent.
 * <p>
 * Sent, bulk-sent and written mail are recorded separately, in the order they arrive. The mail recorded is the mail
 * the service is given, so when it is reached through {@link MailServiceStaticSingleton#getEffective()} it is the mail
 * after Skyve's pre-processing (default sender, test recipient and so on).
 * </p>
 * <p>
 * Either set it with {@link MailServiceStaticSingleton#set(MailService)}, or call {@link #install()} before a test
 * and {@link #uninstall()} after it.
 * </p>
 */
public class CaptureMailService implements MailService {
	private final List<Mail> sent = new ArrayList<>();
	private final List<List<Mail>> bulkSent = new ArrayList<>();
	private final List<Mail> written = new ArrayList<>();

	private boolean installed;
	private @Nullable MailService previousService;
	private boolean previousTestBogusSend;
	private @Nullable String previousTestRecipient;

	/**
	 * Makes this the configured mail service and switches off the test harness's bogus send and test recipient,
	 * so the mail recorded has its real recipients. {@link #uninstall()} puts all three back.
	 *
	 * @throws IllegalStateException if this is already installed.
	 */
	public void install() {
		if (installed) {
			throw new IllegalStateException("This CaptureMailService is already installed.");
		}
		previousService = configuredService();
		previousTestBogusSend = UtilImpl.SMTP_TEST_BOGUS_SEND;
		previousTestRecipient = UtilImpl.SMTP_TEST_RECIPIENT;
		UtilImpl.SMTP_TEST_BOGUS_SEND = false;
		UtilImpl.SMTP_TEST_RECIPIENT = null;
		MailServiceStaticSingleton.set(this);
		installed = true;
	}

	/**
	 * Puts back what {@link #install()} replaced. If no mail service was configured before, a
	 * {@link NoOpMailService} is configured, so nothing can be sent. Does nothing if this is not installed.
	 */
	public void uninstall() {
		if (! installed) {
			return;
		}
		UtilImpl.SMTP_TEST_BOGUS_SEND = previousTestBogusSend;
		UtilImpl.SMTP_TEST_RECIPIENT = previousTestRecipient;
		MailService previous = previousService;
		MailServiceStaticSingleton.set((previous == null) ? new NoOpMailService() : previous);
		previousService = null;
		installed = false;
	}

	private static @Nullable MailService configuredService() {
		try {
			return MailServiceStaticSingleton.get();
		}
		catch (@SuppressWarnings("unused") IllegalStateException e) {
			return null;
		}
	}

	/**
	 * Records the mail as written; nothing is written to the stream.
	 */
	@Override
	public void writeMail(@Nonnull Mail mail, @Nonnull OutputStream out) {
		written.add(mail);
	}

	/**
	 * Records the mail as sent.
	 */
	@Override
	public void sendMail(@Nonnull Mail mail) {
		sent.add(mail);
	}

	/**
	 * Records the list as one bulk send.
	 */
	@Override
	public void sendBulkMail(@Nonnull List<Mail> mails) {
		bulkSent.add(mails);
	}

	/**
	 * Returns the mail given to {@link #sendMail(Mail)}, oldest first. Bulk sends are not included.
	 *
	 * @return an unmodifiable view of the mail sent.
	 */
	public @Nonnull List<Mail> getSent() {
		return Collections.unmodifiableList(sent);
	}

	/**
	 * Returns the lists given to {@link #sendBulkMail(List)}, oldest first.
	 *
	 * @return an unmodifiable view of the bulk sends.
	 */
	public @Nonnull List<List<Mail>> getBulkSent() {
		return Collections.unmodifiableList(bulkSent);
	}

	/**
	 * Returns the mail given to {@link #writeMail(Mail, OutputStream)}, oldest first.
	 *
	 * @return an unmodifiable view of the mail written.
	 */
	public @Nonnull List<Mail> getWritten() {
		return Collections.unmodifiableList(written);
	}

	/**
	 * Returns the mail most recently given to {@link #sendMail(Mail)}.
	 *
	 * @return the last mail sent.
	 * @throws IllegalStateException if no mail has been sent.
	 */
	public @Nonnull Mail getLastSent() {
		return last(sent, "No mail has been sent.");
	}

	/**
	 * Returns the list most recently given to {@link #sendBulkMail(List)}.
	 *
	 * @return the last bulk send.
	 * @throws IllegalStateException if no bulk mail has been sent.
	 */
	public @Nonnull List<Mail> getLastBulkSent() {
		return last(bulkSent, "No bulk mail has been sent.");
	}

	/**
	 * Returns the mail most recently given to {@link #writeMail(Mail, OutputStream)}.
	 *
	 * @return the last mail written.
	 * @throws IllegalStateException if no mail has been written.
	 */
	public @Nonnull Mail getLastWritten() {
		return last(written, "No mail has been written.");
	}

	private static @Nonnull <T> T last(@Nonnull List<T> list, @Nonnull String message) {
		if (list.isEmpty()) {
			throw new IllegalStateException(message);
		}
		return list.get(list.size() - 1);
	}
}
