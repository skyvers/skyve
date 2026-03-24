package org.skyve.impl.mail;

import java.io.OutputStream;
import java.util.List;

import org.skyve.impl.util.UtilImpl;
import org.skyve.util.Mail;
import org.skyve.util.MailDispatchOutcome;
import org.skyve.util.MailService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import jakarta.annotation.Nonnull;

/**
 * Decorator that logs outbound mail dispatch attempts.
 */
public class LoggingMailService implements MailService {
	private static final Logger LOGGER = LoggerFactory.getLogger(LoggingMailService.class);
	private final MailService delegate;

	public LoggingMailService(@Nonnull MailService delegate) {
		this.delegate = delegate;
	}

	@Override
	public void writeMail(@Nonnull Mail mail, @Nonnull OutputStream out) {
		delegate.writeMail(mail, out);
	}

	@Override
	public void sendMail(@Nonnull Mail mail) {
		if (UtilImpl.SMTP_TEST_BOGUS_SEND) {
			logBogusSendMail(mail);
			MailLogUtil.logMail(mail, MailDispatchOutcome.skipped(resolveProviderName(), "testBogusSend"));
			return;
		}

		MailDispatchOutcome outcome = null;
		try {
			outcome = delegate.dispatchMail(mail);
			MailLogUtil.logMail(mail, defaultOutcomeIfNull(outcome));
		}
		catch (RuntimeException e) {
			MailLogUtil.logMail(mail, failureOutcome(outcome, e));
			throw e;
		}
	}

	@Override
	public void sendBulkMail(@Nonnull List<Mail> mails) {
		if (UtilImpl.SMTP_TEST_BOGUS_SEND) {
			logBogusSendBulkMail(mails);
			MailLogUtil.logBulkMail(mails, MailDispatchOutcome.skipped(resolveProviderName(), "testBogusSend"));
			return;
		}

		MailDispatchOutcome outcome = null;
		try {
			outcome = delegate.dispatchBulkMail(mails);
			MailLogUtil.logBulkMail(mails, defaultOutcomeIfNull(outcome));
		}
		catch (RuntimeException e) {
			MailLogUtil.logBulkMail(mails, failureOutcome(outcome, e));
			throw e;
		}
	}

	@Override
	public @Nonnull MailDispatchOutcome dispatchMail(@Nonnull Mail mail) {
		if (UtilImpl.SMTP_TEST_BOGUS_SEND) {
			logBogusSendMail(mail);
			MailDispatchOutcome outcome = MailDispatchOutcome.skipped(resolveProviderName(), "testBogusSend");
			MailLogUtil.logMail(mail, outcome);
			return outcome;
		}

		MailDispatchOutcome outcome = null;
		try {
			outcome = delegate.dispatchMail(mail);
			MailDispatchOutcome result = defaultOutcomeIfNull(outcome);
			MailLogUtil.logMail(mail, result);
			return result;
		}
		catch (RuntimeException e) {
			MailDispatchOutcome failure = failureOutcome(outcome, e);
			MailLogUtil.logMail(mail, failure);
			throw e;
		}
	}

	@Override
	public @Nonnull MailDispatchOutcome dispatchBulkMail(@Nonnull List<Mail> mails) {
		if (UtilImpl.SMTP_TEST_BOGUS_SEND) {
			logBogusSendBulkMail(mails);
			MailDispatchOutcome outcome = MailDispatchOutcome.skipped(resolveProviderName(), "testBogusSend");
			MailLogUtil.logBulkMail(mails, outcome);
			return outcome;
		}

		MailDispatchOutcome outcome = null;
		try {
			outcome = delegate.dispatchBulkMail(mails);
			MailDispatchOutcome result = defaultOutcomeIfNull(outcome);
			MailLogUtil.logBulkMail(mails, result);
			return result;
		}
		catch (RuntimeException e) {
			MailDispatchOutcome failure = failureOutcome(outcome, e);
			MailLogUtil.logBulkMail(mails, failure);
			throw e;
		}
	}

	private @Nonnull MailDispatchOutcome defaultOutcomeIfNull(MailDispatchOutcome outcome) {
		if (outcome == null) {
			return MailDispatchOutcome.sent(resolveProviderName());
		}

		if (outcome.getProvider() == null) {
			switch (outcome.getStatus()) {
			case FAILED:
				return MailDispatchOutcome.failed(resolveProviderName(), outcome.getFailureDetail());
			case SKIPPED:
				return MailDispatchOutcome.skipped(resolveProviderName(), outcome.getRelayDetail());
			case SENT:
			default:
				return MailDispatchOutcome.sent(resolveProviderName(),
												outcome.getProviderMessageId(),
												outcome.getRelayStatus(),
												outcome.getRelayDetail());
			}
		}

		return outcome;
	}

	private @Nonnull MailDispatchOutcome failureOutcome(MailDispatchOutcome outcome, RuntimeException e) {
		String provider = (outcome == null) ? resolveProviderName() : outcome.getProvider();
		String detail = e.getMessage();
		if ((detail == null) || detail.isBlank()) {
			detail = e.getClass().getName();
		}
		return MailDispatchOutcome.failed(provider, detail);
	}

	private String resolveProviderName() {
		return delegate.getClass().getSimpleName();
	}

	private static void logBogusSendBulkMail(@Nonnull List<Mail> mails) {
		if (mails.isEmpty()) {
			LOGGER.info("Bogus-send enabled: skipped dispatch for empty bulk mail request.");
			return;
		}
		for (Mail mail : mails) {
			logBogusSendMail(mail);
		}
	}

	private static void logBogusSendMail(@Nonnull Mail mail) {
		LOGGER.info("@@@@@@@@@@@@ EMAIL (BOGUS SEND ENABLED) @@@@@@@@@@@@");
		LOGGER.info("TO:");
		for (String to : mail.getRecipientEmailAddresses()) {
			LOGGER.info("    {}", to);
		}
		LOGGER.info("CC:");
		for (String cc : mail.getCcEmailAddresses()) {
			LOGGER.info("    {}", cc);
		}
		LOGGER.info("BCC:");
		for (String bcc : mail.getBccEmailAddresses()) {
			LOGGER.info("    {}", bcc);
		}
		LOGGER.info("SENDER: {}", mail.getSenderEmailAddress());
		LOGGER.info("SUBJECT {}", mail.getSubject());
		LOGGER.info("BODY {}", mail.getBody());
		LOGGER.info("CONTENT TYPE: {}", mail.getContentType());
		LOGGER.info("@@@@@@@@@@@@ EMAIL (BOGUS SEND ENABLED) @@@@@@@@@@@@");
	}
}
