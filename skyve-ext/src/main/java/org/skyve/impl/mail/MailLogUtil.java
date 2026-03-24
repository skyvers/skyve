package org.skyve.impl.mail;

import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;
import java.util.stream.Collectors;

import org.skyve.CORE;
import org.skyve.domain.app.admin.MailLog;
import org.skyve.domain.types.Timestamp;
import org.skyve.impl.metadata.user.SuperUser;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.persistence.hibernate.AbstractHibernatePersistence;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.view.TextOutput.Sanitisation;
import org.skyve.util.Mail;
import org.skyve.util.MailAttachment;
import org.skyve.util.MailDispatchOutcome;
import org.skyve.util.OWASP;
import org.skyve.util.Util;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Utility for persisting outbound mail dispatch logs.
 */
public class MailLogUtil {
	private static final Logger LOGGER = LoggerFactory.getLogger(MailLogUtil.class);
	private static final String ANONYMOUS_MAIL_LOG_USER = "mailLogUser";
	private static final String REDACTED_BODY_EXCERPT = "[REDACTED]";
	private static final Pattern HTML_LINE_BREAK_TAG_PATTERN = Pattern.compile("(?i)<\\s*br\\s*/?\\s*>");
	private static final Pattern HTML_BLOCK_CLOSE_TAG_PATTERN = Pattern.compile("(?i)</\\s*(p|div|li|tr|h[1-6]|ul|ol|table)\\s*>");
	private static final Pattern HORIZONTAL_WHITESPACE_PATTERN = Pattern.compile("[\\t\\f\\x0B ]+");
	private static final Pattern EXCESS_NEWLINE_PATTERN = Pattern.compile("\\n{3,}");
	private static final Pattern TWO_FACTOR_CODE_PATTERN = Pattern.compile("(?i)(\\b(?:verification|security|one[-\\s]?time|two[-\\s]?factor|2fa|otp)\\s+code\\s*(?:(?:is\\s*[:=]?)|[:=])?\\s*)(\\d{4,10})\\b");

	@FunctionalInterface
	interface Recorder {
		void record(MailLogEntry entry);
	}

	private static volatile Recorder recorder;

	private MailLogUtil() {
		// utility
	}

	public static void logMail(@Nonnull Mail mail, @Nonnull MailDispatchOutcome outcome) {
		MailLogEntry entry = createSingleEntry(mail, outcome);
		persist(entry);
	}

	public static void logBulkMail(@Nonnull List<Mail> mails, @Nonnull MailDispatchOutcome outcome) {
		MailLogEntry entry = createBulkEntry(mails, outcome);
		persist(entry);
	}

	static void setRecorderForTesting(Recorder recorder) {
		MailLogUtil.recorder = recorder;
	}

	static void clearRecorderForTesting() {
		MailLogUtil.recorder = null;
	}

	static @Nullable String bodyExcerpt(@Nullable String body) {
		return (Util.processStringValue(body) == null) ? null : REDACTED_BODY_EXCERPT;
	}

	private static @Nullable String plainTextBody(@Nullable String body) {
		String cleaned = Util.processStringValue(body);
		if (cleaned == null) {
			return null;
		}

		cleaned = cleaned.replace("\r\n", "\n").replace('\r', '\n');
		cleaned = HTML_LINE_BREAK_TAG_PATTERN.matcher(cleaned).replaceAll("\n");
		cleaned = HTML_BLOCK_CLOSE_TAG_PATTERN.matcher(cleaned).replaceAll("\n");

		cleaned = Util.processStringValue(OWASP.sanitise(Sanitisation.text, cleaned));
		if (cleaned == null) {
			return null;
		}

		cleaned = HORIZONTAL_WHITESPACE_PATTERN.matcher(cleaned).replaceAll(" ");
		cleaned = cleaned.replaceAll(" *\\n *", "\n");
		cleaned = EXCESS_NEWLINE_PATTERN.matcher(cleaned).replaceAll("\n\n");
		cleaned = Util.processStringValue(cleaned);
		if (cleaned == null) {
			return null;
		}
		return maskSensitiveCodes(cleaned);
	}

	private static String maskSensitiveCodes(String body) {
		Matcher matcher = TWO_FACTOR_CODE_PATTERN.matcher(body);
		StringBuffer result = new StringBuffer(body.length());
		while (matcher.find()) {
			String replacement = matcher.group(1) + "*".repeat(matcher.group(2).length());
			matcher.appendReplacement(result, Matcher.quoteReplacement(replacement));
		}
		matcher.appendTail(result);
		return result.toString();
	}

	private static MailLogEntry createSingleEntry(Mail mail, MailDispatchOutcome outcome) {
		return createEntry(List.of(mail), outcome, false);
	}

	private static MailLogEntry createBulkEntry(List<Mail> mails, MailDispatchOutcome outcome) {
		return createEntry(mails, outcome, true);
	}

	private static MailLogEntry createEntry(List<Mail> mails, MailDispatchOutcome outcome, boolean bulk) {
		TreeSet<String> toRecipients = new TreeSet<>();
		TreeSet<String> ccRecipients = new TreeSet<>();
		TreeSet<String> bccRecipients = new TreeSet<>();
		TreeSet<String> attachmentNames = new TreeSet<>();
		Set<String> uniqueRecipients = new HashSet<>();
		Set<String> subjectVariants = new HashSet<>();
		Set<String> bodyVariants = new HashSet<>();

		for (Mail mail : mails) {
			for (String to : mail.getRecipientEmailAddresses()) {
				toRecipients.add(to);
				uniqueRecipients.add(to);
			}
			for (String cc : mail.getCcEmailAddresses()) {
				ccRecipients.add(cc);
				uniqueRecipients.add(cc);
			}
			for (String bcc : mail.getBccEmailAddresses()) {
				bccRecipients.add(bcc);
				uniqueRecipients.add(bcc);
			}

			for (MailAttachment attachment : mail.getAttachments()) {
				if (attachment != null) {
					String attachmentName = Util.processStringValue(attachment.getAttachmentFileName());
					if (attachmentName != null) {
						attachmentNames.add(attachmentName);
					}
				}
			}

			String subject = Util.processStringValue(mail.getSubject());
			subjectVariants.add((subject == null) ? "<null>" : subject);

			String plainBody = plainTextBody(mail.getBody());
			bodyVariants.add((plainBody == null) ? "<null>" : plainBody);
		}

		Mail firstMail = mails.isEmpty() ? new Mail() : mails.get(0);
		String firstSubject = Util.processStringValue(firstMail.getSubject());
		String firstBodyExcerpt = bodyExcerpt(firstMail.getBody());

		return new MailLogEntry(new Timestamp(),
								join(toRecipients),
								join(ccRecipients),
								join(bccRecipients),
								firstSubject,
								firstBodyExcerpt,
								join(attachmentNames),
								outcome.getStatus().name(),
								outcome.getProvider(),
								outcome.getProviderMessageId(),
								outcome.getRelayStatus(),
								outcome.getRelayDetail(),
								outcome.getFailureDetail(),
								Boolean.valueOf(bulk),
								Long.valueOf(mails.size()),
								Long.valueOf(uniqueRecipients.size()),
								Boolean.valueOf(subjectVariants.size() > 1),
								Long.valueOf(subjectVariants.size()),
								Boolean.valueOf(bodyVariants.size() > 1),
								Long.valueOf(bodyVariants.size()));
	}

	private static @Nullable String join(TreeSet<String> values) {
		if (values.isEmpty()) {
			return null;
		}
		return values.stream().collect(Collectors.joining(", "));
	}

	private static void persist(MailLogEntry entry) {
		Recorder r = recorder;
		if (r != null) {
			r.record(entry);
			return;
		}

		if (AbstractPersistence.IMPLEMENTATION_CLASS == null) {
			return;
		}

		AbstractHibernatePersistence tempP = null;
		try {
			org.skyve.metadata.user.User currentUser = null;
			try {
				currentUser = CORE.getPersistence().getUser();
			}
			catch (Exception e) {
				LOGGER.debug("Unable to access current persistence user for MailLog", e);
			}

			SuperUser superUser = createMailLogUser(currentUser);
			if (superUser == null) {
				LOGGER.warn("Cannot persist MailLog as customer is indeterminate");
				return;
			}

			tempP = (AbstractHibernatePersistence) AbstractPersistence.newInstance();
			tempP.setUser(superUser);
			tempP.begin();

			try {
				MailLog log = MailLog.newInstance(superUser);
				log.setTimestamp(entry.timestamp);
				log.setToRecipients(entry.toRecipients);
				log.setCcRecipients(entry.ccRecipients);
				log.setBccRecipients(entry.bccRecipients);
				log.setSubject(entry.subject);
				log.setBodyExcerpt(entry.bodyExcerpt);
				log.setAttachmentFileNames(entry.attachmentFileNames);
				log.setDispatchStatus(entry.dispatchStatus);
				log.setProvider(entry.provider);
				log.setProviderMessageId(entry.providerMessageId);
				log.setRelayStatus(entry.relayStatus);
				log.setRelayDetail(entry.relayDetail);
				log.setErrorDetail(entry.errorDetail);
				log.setIsBulk(entry.isBulk);
				log.setMailCount(entry.mailCount);
				log.setRecipientCount(entry.recipientCount);
				log.setHasMultipleSubjects(entry.hasMultipleSubjects);
				log.setSubjectVariantCount(entry.subjectVariantCount);
				log.setHasMultipleBodies(entry.hasMultipleBodies);
				log.setBodyVariantCount(entry.bodyVariantCount);
				tempP.upsertBeanTuple(log);
			}
			catch (Exception e) {
				LOGGER.error("Failed to persist MailLog entry", e);
			}

			try {
				tempP.commit(false);
			}
			catch (Exception e) {
				LOGGER.error("Failed to commit MailLog transaction", e);
			}
		}
		catch (Exception e) {
			LOGGER.error("Failed to create MailLog persistence context", e);
		}
		finally {
			if (tempP != null) {
				try {
					tempP.close();
				}
				catch (Exception e) {
					LOGGER.error("Failed to close MailLog persistence context", e);
				}
			}
		}
	}

	private static @Nullable SuperUser createMailLogUser(@Nullable org.skyve.metadata.user.User currentUser) {
		String currentUserId = (currentUser == null) ? null : Util.processStringValue(currentUser.getId());
		if (currentUserId != null) {
			return new SuperUser(currentUser);
		}

		String customerName = (currentUser == null) ? null : Util.processStringValue(currentUser.getCustomerName());
		if (customerName == null) {
			customerName = Util.processStringValue(UtilImpl.CUSTOMER);
		}
		if (customerName == null) {
			return null;
		}

		SuperUser superUser = new SuperUser();
		superUser.setName(ANONYMOUS_MAIL_LOG_USER);
		superUser.setId(ANONYMOUS_MAIL_LOG_USER);
		superUser.setCustomerName(customerName);
		return superUser;
	}

	static final class MailLogEntry {
		private final Timestamp timestamp;
		private final String toRecipients;
		private final String ccRecipients;
		private final String bccRecipients;
		private final String subject;
		private final String bodyExcerpt;
		private final String attachmentFileNames;
		private final String dispatchStatus;
		private final String provider;
		private final String providerMessageId;
		private final String relayStatus;
		private final String relayDetail;
		private final String errorDetail;
		private final Boolean isBulk;
		private final Long mailCount;
		private final Long recipientCount;
		private final Boolean hasMultipleSubjects;
		private final Long subjectVariantCount;
		private final Boolean hasMultipleBodies;
		private final Long bodyVariantCount;

		private MailLogEntry(Timestamp timestamp,
							String toRecipients,
							String ccRecipients,
							String bccRecipients,
							String subject,
							String bodyExcerpt,
							String attachmentFileNames,
							String dispatchStatus,
							String provider,
							String providerMessageId,
							String relayStatus,
							String relayDetail,
							String errorDetail,
							Boolean isBulk,
							Long mailCount,
							Long recipientCount,
							Boolean hasMultipleSubjects,
							Long subjectVariantCount,
							Boolean hasMultipleBodies,
							Long bodyVariantCount) {
			this.timestamp = timestamp;
			this.toRecipients = toRecipients;
			this.ccRecipients = ccRecipients;
			this.bccRecipients = bccRecipients;
			this.subject = subject;
			this.bodyExcerpt = bodyExcerpt;
			this.attachmentFileNames = attachmentFileNames;
			this.dispatchStatus = dispatchStatus;
			this.provider = provider;
			this.providerMessageId = providerMessageId;
			this.relayStatus = relayStatus;
			this.relayDetail = relayDetail;
			this.errorDetail = errorDetail;
			this.isBulk = isBulk;
			this.mailCount = mailCount;
			this.recipientCount = recipientCount;
			this.hasMultipleSubjects = hasMultipleSubjects;
			this.subjectVariantCount = subjectVariantCount;
			this.hasMultipleBodies = hasMultipleBodies;
			this.bodyVariantCount = bodyVariantCount;
		}

		Timestamp getTimestamp() {
			return timestamp;
		}

		String getToRecipients() {
			return toRecipients;
		}

		String getCcRecipients() {
			return ccRecipients;
		}

		String getBccRecipients() {
			return bccRecipients;
		}

		String getSubject() {
			return subject;
		}

		String getBodyExcerpt() {
			return bodyExcerpt;
		}

		String getAttachmentFileNames() {
			return attachmentFileNames;
		}

		String getDispatchStatus() {
			return dispatchStatus;
		}

		String getProvider() {
			return provider;
		}

		String getProviderMessageId() {
			return providerMessageId;
		}

		String getRelayStatus() {
			return relayStatus;
		}

		String getRelayDetail() {
			return relayDetail;
		}

		String getErrorDetail() {
			return errorDetail;
		}

		Boolean getIsBulk() {
			return isBulk;
		}

		Long getMailCount() {
			return mailCount;
		}

		Long getRecipientCount() {
			return recipientCount;
		}

		Boolean getHasMultipleSubjects() {
			return hasMultipleSubjects;
		}

		Long getSubjectVariantCount() {
			return subjectVariantCount;
		}

		Boolean getHasMultipleBodies() {
			return hasMultipleBodies;
		}

		Long getBodyVariantCount() {
			return bodyVariantCount;
		}
	}
}
