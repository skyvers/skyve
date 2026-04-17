package org.skyve.impl.mail;

import java.util.HashSet;
import java.util.List;
import java.util.Locale;
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
	private static final String[] SENSITIVE_CODE_PREFIXES = {
			"verification code",
			"security code",
			"one time code",
			"one-time code",
			"onetime code",
			"two factor code",
			"two-factor code",
			"twofactor code",
			"2fa code",
			"otp code",
			"passcode code",
			"pin code",
			"token code"
	};

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
		String cleaned = Util.processStringValue(body);
		return (cleaned == null) ? null : REDACTED_BODY_EXCERPT;
	}

	private static @Nullable String plainTextBody(@Nullable String body) {
		String cleaned = Util.processStringValue(body);
		if (cleaned == null) {
			return null;
		}

		cleaned = cleaned.replace("\r\n", "\n").replace('\r', '\n');
		cleaned = normaliseHtmlLineBreaks(cleaned);

		cleaned = Util.processStringValue(OWASP.sanitise(Sanitisation.text, cleaned));
		if (cleaned == null) {
			return null;
		}

		cleaned = collapseHorizontalWhitespace(cleaned);
		cleaned = trimSpacesAroundNewlines(cleaned);
		cleaned = collapseExcessNewlines(cleaned);
		cleaned = Util.processStringValue(cleaned);
		if (cleaned == null) {
			return null;
		}
		return maskSensitiveCodes(cleaned);
	}

	private static String normaliseHtmlLineBreaks(String input) {
		StringBuilder result = new StringBuilder(input.length());
		int i = 0;
		int length = input.length();
		while (i < length) {
			char c = input.charAt(i);
			if (c != '<') {
				result.append(c);
				i++;
				continue;
			}

			int tagEnd = i + 1;
			while ((tagEnd < length) && (input.charAt(tagEnd) != '>')) {
				tagEnd++;
			}
			if (tagEnd >= length) {
				result.append(input, i, length);
				break;
			}

			if (isLineBreakTag(input, i + 1, tagEnd) || isBlockClosingTag(input, i + 1, tagEnd)) {
				result.append('\n');
			}
			else {
				result.append(input, i, tagEnd + 1);
			}
			i = tagEnd + 1;
		}
		return result.toString();
	}

	private static boolean isLineBreakTag(String input, int start, int endExclusive) {
		int i = skipWhitespace(input, start, endExclusive);
		if (!isTagNameAt(input, i, endExclusive, "br")) {
			return false;
		}
		i += 2;
		i = skipWhitespace(input, i, endExclusive);
		if ((i < endExclusive) && (input.charAt(i) == '/')) {
			i++;
			i = skipWhitespace(input, i, endExclusive);
		}
		return i == endExclusive;
	}

	private static boolean isBlockClosingTag(String input, int start, int endExclusive) {
		int i = skipWhitespace(input, start, endExclusive);
		if ((i >= endExclusive) || (input.charAt(i) != '/')) {
			return false;
		}

		i++;
		i = skipWhitespace(input, i, endExclusive);
		int nameStart = i;
		while ((i < endExclusive) && isTagNameChar(input.charAt(i))) {
			i++;
		}
		if (nameStart == i) {
			return false;
		}

		String name = input.substring(nameStart, i).toLowerCase(Locale.ROOT);
		if (!isBlockClosingTagName(name)) {
			return false;
		}

		i = skipWhitespace(input, i, endExclusive);
		return i == endExclusive;
	}

	private static int skipWhitespace(String input, int start, int endExclusive) {
		int i = start;
		while ((i < endExclusive) && Character.isWhitespace(input.charAt(i))) {
			i++;
		}
		return i;
	}

	private static boolean isTagNameAt(String input, int start, int endExclusive, String name) {
		if ((start + name.length()) > endExclusive) {
			return false;
		}
		return input.regionMatches(true, start, name, 0, name.length());
	}

	private static boolean isTagNameChar(char c) {
		return Character.isLetterOrDigit(c);
	}

	private static boolean isBlockClosingTagName(String name) {
		switch (name) {
		case "p":
		case "div":
		case "li":
		case "tr":
		case "ul":
		case "ol":
		case "table":
		case "h1":
		case "h2":
		case "h3":
		case "h4":
		case "h5":
		case "h6":
			return true;
		default:
			return false;
		}
	}

	private static String collapseHorizontalWhitespace(String input) {
		StringBuilder result = new StringBuilder(input.length());
		boolean inHorizontalWhitespace = false;
		for (int i = 0, length = input.length(); i < length; i++) {
			char c = input.charAt(i);
			if (isHorizontalWhitespace(c)) {
				if (!inHorizontalWhitespace) {
					result.append(' ');
					inHorizontalWhitespace = true;
				}
			}
			else {
				result.append(c);
				inHorizontalWhitespace = false;
			}
		}
		return result.toString();
	}

	private static boolean isHorizontalWhitespace(char c) {
		return (c == ' ') || (c == '\t') || (c == '\f') || (c == '\u000B');
	}

	private static String trimSpacesAroundNewlines(String input) {
		StringBuilder result = new StringBuilder(input.length());
		for (int i = 0, length = input.length(); i < length; i++) {
			char c = input.charAt(i);
			if (c != ' ') {
				result.append(c);
				continue;
			}

			boolean beforeNewline = (i > 0) && (input.charAt(i - 1) == '\n');
			boolean afterNewline = ((i + 1) < length) && (input.charAt(i + 1) == '\n');
			if (!beforeNewline && !afterNewline) {
				result.append(c);
			}
		}
		return result.toString();
	}

	private static String collapseExcessNewlines(String input) {
		StringBuilder result = new StringBuilder(input.length());
		int consecutiveNewlines = 0;
		for (int i = 0, length = input.length(); i < length; i++) {
			char c = input.charAt(i);
			if (c == '\n') {
				consecutiveNewlines++;
				if (consecutiveNewlines <= 2) {
					result.append(c);
				}
			}
			else {
				consecutiveNewlines = 0;
				result.append(c);
			}
		}
		return result.toString();
	}

	private static String maskSensitiveCodes(String body) {
		String lower = body.toLowerCase(Locale.ROOT);
		StringBuilder result = new StringBuilder(body);
		int length = body.length();

		for (int i = 0; i < length; i++) {
			for (String prefix : SENSITIVE_CODE_PREFIXES) {
				int prefixLength = prefix.length();
				if ((i + prefixLength) > length) {
					continue;
				}
				if ((i > 0) && isWordChar(lower.charAt(i - 1))) {
					continue;
				}
				if (!lower.startsWith(prefix, i)) {
					continue;
				}
				if (((i + prefixLength) < length) && isWordChar(lower.charAt(i + prefixLength))) {
					continue;
				}

				int cursor = skipWhitespace(lower, i + prefixLength, length);
				if (startsWithWord(lower, cursor, "is")) {
					cursor += 2;
					cursor = skipWhitespace(lower, cursor, length);
					if ((cursor < length) && ((lower.charAt(cursor) == ':') || (lower.charAt(cursor) == '='))) {
						cursor++;
						cursor = skipWhitespace(lower, cursor, length);
					}
				}
				else if ((cursor < length) && ((lower.charAt(cursor) == ':') || (lower.charAt(cursor) == '='))) {
					cursor++;
					cursor = skipWhitespace(lower, cursor, length);
				}

				int digitStart = cursor;
				while ((cursor < length) && Character.isDigit(lower.charAt(cursor))) {
					cursor++;
				}
				int digitLength = cursor - digitStart;
				if ((digitLength < 4) || (digitLength > 10)) {
					continue;
				}
				if ((cursor < length) && isWordChar(lower.charAt(cursor))) {
					continue;
				}

				for (int d = digitStart; d < cursor; d++) {
					result.setCharAt(d, '*');
				}
				i = cursor - 1;
				break;
			}
		}

		return result.toString();
	}

	private static boolean startsWithWord(String lower, int start, String word) {
		int end = start + word.length();
		if (end > lower.length()) {
			return false;
		}
		if (!lower.startsWith(word, start)) {
			return false;
		}
		if ((start > 0) && isWordChar(lower.charAt(start - 1))) {
			return false;
		}
		return (end >= lower.length()) || !isWordChar(lower.charAt(end));
	}

	private static boolean isWordChar(char c) {
		return Character.isLetterOrDigit(c) || (c == '_');
	}

	private static MailLogEntry createSingleEntry(Mail mail, MailDispatchOutcome outcome) {
		return createEntry(List.of(mail), outcome, false);
	}

	private static MailLogEntry createBulkEntry(List<Mail> mails, MailDispatchOutcome outcome) {
		return createEntry(mails, outcome, true);
	}

	private static MailLogEntry createEntry(List<Mail> mails, MailDispatchOutcome outcome, boolean bulk) {
		// TreeSet values are persisted/displayed, so keep deterministic ordering.
		TreeSet<String> toRecipients = new TreeSet<>();
		TreeSet<String> ccRecipients = new TreeSet<>();
		TreeSet<String> bccRecipients = new TreeSet<>();
		TreeSet<String> attachmentNames = new TreeSet<>();
		// HashSet values are metrics-only uniqueness counters.
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

		Mail firstMail = mails.isEmpty() ? null : mails.get(0);
		String firstSubject = (firstMail == null) ? null : Util.processStringValue(firstMail.getSubject());
		String firstBodyExcerpt = (firstMail == null) ? null : bodyExcerpt(firstMail.getBody());

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

	/**
	 * Persist the mail log entry in an isolated transaction.
	 * <p>
	 * Mail dispatch can occur while the caller is inside a business transaction that later rolls back,
	 * or when the caller's persistence context is in an error/rollback-only state after a dispatch failure.
	 * Using a fresh persistence instance keeps mail logging best-effort: failures here do not poison the
	 * caller's unit of work, and the dispatch audit record can still commit independently of caller rollback.
	 */
	private static void persist(MailLogEntry entry) {
		Recorder r = recorder;
		if (r != null) {
			r.record(entry);
			return;
		}

		AbstractHibernatePersistence tempP = null;
		try {
			// Mail logging is best-effort and must not fail the caller's mail flow.
			org.skyve.metadata.user.User currentUser = resolveCurrentUser();
			SuperUser superUser = createMailLogUser(currentUser);
			if (superUser == null) {
				LOGGER.warn("Cannot persist MailLog as customer is indeterminate");
				return;
			}

			tempP = (AbstractHibernatePersistence) AbstractPersistence.newInstance();
			tempP.setUser(superUser);
			tempP.begin();

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
			tempP.commit(false);
		}
		catch (Exception e) {
			LOGGER.error("Failed to persist MailLog entry", e);
			rollbackQuietly(tempP);
		}
		finally {
			closeQuietly(tempP);
		}
	}

	private static @Nullable org.skyve.metadata.user.User resolveCurrentUser() {
		try {
			return CORE.getPersistence().getUser();
		}
		catch (Exception e) {
			LOGGER.debug("Unable to access current persistence user for MailLog", e);
			return null;
		}
	}

	private static void rollbackQuietly(@Nullable AbstractHibernatePersistence persistence) {
		if (persistence == null) {
			return;
		}
		try {
			persistence.rollback();
		}
		catch (Exception rollbackException) {
			LOGGER.error("Failed to rollback MailLog transaction", rollbackException);
		}
	}

	private static void closeQuietly(@Nullable AbstractHibernatePersistence persistence) {
		if (persistence == null) {
			return;
		}
		try {
			persistence.close();
		}
		catch (Exception closeException) {
			LOGGER.error("Failed to close MailLog persistence context", closeException);
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

	/**
	 * MailLogEntry is an internal immutable DTO so we can build the full log payload once,
	 * then hand the same shape to either the test recorder or persistence path.
	 * It keeps MailLogUtil’s collection/normalisation logic separate from DB write concerns
	 * and makes the entry data easy to assert in unit tests.
	 */
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
