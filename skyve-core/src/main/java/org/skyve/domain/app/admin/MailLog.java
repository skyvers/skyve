package org.skyve.domain.app.admin;

import org.skyve.CORE;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.app.AppConstants;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.types.Timestamp;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Persistent record of an outbound mail dispatch attempt.
 */
public interface MailLog extends PersistentBean {
	/** The name of the content attribute holding the zip of the attachments sent. */
	public static final String ATTACHMENTS_ATTRIBUTE_NAME = "attachments";

	/**
	 * Create a {@link MailLog} instance using the current user context.
	 */
	public static @Nonnull MailLog newInstance() {
		return newInstance(CORE.getUser());
	}

	/**
	 * Create a {@link MailLog} instance for the supplied user context.
	 */
	public static @Nonnull MailLog newInstance(@Nonnull org.skyve.metadata.user.User user) {
		try {
			Customer customer = user.getCustomer();
			org.skyve.metadata.module.Module module = customer.getModule(AppConstants.ADMIN_MODULE_NAME);
			Document document = module.getDocument(customer, AppConstants.MAIL_LOG_DOCUMENT_NAME);

			return document.newInstance(user);
		}
		catch (Exception e) {
			throw new DomainException("Could not instantiate a new MailLog", e);
		}
	}

	@Nullable Timestamp getTimestamp();
	@Nullable Timestamp getArchiveTimestamp();
	@Nullable String getToRecipients();
	@Nullable String getCcRecipients();
	@Nullable String getBccRecipients();
	@Nullable String getArchiveFilename();
	@Nullable String getSubject();
	@Nullable String getBodyExcerpt();
	@Nullable String getAttachmentFileNames();
	@Nullable String getAttachments();
	@Nullable String getDispatchStatus();
	@Nullable String getProvider();
	@Nullable String getProviderMessageId();
	@Nullable String getRelayStatus();
	@Nullable String getRelayDetail();
	@Nullable String getErrorDetail();
	@Nullable Boolean getIsBulk();
	@Nullable Long getMailCount();
	@Nullable Long getRecipientCount();
	@Nullable Boolean getHasMultipleSubjects();
	@Nullable Long getSubjectVariantCount();
	@Nullable Boolean getHasMultipleBodies();
	@Nullable Long getBodyVariantCount();

	void setTimestamp(@Nullable Timestamp timestamp);
	void setArchiveTimestamp(@Nullable Timestamp archiveTimestamp);
	void setToRecipients(@Nullable String toRecipients);
	void setCcRecipients(@Nullable String ccRecipients);
	void setBccRecipients(@Nullable String bccRecipients);
	void setArchiveFilename(@Nullable String archiveFilename);
	void setSubject(@Nullable String subject);
	void setBodyExcerpt(@Nullable String bodyExcerpt);
	void setAttachmentFileNames(@Nullable String attachmentFileNames);
	void setAttachments(@Nullable String attachments);
	void setDispatchStatus(@Nullable String dispatchStatus);
	void setProvider(@Nullable String provider);
	void setProviderMessageId(@Nullable String providerMessageId);
	void setRelayStatus(@Nullable String relayStatus);
	void setRelayDetail(@Nullable String relayDetail);
	void setErrorDetail(@Nullable String errorDetail);
	void setIsBulk(@Nullable Boolean isBulk);
	void setMailCount(@Nullable Long mailCount);
	void setRecipientCount(@Nullable Long recipientCount);
	void setHasMultipleSubjects(@Nullable Boolean hasMultipleSubjects);
	void setSubjectVariantCount(@Nullable Long subjectVariantCount);
	void setHasMultipleBodies(@Nullable Boolean hasMultipleBodies);
	void setBodyVariantCount(@Nullable Long bodyVariantCount);
}
