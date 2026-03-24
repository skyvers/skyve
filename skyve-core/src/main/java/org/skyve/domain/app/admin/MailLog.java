package org.skyve.domain.app.admin;

import org.skyve.CORE;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.app.AppConstants;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.types.Timestamp;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;

import jakarta.annotation.Nonnull;

public interface MailLog extends PersistentBean {

	public static @Nonnull MailLog newInstance() {
		return newInstance(CORE.getUser());
	}

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

	Timestamp getTimestamp();
	String getToRecipients();
	String getCcRecipients();
	String getBccRecipients();
	String getSubject();
	String getBodyExcerpt();
	String getAttachmentFileNames();
	String getDispatchStatus();
	String getProvider();
	String getProviderMessageId();
	String getRelayStatus();
	String getRelayDetail();
	String getErrorDetail();
	Boolean getIsBulk();
	Long getMailCount();
	Long getRecipientCount();
	Boolean getHasMultipleSubjects();
	Long getSubjectVariantCount();
	Boolean getHasMultipleBodies();
	Long getBodyVariantCount();

	void setTimestamp(Timestamp timestamp);
	void setToRecipients(String toRecipients);
	void setCcRecipients(String ccRecipients);
	void setBccRecipients(String bccRecipients);
	void setSubject(String subject);
	void setBodyExcerpt(String bodyExcerpt);
	void setAttachmentFileNames(String attachmentFileNames);
	void setDispatchStatus(String dispatchStatus);
	void setProvider(String provider);
	void setProviderMessageId(String providerMessageId);
	void setRelayStatus(String relayStatus);
	void setRelayDetail(String relayDetail);
	void setErrorDetail(String errorDetail);
	void setIsBulk(Boolean isBulk);
	void setMailCount(Long mailCount);
	void setRecipientCount(Long recipientCount);
	void setHasMultipleSubjects(Boolean hasMultipleSubjects);
	void setSubjectVariantCount(Long subjectVariantCount);
	void setHasMultipleBodies(Boolean hasMultipleBodies);
	void setBodyVariantCount(Long bodyVariantCount);
}
