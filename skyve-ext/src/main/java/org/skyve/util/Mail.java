package org.skyve.util;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;

import org.skyve.content.MimeType;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Encapsulates and builds an email message.
 * The default content type is HTML.
 * @author mike
 */
public class Mail implements Serializable {
	private static final long serialVersionUID = -1986344773711564000L;

	private Set<String> recipientEmailAddresses = new TreeSet<>();
	private Set<String> ccEmailAddresses = new TreeSet<>();
	private Set<String> bccEmailAddresses = new TreeSet<>();
	private String senderEmailAddress;
	private String subject;
	private String body;
	private MimeType contentType = MimeType.html;
	private List<MailAttachment> attachments = new ArrayList<>();
	private Map<String, String> headers = new TreeMap<>();
	
	/**
	 * Add recipients
	 * @param recipientEmailAddresses
	 * @return this
	 */
	public @Nonnull Mail addTo(@SuppressWarnings("hiding") @Nullable Collection<String> recipientEmailAddresses) {
		if (recipientEmailAddresses != null) {
			this.recipientEmailAddresses.addAll(recipientEmailAddresses);
		}
		return this;
	}
	
	/**
	 * Add recipient
	 * @param recipientEmailAddress
	 * @return this
	 */
	public @Nonnull Mail addTo(@Nullable String recipientEmailAddress) {
		if (recipientEmailAddress != null) {
			recipientEmailAddresses.add(recipientEmailAddress);
		}
		return this;
	}
	
	/**
	 * Add recipients
	 * @param recipientEmailAddresses
	 * @return this
	 */
	public @Nonnull Mail addTo(@SuppressWarnings("hiding") @Nullable String... recipientEmailAddresses) {
		if (recipientEmailAddresses != null) {
			for (String recipientEmailAddress : recipientEmailAddresses) {
				addTo(recipientEmailAddress);
			}
		}
		return this;
	}
	
	public @Nonnull Set<String> getRecipientEmailAddresses() {
		return recipientEmailAddresses;
	}
	
	/**
	 * Add carbon copies
	 * @param ccEmailAddress
	 * @return this
	 */
	public @Nonnull Mail addCC(@SuppressWarnings("hiding") @Nullable Collection<String> ccEmailAddresses) {
		if (ccEmailAddresses != null) {
			this.ccEmailAddresses.addAll(ccEmailAddresses);
		}
		return this;
	}
	
	/**
	 * Add carbon copy
	 * @param ccEmailAddresses
	 * @return this
	 */
	public @Nonnull Mail addCC(@Nullable String ccEmailAddress) {
		if (ccEmailAddress != null) {
			ccEmailAddresses.add(ccEmailAddress);
		}
		return this;
	}
	
	/**
	 * Add carbon copies
	 * @param ccEmailAddresses
	 * @return this
	 */
	public @Nonnull Mail addCC(@SuppressWarnings("hiding") @Nullable String... ccEmailAddresses) {
		if (ccEmailAddresses != null) {
			for (String ccEmailAddress : ccEmailAddresses) {
				addCC(ccEmailAddress);
			}
		}
		return this;
	}

	public @Nonnull Set<String> getCcEmailAddresses() {
		return ccEmailAddresses;
	}
	
	/**
	 * Add Blind Carbon Copies
	 * @param bccEmailAddresses
	 * @return this
	 */
	public @Nonnull Mail addBCC(@SuppressWarnings("hiding") @Nullable Collection<String> bccEmailAddresses) {
		if (bccEmailAddresses != null) {
			this.bccEmailAddresses.addAll(bccEmailAddresses);
		}
		return this;
	}
	
	/**
	 * Add Blind Carbon Copy
	 * @param bccEmailAddress
	 * @return this
	 */
	public @Nonnull Mail addBCC(@Nullable String bccEmailAddress) {
		if (bccEmailAddress != null) {
			bccEmailAddresses.add(bccEmailAddress);
		}
		return this;
	}
	
	/**
	 * Add Blind Carbon Copies
	 * @param bccEmailAddresses
	 * @return this
	 */
	public @Nonnull Mail addBCC(@SuppressWarnings("hiding") @Nullable String... bccEmailAddresses) {
		if (bccEmailAddresses != null) {
			for (String bccEmailAddress : bccEmailAddresses) {
				addBCC(bccEmailAddress);
			}
		}
		return this;
	}

	public @Nonnull Set<String> getBccEmailAddresses() {
		return bccEmailAddresses;
	}
	
	/**
	 * Set the sender
	 * @param senderEmailAddress
	 * @return this
	 */
	public @Nonnull Mail from(@SuppressWarnings("hiding") @Nullable String senderEmailAddress) {
		this.senderEmailAddress = senderEmailAddress;
		return this;
	}

	public @Nullable String getSenderEmailAddress() {
		return senderEmailAddress;
	}
	
	/**
	 * Set the subject
	 * @param subject
	 * @return this
	 */
	public @Nonnull Mail subject(@SuppressWarnings("hiding") @Nullable String subject) {
		this.subject = subject;
		return this;
	}
	
	public @Nullable String getSubject() {
		return subject;
	}

	/**
	 * Set the body
	 * @param body
	 * @return this
	 */
	public @Nonnull Mail body(@SuppressWarnings("hiding") @Nullable String body) {
		this.body = body;
		return this;
	}

	public @Nullable String getBody() {
		return body;
	}
	
	/**
	 * Set the content type
	 * @param contentType
	 * @return this
	 */
	public @Nonnull Mail contentType(@SuppressWarnings("hiding") @Nonnull MimeType contentType) {
		this.contentType = contentType;
		return this;
	}
	
	/**
	 * Set the mail to be text/plain
	 * @return this
	 */
	public @Nonnull Mail textPlain() {
		this.contentType = MimeType.plain;
		return this;
	}
	
	/**
	 * Set the mail to be html - this is the default.
	 * @return this
	 */
	public @Nonnull Mail html() {
		this.contentType = MimeType.html;
		return this;
	}
	
	public @Nonnull MimeType getContentType() {
		return contentType;
	}
	
	/**
	 * Add attachments.
	 * @param attachments
	 * @return this
	 */
	public @Nonnull Mail attach(@SuppressWarnings("hiding") @Nullable Collection<MailAttachment> attachments) {
		if (attachments != null) {
			this.attachments.addAll(attachments);
		}
		return this;
	}
	
	/**
	 * Add an attachment
	 * @param attachment
	 * @return this
	 */
	public @Nonnull Mail attach(@Nullable MailAttachment attachment) {
		if (attachment != null) {
			attachments.add(attachment);
		}
		return this;
	}
	
	/**
	 * Add attachments
	 * @param attachments
	 * @return this
	 */
	public @Nonnull Mail attach(@SuppressWarnings("hiding") @Nullable MailAttachment... attachments) {
		if (attachments != null) {
			for (MailAttachment attachment : attachments) {
				attach(attachment);
			}
		}
		return this;
	}
	
	/**
	 * Add attachment
	 * @param attachmentFileName
	 * @param attachment
	 * @param attachmentMimeType
	 * @return this
	 */
	public @Nonnull Mail attach(@Nullable String attachmentFileName, @Nullable byte[] attachment, @Nonnull MimeType attachmentMimeType) {
		return attach(new MailAttachment(attachmentFileName, attachment, attachmentMimeType));
	}

	public @Nonnull List<MailAttachment> getAttachments() {
		return attachments;
	}
	
	/**
	 * Add message headers
	 * @param headers
	 * @return this
	 */
	public @Nonnull Mail header(@SuppressWarnings("hiding") @Nullable Map<String, String> headers) {
		if (headers != null) {
			this.headers.putAll(headers);
		}
		return this;
	}
	
	/**
	 * Add a message header
	 * @param headerName
	 * @param headerValue
	 * @return this
	 */
	public @Nonnull Mail header(@Nonnull String headerName, @Nonnull String headerValue) {
		headers.put(headerName, headerValue);
		return this;
	}
	
	/**
	 * Add the "X-Unsent" header to allow email clients to open this email 
	 * from the file system as unsent - ready to send.
	 * @return this
	 */
	public @Nonnull Mail unsent() {
		header("X-Unsent", "1");
		return this;
	}
	
	public @Nonnull Map<String, String> getHeaders() {
		return headers;
	}
}
