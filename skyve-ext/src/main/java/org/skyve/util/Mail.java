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
	public Mail addTo(@SuppressWarnings("hiding") Collection<String> recipientEmailAddresses) {
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
	public Mail addTo(String recipientEmailAddress) {
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
	public Mail addTo(@SuppressWarnings("hiding") String... recipientEmailAddresses) {
		if (recipientEmailAddresses != null) {
			for (String recipientEmailAddress : recipientEmailAddresses) {
				addTo(recipientEmailAddress);
			}
		}
		return this;
	}
	
	public Set<String> getRecipientEmailAddresses() {
		return recipientEmailAddresses;
	}
	
	/**
	 * Add carbon copies
	 * @param ccEmailAddress
	 * @return this
	 */
	public Mail addCC(@SuppressWarnings("hiding") Collection<String> ccEmailAddresses) {
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
	public Mail addCC(String ccEmailAddress) {
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
	public Mail addCC(@SuppressWarnings("hiding") String... ccEmailAddresses) {
		if (ccEmailAddresses != null) {
			for (String ccEmailAddress : ccEmailAddresses) {
				addCC(ccEmailAddress);
			}
		}
		return this;
	}

	public Set<String> getCcEmailAddresses() {
		return ccEmailAddresses;
	}
	
	/**
	 * Add Blind Carbon Copies
	 * @param bccEmailAddresses
	 * @return this
	 */
	public Mail addBCC(@SuppressWarnings("hiding") Collection<String> bccEmailAddresses) {
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
	public Mail addBCC(String bccEmailAddress) {
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
	public Mail addBCC(@SuppressWarnings("hiding") String... bccEmailAddresses) {
		if (bccEmailAddresses != null) {
			for (String bccEmailAddress : bccEmailAddresses) {
				addBCC(bccEmailAddress);
			}
		}
		return this;
	}

	public Set<String> getBccEmailAddresses() {
		return bccEmailAddresses;
	}
	
	/**
	 * Set the sender
	 * @param senderEmailAddress
	 * @return this
	 */
	public Mail from(@SuppressWarnings("hiding") String senderEmailAddress) {
		this.senderEmailAddress = senderEmailAddress;
		return this;
	}

	public String getSenderEmailAddress() {
		return senderEmailAddress;
	}
	
	/**
	 * Set the subject
	 * @param subject
	 * @return this
	 */
	public Mail subject(@SuppressWarnings("hiding") String subject) {
		this.subject = subject;
		return this;
	}
	
	public String getSubject() {
		return subject;
	}

	/**
	 * Set the body
	 * @param body
	 * @return this
	 */
	public Mail body(@SuppressWarnings("hiding") String body) {
		this.body = body;
		return this;
	}

	public String getBody() {
		return body;
	}
	
	/**
	 * Set the content type
	 * @param contentType
	 * @return this
	 */
	public Mail contentType(@SuppressWarnings("hiding") MimeType contentType) {
		this.contentType = contentType;
		return this;
	}
	
	/**
	 * Set the mail to be text/plain
	 * @return this
	 */
	public Mail textPlain() {
		this.contentType = MimeType.plain;
		return this;
	}
	
	/**
	 * Set the mail to be html - this is the default.
	 * @return this
	 */
	public Mail html() {
		this.contentType = MimeType.html;
		return this;
	}
	
	public MimeType getContentType() {
		return contentType;
	}
	
	/**
	 * Add attachments.
	 * @param attachments
	 * @return this
	 */
	public Mail attach(@SuppressWarnings("hiding") Collection<MailAttachment> attachments) {
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
	public Mail attach(MailAttachment attachment) {
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
	public Mail attach(@SuppressWarnings("hiding") MailAttachment... attachments) {
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
	public Mail attach(String attachmentFileName, byte[] attachment, MimeType attachmentMimeType) {
		return attach(new MailAttachment(attachmentFileName, attachment, attachmentMimeType));
	}

	public List<MailAttachment> getAttachments() {
		return attachments;
	}
	
	/**
	 * Add message headers
	 * @param headers
	 * @return this
	 */
	public Mail header(@SuppressWarnings("hiding") Map<String, String> headers) {
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
	public Mail header(String headerName, String headerValue) {
		headers.put(headerName, headerValue);
		return this;
	}
	
	/**
	 * Add the "X-Unsent" header to allow email clients to open this email 
	 * from the file system as unsent - ready to send.
	 * @return this
	 */
	public Mail unsent() {
		header("X-Unsent", "1");
		return this;
	}
	
	public Map<String, String> getHeaders() {
		return headers;
	}
}
