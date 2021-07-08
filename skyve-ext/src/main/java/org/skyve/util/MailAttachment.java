package org.skyve.util;

import java.io.Serializable;

import org.skyve.EXT;
import org.skyve.content.AttachmentContent;
import org.skyve.content.ContentManager;
import org.skyve.content.MimeType;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.messages.SkyveException;

/**
 * MailAttachment
 * 
 * @author RB
 * 
 * attachmentFileName: the name of the attachment as it will appear
 * attachment: the byte array 
 * attachmentMimeType: the mimetype for the attachment
 */
public class MailAttachment implements Serializable {
	private static final long serialVersionUID = 8103370634731869625L;

	private String attachmentFileName;
	private byte[] attachment;
	private MimeType attachmentMimeType;

	/**
	 * Default constructor
	 */
	public MailAttachment() {
		// nothing to see here
	}

	/**
	 * Simple constructor
	 * 
	 * @param attachmentFileName
	 * @param attachment
	 * @param attachmentMimeType
	 */
	public MailAttachment(String attachmentFileName, byte[] attachment, MimeType attachmentMimeType) {
		this.attachmentFileName = attachmentFileName;
		this.attachment = attachment;
		this.attachmentMimeType = attachmentMimeType;
	}

	/**
	 * Content constructor
	 * 
	 * @param contentId	The contentId of the attachment content to add as an attachment.
	 */
	public MailAttachment(String contentId) {
		try (ContentManager cm = EXT.newContentManager()) {
			AttachmentContent content = cm.getAttachment(contentId);
			if (content == null) {
				throw new DomainException("The content for the attachment can't be retrieved - re-attach the content and try again.");
			}
			this.attachmentFileName = content.getFileName();
			this.attachment = content.getContentBytes();
			this.attachmentMimeType = content.getMimeType();
		}
		catch (Exception e) {
			if (e instanceof SkyveException) {
				throw (SkyveException) e;
			}
			throw new DomainException("Could not get the content to attach", e);
		}
	}

	/**
	 * Named Content constructor
	 * 
	 * @param attachmentFileName
	 * @param contentId	The contentId of the attachment content to add as an attachment.
	 */
	public MailAttachment(String attachmentFileName, String contentId) {
		this(contentId);
		this.attachmentFileName = attachmentFileName;
	}
	
	public String getAttachmentFileName() {
		return attachmentFileName;
	}

	public void setAttachmentFileName(String attachmentFileName) {
		this.attachmentFileName = attachmentFileName;
	}

	public byte[] getAttachment() {
		return attachment;
	}

	public void setAttachment(byte[] attachment) {
		this.attachment = attachment;
	}

	public MimeType getAttachmentMimeType() {
		return attachmentMimeType;
	}

	public void setAttachmentMimeType(MimeType attachmentMimeType) {
		this.attachmentMimeType = attachmentMimeType;
	}
}
