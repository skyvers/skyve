package org.skyve.wildcat.util;

import org.skyve.content.MimeType;

/**
 * MailAttachment 
 * 
 * @author RB
 *
 * attachmentFileName: the name of the attachment as it will appear
 * attachment: the byte array
 * attachmentMimeType: the mimetype for the attachment
 *
 */
public class MailAttachment {

	private String attachmentFileName;
	private byte[] attachment;
	private MimeType attachmentMimeType;
	
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

	public MailAttachment(String attachmentFileName, byte[] attachment, MimeType attachmentMimeType) {
		super();
		this.attachmentFileName = attachmentFileName;
		this.attachment = attachment;
		this.attachmentMimeType = attachmentMimeType;
	}
}
