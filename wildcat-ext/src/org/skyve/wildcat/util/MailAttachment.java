package org.skyve.wildcat.util;

import java.io.ByteArrayOutputStream;
import java.util.Map;

import net.sf.jasperreports.engine.JasperPrint;

import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.content.MimeType;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.user.User;
import org.skyve.persistence.Persistence;
import org.skyve.report.ReportFormat;
import org.skyve.wildcat.content.AttachmentContent;
import org.skyve.wildcat.content.ContentManager;

/**
 * MailAttachment
 * 
 * @author RB
 * 
 *         attachmentFileName: the name of the attachment as it will appear
 *         attachment: the byte array attachmentMimeType: the mimetype for the
 *         attachment
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

	/**
	 * Simple constructor
	 * 
	 * @param attachmentFileName
	 * @param attachment
	 * @param attachmentMimeType
	 */
	public MailAttachment(String attachmentFileName, byte[] attachment, MimeType attachmentMimeType) {
		super();
		this.attachmentFileName = attachmentFileName;
		this.attachment = attachment;
		this.attachmentMimeType = attachmentMimeType;
	}

	/**
	 * Constructor to create mail attachment from bean content attribute
	 * 
	 * @param beanContent
	 * @param attachmentName
	 * @throws Exception
	 */
	public MailAttachment(String beanContent, String attachmentName) throws Exception {
		super();
		try (ContentManager cm = EXT.newContentManager()) {
			if (beanContent != null) {
				AttachmentContent content = cm.get(beanContent);
				if (content == null) {
					throw new Exception("The content for the attachment can't be retrieved - re-attach the content and try again.");
				}
				byte[] fileBytes = content.getContentBytes();

				this.attachmentFileName = attachmentName;
				this.attachment = fileBytes;
				this.attachmentMimeType = content.getMimeType();
			}
		}
	}

	/**
	 * Constructor to create mail attachment from a report
	 * 
	 * @param reportModuleName
	 * @param reportDocumentName
	 * @param reportName
	 * @param parameters
	 */
	public MailAttachment(String reportModuleName, String reportDocumentName, String reportName, Map<String, Object> parameters) throws Exception {

		Persistence persistence = CORE.getPersistence();
		User user = persistence.getUser();
		Customer customer = user.getCustomer();
		Document invoiceDocument = customer.getModule(reportModuleName).getDocument(customer, reportDocumentName);

		ByteArrayOutputStream out = new ByteArrayOutputStream();
		JasperPrint jp = EXT.runSQLReport(user, invoiceDocument, reportName, parameters, ReportFormat.pdf, out);
		byte[] reportBytes = out.toByteArray();
		this.attachment = reportBytes;

		this.attachmentFileName = reportName;
		this.attachmentMimeType = MimeType.pdf;
	}
}
