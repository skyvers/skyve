package org.skyve.wildcat.content;

import java.io.BufferedInputStream;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Date;

import org.skyve.content.MimeType;

public class AttachmentContent extends Content {
	private static final long serialVersionUID = 5929667528318345993L;

	private String binding;
	private String contentId;
	private String fileName;
	private MimeType mimeType = MimeType.plain;
	protected Date lastModified;
	private InputStream stream;
	private byte[] bytes;

	private AttachmentContent(String bizCustomer, 
								String bizModule, 
								String bizDocument, 
								String bizDataGroupId, 
								String bizUserId,
								String bizId,
								String binding,
								String fileName,
								MimeType mimeType) {
		super(bizCustomer, bizModule, bizDocument, bizDataGroupId, bizUserId, bizId);
		this.binding = binding;
		this.fileName = fileName;
		this.mimeType = mimeType;
		if ((fileName != null) && (mimeType == null)) {
			this.mimeType = MimeType.fromFileName(fileName);
		}
	}

	public AttachmentContent(String bizCustomer, 
								String bizModule, 
								String bizDocument, 
								String bizDataGroupId, 
								String bizUserId,
								String bizId,
								String binding,
								String fileName,
								MimeType mimeType,
								byte[] bytes) {
		this(bizCustomer, bizModule, bizDocument, bizDataGroupId, bizUserId, bizId, binding, fileName, mimeType);
		this.bytes = bytes;
	}

	public AttachmentContent(String bizCustomer, 
								String bizModule, 
								String bizDocument, 
								String bizDataGroupId, 
								String bizUserId,
								String bizId,
								String binding,
								MimeType mimeType,
								byte[] bytes) {
		this(bizCustomer, bizModule, bizDocument, bizDataGroupId, bizUserId, bizId, binding, null, mimeType);
		this.bytes = bytes;
	}

	public AttachmentContent(String bizCustomer, 
								String bizModule, 
								String bizDocument, 
								String bizDataGroupId, 
								String bizUserId,
								String bizId,
								String binding,
								String fileName,
								byte[] bytes) {
		this(bizCustomer, bizModule, bizDocument, bizDataGroupId, bizUserId, bizId, binding, fileName, (MimeType) null);
		this.bytes = bytes;
	}
	
	public AttachmentContent(String bizCustomer, 
								String bizModule, 
								String bizDocument, 
								String bizDataGroupId, 
								String bizUserId,
								String bizId,
								String binding,
								String fileName,
								MimeType mimeType,
								InputStream stream) {
		this(bizCustomer, bizModule, bizDocument, bizDataGroupId, bizUserId, bizId, binding, fileName, mimeType);
		this.stream = stream;
	}
	
	public AttachmentContent(String bizCustomer, 
								String bizModule, 
								String bizDocument, 
								String bizDataGroupId, 
								String bizUserId,
								String bizId,
								String binding,
								MimeType mimeType,
								InputStream stream) {
		this(bizCustomer, bizModule, bizDocument, bizDataGroupId, bizUserId, bizId, binding, null, mimeType);
		this.stream = stream;
	}
	
	public AttachmentContent(String bizCustomer, 
								String bizModule, 
								String bizDocument, 
								String bizDataGroupId, 
								String bizUserId,
								String bizId,
								String binding,
								String fileName,
								InputStream stream) {
		this(bizCustomer, bizModule, bizDocument, bizDataGroupId, bizUserId, bizId, binding, fileName, (MimeType) null);
		this.stream = stream;
	}

	public final String getBinding() {
		return binding;
	}
	
	public final String getContentId() {
		return contentId;
	}
	
	public final void setContentId(String contentId) {
		this.contentId = contentId;
	}

	public final String getFileName() {
		return fileName;
	}

	public final MimeType getMimeType() {
		return mimeType;
	}
	
	public final Date getLastModified() {
		return lastModified;
	}
	
	public final void setLastModified(Date lastModified) {
		this.lastModified = lastModified;
	}

	public final InputStream getContentStream() {
		if (stream == null) {
			stream = new ByteArrayInputStream(bytes);
		}
		
		return stream;
	}
	
	public final byte[] getContentBytes() throws IOException {
		if (bytes == null) {
			try (BufferedInputStream bis = new BufferedInputStream(stream)) {
				try (ByteArrayOutputStream baos = new ByteArrayOutputStream()) {
					byte[] temp = new byte[1024]; // 1K
					int bytesRead = 0;
					while ((bytesRead = bis.read(temp)) > 0) {
						baos.write(temp, 0, bytesRead);
					}
					bytes = baos.toByteArray();
				}
			}
		}
		
		return bytes;
	}
}
