package org.skyve.content;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InvalidObjectException;
import java.io.ObjectStreamException;
import java.util.Date;

import org.skyve.content.MimeType;
import org.skyve.util.FileUtil;

public class AttachmentContent extends Content {
	private static final long serialVersionUID = 5929667528318345993L;

	private String attributeName;
	private String contentId;
	private String fileName;
	private MimeType mimeType = MimeType.plain;
	protected Date lastModified;
	private transient InputStream stream;
	private byte[] bytes;

	private AttachmentContent(String bizCustomer, 
								String bizModule, 
								String bizDocument, 
								String bizDataGroupId, 
								String bizUserId,
								String bizId,
								String attributeName,
								String fileName,
								MimeType mimeType) {
		super(bizCustomer, bizModule, bizDocument, bizDataGroupId, bizUserId, bizId);
		if ((attributeName == null) || (attributeName.indexOf('.') >= 0)) {
			throw new IllegalArgumentException("No complex/compound bindings allowed in AttachmentContent - use the correct Document and attribute combination");
		}
		this.attributeName = attributeName;
		this.fileName = fileName;
		if (fileName == null) {
			if (mimeType != null) {
				this.fileName = "content." + mimeType.getStandardFileSuffix();
			}
			else {
				this.fileName = "content";
			}
		}
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
								String attributeName,
								String fileName,
								MimeType mimeType,
								byte[] bytes) {
		this(bizCustomer, bizModule, bizDocument, bizDataGroupId, bizUserId, bizId, attributeName, fileName, mimeType);
		this.bytes = bytes;
	}

	public AttachmentContent(String bizCustomer, 
								String bizModule, 
								String bizDocument, 
								String bizDataGroupId, 
								String bizUserId,
								String bizId,
								String attributeName,
								MimeType mimeType,
								byte[] bytes) {
		this(bizCustomer, bizModule, bizDocument, bizDataGroupId, bizUserId, bizId, attributeName, null, mimeType);
		this.bytes = bytes;
	}

	public AttachmentContent(String bizCustomer, 
								String bizModule, 
								String bizDocument, 
								String bizDataGroupId, 
								String bizUserId,
								String bizId,
								String attributeName,
								String fileName,
								byte[] bytes) {
		this(bizCustomer, bizModule, bizDocument, bizDataGroupId, bizUserId, bizId, attributeName, fileName, (MimeType) null);
		this.bytes = bytes;
	}
	
	public AttachmentContent(String bizCustomer, 
								String bizModule, 
								String bizDocument, 
								String bizDataGroupId, 
								String bizUserId,
								String bizId,
								String attributeName,
								String fileName,
								MimeType mimeType,
								InputStream stream) {
		this(bizCustomer, bizModule, bizDocument, bizDataGroupId, bizUserId, bizId, attributeName, fileName, mimeType);
		this.stream = stream;
	}
	
	public AttachmentContent(String bizCustomer, 
								String bizModule, 
								String bizDocument, 
								String bizDataGroupId, 
								String bizUserId,
								String bizId,
								String attributeName,
								MimeType mimeType,
								InputStream stream) {
		this(bizCustomer, bizModule, bizDocument, bizDataGroupId, bizUserId, bizId, attributeName, null, mimeType);
		this.stream = stream;
	}
	
	public AttachmentContent(String bizCustomer, 
								String bizModule, 
								String bizDocument, 
								String bizDataGroupId, 
								String bizUserId,
								String bizId,
								String attributeName,
								String fileName,
								InputStream stream) {
		this(bizCustomer, bizModule, bizDocument, bizDataGroupId, bizUserId, bizId, attributeName, fileName, (MimeType) null);
		this.stream = stream;
	}

	public final String getAttributeName() {
		return attributeName;
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

	/**
	 * NB This must be closed by the caller.
	 */
	public final InputStream getContentStream() {
		if (stream == null) {
			stream = new ByteArrayInputStream(bytes);
		}
		
		return stream;
	}
	
	public final byte[] getContentBytes() throws IOException {
		
		if (bytes == null) {			
			bytes = FileUtil.getFileBytes(stream);
		}
		
		return bytes;
	}
	
	/**
	 * Ensure that a stream is converted to a self contained byte[] before serializing.
	 * 
	 * @return this
	 * @throws ObjectStreamException
	 */
	private Object writeReplace() throws ObjectStreamException {
		if (stream != null) {
			try {
				getContentBytes();
				stream = null;
			} 
			catch (IOException e) {
				e.printStackTrace();
				throw new InvalidObjectException(e.getLocalizedMessage());
			}
		}
		return this;
	}
}
