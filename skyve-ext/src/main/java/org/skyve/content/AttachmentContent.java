package org.skyve.content;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.InvalidObjectException;
import java.io.ObjectStreamException;
import java.util.Date;

import org.apache.commons.io.FilenameUtils;
import org.skyve.util.FileUtil;

public class AttachmentContent extends Content {
	private static final long serialVersionUID = 5929667528318345993L;

	private String attributeName;
	private String contentId;
	private String fileName;
	private String contentType;
	protected Date lastModified;
	private transient File file;
	private byte[] bytes;

	private AttachmentContent(String bizCustomer, 
								String bizModule, 
								String bizDocument, 
								String bizDataGroupId, 
								String bizUserId,
								String bizId,
								String attributeName,
								String fileName,
								String contentType) {
		super(bizCustomer, bizModule, bizDocument, bizDataGroupId, bizUserId, bizId);
		if ((attributeName == null) || (attributeName.indexOf('.') >= 0)) {
			throw new IllegalArgumentException("No complex/compound bindings allowed in AttachmentContent - use the correct Document and attribute combination");
		}
		this.attributeName = attributeName;
		this.fileName = fileName;
		if (fileName == null) {
			if (contentType != null) {
				MimeType mimeType = MimeType.fromContentType(contentType);
				if (mimeType != null) {
					this.fileName = "content." + mimeType.getStandardFileSuffix();
				}
				else {
					this.fileName = "content";
				}
			}
			else {
				this.fileName = "content";
			}
		}
		else {
			// remove the path
			this.fileName = FilenameUtils.getName(fileName);
			// remove any invalid chars on all OSs (restricted by windows)
			this.fileName = this.fileName.replaceAll("[\u0001-\u001f<>:\"/\\\\|?*\u007f]+", "").trim();
		}
		this.contentType = contentType;
		if ((this.fileName != null) && (this.contentType == null)) {
			MimeType mimeType = MimeType.fromFileName(fileName);
			if (mimeType != null) {
				this.contentType = mimeType.toString();
			}
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
		this(bizCustomer,
				bizModule,
				bizDocument,
				bizDataGroupId,
				bizUserId,
				bizId,
				attributeName,
				fileName,
				(mimeType == null) ? null : mimeType.toString());
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
		this(bizCustomer,
				bizModule,
				bizDocument,
				bizDataGroupId,
				bizUserId,
				bizId,
				attributeName,
				null,
				(mimeType == null) ? null : mimeType.toString());
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
		this(bizCustomer,
				bizModule,
				bizDocument,
				bizDataGroupId,
				bizUserId,
				bizId,
				attributeName,
				fileName,
				(String) null);
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
								File file) {
		this(bizCustomer,
				bizModule,
				bizDocument,
				bizDataGroupId,
				bizUserId,
				bizId,
				attributeName,
				fileName,
				(mimeType == null) ? null : mimeType.toString());
		this.file = file;
	}
	
	public AttachmentContent(String bizCustomer, 
								String bizModule, 
								String bizDocument, 
								String bizDataGroupId, 
								String bizUserId,
								String bizId,
								String attributeName,
								MimeType mimeType,
								File file) {
		this(bizCustomer,
				bizModule,
				bizDocument,
				bizDataGroupId,
				bizUserId,
				bizId,
				attributeName,
				null,
				(mimeType == null) ? null : mimeType.toString());
		this.file = file;
	}
	
	public AttachmentContent(String bizCustomer, 
								String bizModule, 
								String bizDocument, 
								String bizDataGroupId, 
								String bizUserId,
								String bizId,
								String attributeName,
								String fileName,
								File file) {
		this(bizCustomer,
				bizModule,
				bizDocument,
				bizDataGroupId,
				bizUserId,
				bizId,
				attributeName,
				fileName,
				(String) null);
		this.file = file;
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
		return (contentType == null) ? null : MimeType.fromContentType(contentType);
	}
	
	public final String getContentType() {
		return contentType;
	}
	
	public final void setContentType(String contentType) {
		this.contentType = contentType;
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
		if (file == null) {
			return new ByteArrayInputStream(bytes);
		}
		
		try {
			return new FileInputStream(file);
		}
		catch (@SuppressWarnings("unused") FileNotFoundException e) {
			return new ByteArrayInputStream(new byte[0]);
		}
	}
	
	public final byte[] getContentBytes() throws IOException {
		if (bytes == null) {
			try (InputStream is = getContentStream()) {
				bytes = FileUtil.getFileBytes(is);
			}
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
		if (file != null) {
			try {
				getContentBytes();
				file = null;
			} 
			catch (IOException e) {
				e.printStackTrace();
				throw new InvalidObjectException(e.getLocalizedMessage());
			}
		}
		return this;
	}
}
