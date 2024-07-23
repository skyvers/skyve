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

/**
 * Represents an attachment stored in a content document attribute.
 * The contentId is the unique identifier used to get and manipulate this content.
 * @author mike
 */
public class AttachmentContent extends Content {
	private static final long serialVersionUID = 5929667528318345993L;

	private String attributeName;
	private String contentId;
	private String fileName;
	private String contentType;
	private Date lastModified;
	private transient File file;
	private byte[] bytes;
	private String markup; // Editable markup, usually SVG on a raster image

	private AttachmentContent(String bizCustomer, 
								String bizModule, 
								String bizDocument, 
								String bizDataGroupId, 
								String bizUserId,
								String bizId,
								String attributeName,
								String fileName,
								String contentType,
								String markup) {
		super(bizCustomer, bizModule, bizDocument, bizDataGroupId, bizUserId, bizId);
		if ((attributeName == null) || (attributeName.indexOf('.') >= 0)) {
			throw new IllegalArgumentException("No complex/compound bindings allowed in AttachmentContent - use the correct Document and attribute combination");
		}
		this.attributeName = attributeName;
		this.fileName = fileName;
		if (this.fileName == null) {
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
			this.fileName = FilenameUtils.getName(this.fileName);
			// remove any invalid chars on all OSs (restricted by windows)
			this.fileName = this.fileName.replaceAll("[\u0001-\u001f<>:\"/\\\\|?*\u007f]+", "").trim();
		}
		this.contentType = contentType;
		if ((this.fileName != null) && (this.contentType == null)) {
			MimeType mimeType = MimeType.fromFileName(this.fileName);
			if (mimeType != null) {
				this.contentType = mimeType.toString();
			}
		}
		this.markup = markup;
	}

	/**
	 * Copy constructor to use when updating metadata through ContentManager.update() with a remote call - EJB, JDBC, REST.
	 * 
	 * @param forUpdate	The attachment content to copy.
	 */
	public AttachmentContent(AttachmentContent forUpdate) {
		this(forUpdate.bizCustomer,
				forUpdate.bizModule,
				forUpdate.bizDocument,
				forUpdate.bizDataGroupId,
				forUpdate.bizUserId,
				forUpdate.bizId,
				forUpdate.attributeName,
				forUpdate.fileName,
				forUpdate.contentType,
				forUpdate.markup);
		this.bytes = new byte[0];
		this.contentId = forUpdate.contentId;
	}
	
	/**
	 * Bytes filename mime type constructor.
	 * 
	 * @param bizCustomer
	 * @param bizModule
	 * @param bizDocument
	 * @param bizDataGroupId
	 * @param bizUserId
	 * @param bizId
	 * @param attributeName
	 * @param fileName
	 * @param mimeType
	 * @param bytes
	 */
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
				(mimeType == null) ? null : mimeType.toString(),
				null);
		this.bytes = bytes;
	}

	/**
	 * Bytes filename mime type markup constructor.
	 * 
	 * @param bizCustomer
	 * @param bizModule
	 * @param bizDocument
	 * @param bizDataGroupId
	 * @param bizUserId
	 * @param bizId
	 * @param attributeName
	 * @param fileName
	 * @param mimeType
	 * @param bytes
	 * @param markup
	 */
	public AttachmentContent(String bizCustomer, 
								String bizModule, 
								String bizDocument, 
								String bizDataGroupId, 
								String bizUserId,
								String bizId,
								String attributeName,
								String fileName,
								MimeType mimeType,
								byte[] bytes,
								String markup) {
		this(bizCustomer,
				bizModule,
				bizDocument,
				bizDataGroupId,
				bizUserId,
				bizId,
				attributeName,
				fileName,
				(mimeType == null) ? null : mimeType.toString(),
				markup);
		this.bytes = bytes;
	}

	/**
	 * Bytes mime type constructor.
	 * 
	 * @param bizCustomer
	 * @param bizModule
	 * @param bizDocument
	 * @param bizDataGroupId
	 * @param bizUserId
	 * @param bizId
	 * @param attributeName
	 * @param mimeType
	 * @param bytes
	 */
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
				(mimeType == null) ? null : mimeType.toString(),
				null);
		this.bytes = bytes;
	}

	/**
	 * Bytes mime type markup constructor.
	 * 
	 * @param bizCustomer
	 * @param bizModule
	 * @param bizDocument
	 * @param bizDataGroupId
	 * @param bizUserId
	 * @param bizId
	 * @param attributeName
	 * @param mimeType
	 * @param bytes
	 * @param markup
	 */
	public AttachmentContent(String bizCustomer, 
								String bizModule, 
								String bizDocument, 
								String bizDataGroupId, 
								String bizUserId,
								String bizId,
								String attributeName,
								MimeType mimeType,
								byte[] bytes,
								String markup) {
		this(bizCustomer,
				bizModule,
				bizDocument,
				bizDataGroupId,
				bizUserId,
				bizId,
				attributeName,
				null,
				(mimeType == null) ? null : mimeType.toString(),
				markup);
		this.bytes = bytes;
	}

	/**
	 * Bytes filename constructor.
	 * 
	 * @param bizCustomer
	 * @param bizModule
	 * @param bizDocument
	 * @param bizDataGroupId
	 * @param bizUserId
	 * @param bizId
	 * @param attributeName
	 * @param fileName
	 * @param bytes
	 */
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
				(String) null,
				null);
		this.bytes = bytes;
	}
	
	/**
	 * Bytes filename markup constructor.
	 * 
	 * @param bizCustomer
	 * @param bizModule
	 * @param bizDocument
	 * @param bizDataGroupId
	 * @param bizUserId
	 * @param bizId
	 * @param attributeName
	 * @param fileName
	 * @param bytes
	 * @param markup
	 */
	public AttachmentContent(String bizCustomer, 
								String bizModule, 
								String bizDocument, 
								String bizDataGroupId, 
								String bizUserId,
								String bizId,
								String attributeName,
								String fileName,
								byte[] bytes,
								String markup) {
		this(bizCustomer,
				bizModule,
				bizDocument,
				bizDataGroupId,
				bizUserId,
				bizId,
				attributeName,
				fileName,
				(String) null,
				markup);
		this.bytes = bytes;
	}

	/**
	 * File filename mime type constructor.
	 * 
	 * @param bizCustomer
	 * @param bizModule
	 * @param bizDocument
	 * @param bizDataGroupId
	 * @param bizUserId
	 * @param bizId
	 * @param attributeName
	 * @param fileName
	 * @param mimeType
	 * @param file
	 */
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
				(mimeType == null) ? null : mimeType.toString(),
				null);
		this.file = file;
	}
	
	/**
	 * File filename mime type markup constructor.
	 * 
	 * @param bizCustomer
	 * @param bizModule
	 * @param bizDocument
	 * @param bizDataGroupId
	 * @param bizUserId
	 * @param bizId
	 * @param attributeName
	 * @param fileName
	 * @param mimeType
	 * @param file
	 * @param markup
	 */
	public AttachmentContent(String bizCustomer, 
								String bizModule, 
								String bizDocument, 
								String bizDataGroupId, 
								String bizUserId,
								String bizId,
								String attributeName,
								String fileName,
								MimeType mimeType,
								File file,
								String markup) {
		this(bizCustomer,
				bizModule,
				bizDocument,
				bizDataGroupId,
				bizUserId,
				bizId,
				attributeName,
				fileName,
				(mimeType == null) ? null : mimeType.toString(),
				markup);
		this.file = file;
	}

	/**
	 * File mime type constructor.
	 * 
	 * @param bizCustomer
	 * @param bizModule
	 * @param bizDocument
	 * @param bizDataGroupId
	 * @param bizUserId
	 * @param bizId
	 * @param attributeName
	 * @param mimeType
	 * @param file
	 */
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
				(mimeType == null) ? null : mimeType.toString(),
				null);
		this.file = file;
	}
	
	/**
	 * File mime type markup constructor.
	 * 
	 * @param bizCustomer
	 * @param bizModule
	 * @param bizDocument
	 * @param bizDataGroupId
	 * @param bizUserId
	 * @param bizId
	 * @param attributeName
	 * @param mimeType
	 * @param file
	 * @param markup
	 */
	public AttachmentContent(String bizCustomer, 
								String bizModule, 
								String bizDocument, 
								String bizDataGroupId, 
								String bizUserId,
								String bizId,
								String attributeName,
								MimeType mimeType,
								File file,
								String markup) {
		this(bizCustomer,
				bizModule,
				bizDocument,
				bizDataGroupId,
				bizUserId,
				bizId,
				attributeName,
				null,
				(mimeType == null) ? null : mimeType.toString(),
				markup);
		this.file = file;
	}

	/**
	 * File filename constructor.
	 * 
	 * @param bizCustomer
	 * @param bizModule
	 * @param bizDocument
	 * @param bizDataGroupId
	 * @param bizUserId
	 * @param bizId
	 * @param attributeName
	 * @param fileName
	 * @param file
	 */
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
				(String) null,
				null);
		this.file = file;
	}

	/**
	 * File filename markup constructor.
	 * 
	 * @param bizCustomer
	 * @param bizModule
	 * @param bizDocument
	 * @param bizDataGroupId
	 * @param bizUserId
	 * @param bizId
	 * @param attributeName
	 * @param fileName
	 * @param file
	 * @param markup
	 */
	public AttachmentContent(String bizCustomer, 
								String bizModule, 
								String bizDocument, 
								String bizDataGroupId, 
								String bizUserId,
								String bizId,
								String attributeName,
								String fileName,
								File file,
								String markup) {
		this(bizCustomer,
				bizModule,
				bizDocument,
				bizDataGroupId,
				bizUserId,
				bizId,
				attributeName,
				fileName,
				(String) null,
				markup);
		this.file = file;
	}

	/**
	 * The simple (not compound) attribute name for this attachment.
	 */
	public final String getAttributeName() {
		return attributeName;
	}
	public final void setAttributeName(String attributeName) {
		this.attributeName = attributeName;
	}
	
	/**
	 * The contentId unique identifier within the content repository.
	 * @return	a unique identifier or null if this has not yet been put in the content repository.
	 */
	public final String getContentId() {
		return contentId;
	}
	
	public final void setContentId(String contentId) {
		this.contentId = contentId;
	}

	/**
	 * Name of the originating file this content came from.
	 * @return
	 */
	public final String getFileName() {
		return fileName;
	}
	public final void setFileName(String fileName) {
		this.fileName = fileName;
	}
	
	/**
	 * The mime type of this content
	 * @return
	 */
	public final MimeType getMimeType() {
		return (contentType == null) ? null : MimeType.fromContentType(contentType);
	}

	/**
	 * The content type of this content - usually matches mime type but may be a variant.
	 * @return
	 */
	public final String getContentType() {
		return contentType;
	}
	
	public final void setContentType(String contentType) {
		this.contentType = contentType;
	}
	
	/**
	 * The date/time of last modification.
	 * @return
	 */
	public final Date getLastModified() {
		return lastModified;
	}
	
	public final void setLastModified(Date lastModified) {
		this.lastModified = lastModified;
	}
	
	/**
	 * Markup to be overlaid on the content as edits - eg SVG on raster image content.
	 * @return	The markup.
	 */
	public String getMarkup() {
		return markup;
	}

	public void setMarkup(String markup) {
		this.markup = markup;
	}

	/**
	 * The content stream.
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
	
	/**
	 * The content bytes.
	 * @return
	 * @throws IOException
	 */
	public final byte[] getContentBytes() throws IOException {
		if (bytes == null) {
			try (InputStream is = getContentStream()) {
				bytes = FileUtil.bytes(is);
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
	
	// Add mutability to Content interface
	
	public final void setBizCustomer(String bizCustomer) {
		this.bizCustomer = bizCustomer;
	}

	public final void setBizModule(String bizModule) {
		this.bizModule = bizModule;
	}

	public final void setBizDocument(String bizDocument) {
		this.bizDocument = bizDocument;
	}
	
	public final void setBizDataGroupId(String bizDataGroupId) {
		this.bizDataGroupId = bizDataGroupId;
	}
	
	public final void setBizUserId(String bizUserId) {
		this.bizUserId = bizUserId;
	}
}
