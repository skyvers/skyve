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

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

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

	public AttachmentContent(@Nonnull String bizCustomer,
								@Nonnull String bizModule,
								@Nonnull String bizDocument,
								@Nullable String bizDataGroupId,
								@Nonnull String bizUserId,
								@Nonnull String bizId,
								@Nonnull String attributeName) {
		super(bizCustomer, bizModule, bizDocument, bizDataGroupId, bizUserId, bizId);
		if (attributeName.indexOf('.') >= 0) {
			throw new IllegalArgumentException("No complex/compound bindings allowed in AttachmentContent - use the correct Document and attribute combination");
		}
		this.attributeName = attributeName;
	}

	public AttachmentContent attachment(@SuppressWarnings("hiding") @Nullable String fileName,
											@Nonnull MimeType mimeType,
											@SuppressWarnings("hiding") @Nonnull byte[] bytes) {
		return internalAttachment(fileName, mimeType.toString(), bytes, null);
	}
	
	public AttachmentContent attachment(@SuppressWarnings("hiding") @Nullable String fileName,
											@Nonnull MimeType mimeType,
											@SuppressWarnings("hiding") @Nonnull File file) {
		return internalAttachment(fileName, mimeType.toString(), null, file);
	}
	
	public AttachmentContent attachment(@SuppressWarnings("hiding") @Nullable String fileName,
											@SuppressWarnings("hiding") @Nonnull String contentType,
											@SuppressWarnings("hiding") @Nonnull byte[] bytes) {
		return internalAttachment(fileName, contentType, bytes, null);
	}
	
	public AttachmentContent attachment(@SuppressWarnings("hiding") @Nullable String fileName,
											@SuppressWarnings("hiding") @Nonnull String contentType,
											@SuppressWarnings("hiding") @Nonnull File file) {
		return internalAttachment(fileName, contentType, null, file);
	}
	
	public AttachmentContent attachment(@Nonnull String fileNameWithStandardSuffix,
											@SuppressWarnings("hiding") @Nonnull byte[] bytes) {
		return internalAttachment(fileNameWithStandardSuffix, null, bytes, null);
	}
	
	public AttachmentContent attachment(@Nonnull String fileNameWithStandardSuffix,
											@SuppressWarnings("hiding") @Nonnull File file) {
		return internalAttachment(fileNameWithStandardSuffix, null, null, file);
	}

	private AttachmentContent internalAttachment(@Nullable String newFileName,
													@Nullable String newContentType,
													@Nullable byte[] newBytes,
													@Nullable File newFile) {
		fileName = newFileName;
		contentType = newContentType;

		// If fileName null and contentType provided, derive default fileName
		if (fileName == null) {
			if (contentType != null) {
				MimeType mimeType = MimeType.fromContentType(contentType);
				if (mimeType != null) {
					fileName = "content." + mimeType.getStandardFileSuffix();
				}
				else {
					fileName = "content";
				}
			}
			else { // neither fileName nor contentType provided, so default fileName
				fileName = "content";
			}
		}
		else {
			// remove the path
			this.fileName = FilenameUtils.getName(this.fileName);
			// remove any invalid chars on all OSs (restricted by windows)
			this.fileName = this.fileName.replaceAll("[\u0001-\u001f<>:\"/\\\\|?*\u007f]+", "").trim();
		}

		// Derive contentType if not supplied but have fileName
		if ((fileName != null) && (contentType == null)) {
			MimeType mimeType = MimeType.fromFileName(this.fileName);
			if (mimeType != null) {
				this.contentType = mimeType.toString();
			}
		}

		bytes = newBytes;
		file = newFile;
		
		return this;
	}

	/**
	 * Set markup to be overlaid on the content.
	 */
	public AttachmentContent markup(@SuppressWarnings("hiding") @Nullable String markup) {
		this.markup = markup;
		return this;
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
	public final @Nullable MimeType getMimeType() {
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

	// Content getters
	
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

	// Cloning 
	
	/**
	 * Clone to use when updating metadata through ContentManager.update() with a remote call - EJB, JDBC, REST.
	 * @return	A clone of the content with zero bytes and no file for remote transmission.
	 */
	public AttachmentContent cloneForRemoteUpdate() {
		AttachmentContent result = new AttachmentContent(bizCustomer,
															bizModule,
															bizDocument,
															bizDataGroupId,
															bizUserId,
															bizId,
															attributeName);
		result.fileName = fileName;
		result.contentType = contentType;
		result.markup = markup;
		
		result.bytes = new byte[0];
		result.contentId = contentId;
		return result;
	}
	
	/**
	 * Clone to use when putting a copy of this content.
	 * @return	A clone of the content linked to its existing file or bytes but with no contentId.
	 */
	public AttachmentContent cloneNewForPut() {
		AttachmentContent result = new AttachmentContent(bizCustomer,
															bizModule,
															bizDocument,
															bizDataGroupId,
															bizUserId,
															bizId,
															attributeName);
		result.fileName = fileName;
		result.contentType = contentType;
		result.markup = markup;
															
		result.bytes = bytes;
		result.file = file;
		return result;
	}

	// Serialization
	
	/**
	 * Ensure that a stream is converted to a self contained byte[] before serializing.
	 * 
	 * @return this
	 * @throws ObjectStreamException
	 */
	private Object writeReplace() throws ObjectStreamException {
		try {
			getContentBytes();
			file = null;
		}
		catch (IOException e) {
			e.printStackTrace();
			throw new InvalidObjectException(e.getLocalizedMessage());
		}
		return this;
	}
}