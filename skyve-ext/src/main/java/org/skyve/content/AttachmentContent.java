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
import org.skyve.util.logging.SkyveLoggerFactory;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

import org.slf4j.Logger;

/**
 * Represents an attachment stored in a content document attribute.
 * The contentId is the unique identifier used to get and manipulate this content.
 * @author mike
 */
public class AttachmentContent extends Content {
	private static final long serialVersionUID = 5929667528318345993L;
	private static final Logger LOGGER = SkyveLoggerFactory.getLogger(AttachmentContent.class);

	private String attributeName;
	private String contentId;
	private String fileName;
	private String contentType;
	private Date lastModified;
	private transient File file;
	private byte[] bytes;
	private String markup; // Editable markup, usually SVG on a raster image

	// Used to reference an external file from the content repo
	// This is the full path including the file name
	private String externalAbsoluteFilePath;

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

	/**
	 * Attach content using a file name and mime type with byte content.
	 *
	 * @param fileName Optional file name to apply to this content.
	 * @param mimeType Mime type for the content.
	 * @param bytes Content bytes to attach.
	 * @return this content instance
	 */
	public @Nonnull AttachmentContent attachment(@SuppressWarnings("hiding") @Nullable String fileName,
													@Nonnull MimeType mimeType,
													@SuppressWarnings("hiding") @Nonnull byte[] bytes) {
		return internalAttachment(fileName, mimeType.toString(), bytes, null);
	}
	
	/**
	 * Attach content using a file name and mime type with file content.
	 *
	 * @param fileName Optional file name to apply to this content.
	 * @param mimeType Mime type for the content.
	 * @param file File to attach.
	 * @return this content instance
	 */
	public @Nonnull AttachmentContent attachment(@SuppressWarnings("hiding") @Nullable String fileName,
													@Nonnull MimeType mimeType,
													@SuppressWarnings("hiding") @Nonnull File file) {
		return internalAttachment(fileName, mimeType.toString(), null, file);
	}
	
	/**
	 * Attach content using a file name and content type with byte content.
	 *
	 * @param fileName Optional file name to apply to this content.
	 * @param contentType Content type to apply to this content.
	 * @param bytes Content bytes to attach.
	 * @return this content instance
	 */
	public @Nonnull AttachmentContent attachment(@SuppressWarnings("hiding") @Nullable String fileName,
											@SuppressWarnings("hiding") @Nonnull String contentType,
											@SuppressWarnings("hiding") @Nonnull byte[] bytes) {
		return internalAttachment(fileName, contentType, bytes, null);
	}
	
	/**
	 * Attach content using a file name and content type with file content.
	 *
	 * @param fileName Optional file name to apply to this content.
	 * @param contentType Content type to apply to this content.
	 * @param file File to attach.
	 * @return this content instance
	 */
	public @Nonnull AttachmentContent attachment(@SuppressWarnings("hiding") @Nullable String fileName,
													@SuppressWarnings("hiding") @Nonnull String contentType,
													@SuppressWarnings("hiding") @Nonnull File file) {
		return internalAttachment(fileName, contentType, null, file);
	}
	
	/**
	 * Attach content using a file name that includes a standard suffix with byte content.
	 *
	 * @param fileNameWithStandardSuffix File name that includes a standard suffix.
	 * @param bytes Content bytes to attach.
	 * @return this content instance
	 */
	public @Nonnull AttachmentContent attachment(@Nonnull String fileNameWithStandardSuffix,
													@SuppressWarnings("hiding") @Nonnull byte[] bytes) {
		return internalAttachment(fileNameWithStandardSuffix, null, bytes, null);
	}
	
	/**
	 * Attach content using a file name that includes a standard suffix with file content.
	 *
	 * @param fileNameWithStandardSuffix File name that includes a standard suffix.
	 * @param file File to attach.
	 * @return this content instance
	 */
	public @Nonnull AttachmentContent attachment(@Nonnull String fileNameWithStandardSuffix,
													@SuppressWarnings("hiding") @Nonnull File file) {
		return internalAttachment(fileNameWithStandardSuffix, null, null, file);
	}

	/**
	 * Configure the attachment metadata and content source.
	 *
	 * @param newFileName File name to apply.
	 * @param newContentType Content type to apply.
	 * @param newBytes Content bytes to attach.
	 * @param newFile File to attach.
	 * @return this content instance
	 */
	private @Nonnull AttachmentContent internalAttachment(@Nullable String newFileName,
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
	 *
	 * @param markup Markup to apply.
	 * @return this content instance
	 */
	public @Nonnull AttachmentContent markup(@SuppressWarnings("hiding") @Nullable String markup) {
		this.markup = markup;
		return this;
	}

	/**
	 * Set an external absolute file path.
	 * This is the full path to the file including the file name.
	 * This must be a valid path that exists and is sanitised - ie not set by user input without sanitisation.
	 *
	 * @param externalAbsoluteFilePath External absolute file path.
	 * @return this content instance
	 */
	public @Nonnull AttachmentContent externalAbsoluteFilePath(@SuppressWarnings("hiding") @Nonnull String externalAbsoluteFilePath) {
		this.externalAbsoluteFilePath = externalAbsoluteFilePath;
		File newFile = new File(externalAbsoluteFilePath);
		if (newFile.exists() && newFile.isFile()) {
			internalAttachment(newFile.getName(), null, null, newFile);
		}
		else {
			throw new IllegalArgumentException("External absolute file path does not exist or is not a file: " + externalAbsoluteFilePath);
		}
		return this;
	}
	
	/**
	 * The simple (not compound) attribute name for this attachment.
	 *
	 * @return attribute name
	 */
	public final @Nonnull String getAttributeName() {
		return attributeName;
	}
	
	/**
	 * Set the simple (not compound) attribute name for this attachment.
	 *
	 * @param attributeName Attribute name
	 */
	public final void setAttributeName(@Nonnull String attributeName) {
		this.attributeName = attributeName;
	}
	
	/**
	 * The contentId unique identifier within the content repository.
	 *
	 * @return a unique identifier or null if this has not yet been put in the content repository.
	 */
	public final @Nullable String getContentId() {
		return contentId;
	}
	
	/**
	 * Set the contentId unique identifier within the content repository.
	 *
	 * @param contentId Content identifier
	 */
	public final void setContentId(@Nullable String contentId) {
		this.contentId = contentId;
	}

	/**
	 * External path of the originating file this content came from.
	 *
	 * @return external absolute file path
	 */
	public final @Nullable String getExternalAbsoluteFilePath() {
		return externalAbsoluteFilePath;
	}

	/**
	 * Set the external absolute file path that the content originated from.
	 *
	 * @param externalAbsoluteFilePath External absolute file path
	 */
	public final void setExternalAbsoluteFilePath(@Nullable String externalAbsoluteFilePath) {
		this.externalAbsoluteFilePath = externalAbsoluteFilePath;
	}

	/**
	 * Name of the originating file this content came from.
	 *
	 * @return file name
	 */
	public final @Nullable String getFileName() {
		return fileName;
	}

	/**
	 * Set the name of the originating file this content came from.
	 *
	 * @param fileName File name
	 */
	public final void setFileName(@Nullable String fileName) {
		this.fileName = fileName;
	}
	
	/**
	 * The mime type of this content.
	 *
	 * @return mime type or null
	 */
	public final @Nullable MimeType getMimeType() {
		return (contentType == null) ? null : MimeType.fromContentType(contentType);
	}

	/**
	 * The content type of this content - usually matches mime type but may be a variant.
	 *
	 * @return content type
	 */
	public final @Nullable String getContentType() {
		return contentType;
	}
	
	/**
	 * Set the content type for this content.
	 *
	 * @param contentType Content type
	 */
	public final void setContentType(@Nullable String contentType) {
		this.contentType = contentType;
	}
	
	/**
	 * The date/time of last modification.
	 *
	 * @return last modified date
	 */
	public final @Nullable Date getLastModified() {
		return lastModified;
	}
	
	/**
	 * Set the date/time of last modification.
	 *
	 * @param lastModified Last modified date
	 */
	public final void setLastModified(@Nullable Date lastModified) {
		this.lastModified = lastModified;
	}
	
	/**
	 * Markup to be overlaid on the content as edits - eg SVG on raster image content.
	 *
	 * @return markup
	 */
	public @Nullable String getMarkup() {
		return markup;
	}
	
	/**
	 * Set markup to be overlaid on the content as edits.
	 *
	 * @param markup Markup to apply
	 */
	public void setMarkup(@Nullable String markup) {
		this.markup = markup;
	}

	// Add mutability to Content interface
	
	/**
	 * Set the customer for this content reference.
	 *
	 * @param bizCustomer Customer identifier
	 */
	public final void setBizCustomer(@Nonnull String bizCustomer) {
		this.bizCustomer = bizCustomer;
	}

	/**
	 * Set the module for this content reference.
	 *
	 * @param bizModule Module identifier
	 */
	public final void setBizModule(@Nonnull String bizModule) {
		this.bizModule = bizModule;
	}
	
	/**
	 * Set the document for this content reference.
	 *
	 * @param bizDocument Document identifier
	 */
	public final void setBizDocument(@Nonnull String bizDocument) {
		this.bizDocument = bizDocument;
	}
	
	/**
	 * Set the data group id for this content reference.
	 *
	 * @param bizDataGroupId Data group identifier
	 */
	public final void setBizDataGroupId(@Nullable String bizDataGroupId) {
		this.bizDataGroupId = bizDataGroupId;
	}
	
	/**
	 * Set the user id for this content reference.
	 *
	 * @param bizUserId User identifier
	 */
	public final void setBizUserId(@Nonnull String bizUserId) {
		this.bizUserId = bizUserId;
	}

	// Content getters
	
	/**
	 * The content stream.
	 * NB This must be closed by the caller.
	 *
	 * @return content stream
	 */
	@SuppressWarnings("resource") // Caller owns and must close the returned stream.
	public final @Nonnull InputStream getContentStream() {
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
	 * Returns the number of bytes available from the current content source without
	 * materialising file-backed content into memory.
	 *
	 * <p>Side effects: none. For file-backed content this delegates to
	 * {@link java.io.File#length()} and does not open the file or populate the cached
	 * byte array used by {@link #getContentBytes()}.
	 *
	 * @return the byte length, or {@code -1} when no content source is attached
	 */
	public final long getContentLength() {
		if (file != null) {
			return file.length();
		}
		return (bytes == null) ? -1L : bytes.length;
	}
	
	/**
	 * Read the content bytes, loading from the content stream if needed.
	 *
	 * @return content bytes
	 * @throws IOException if the stream cannot be read
	 */
	public final @Nonnull byte[] getContentBytes() throws IOException {
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
	 *
	 * @return A clone of the content with zero bytes and no file for remote transmission.
	 */
	public @Nonnull AttachmentContent cloneForRemoteUpdate() {
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
	 *
	 * @return A clone of the content linked to its existing file or bytes but with no contentId.
	 */
	public @Nonnull AttachmentContent cloneNewForPut() {
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
	 * @throws ObjectStreamException if serialization fails
	 */
	private @Nonnull Object writeReplace() throws ObjectStreamException {
		try {
			getContentBytes();
			file = null;
		}
		catch (IOException e) {
			LOGGER.error(e.getMessage(), e);
			throw new InvalidObjectException(e.getLocalizedMessage());
		}
		return this;
	}
}
