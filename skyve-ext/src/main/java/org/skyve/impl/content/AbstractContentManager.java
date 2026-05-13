package org.skyve.impl.content;

import java.io.File;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.StandardCopyOption;
import java.util.Date;
import java.util.Map;
import java.util.TreeMap;

import org.apache.commons.io.FilenameUtils;
import org.skyve.CORE;
import org.skyve.content.AttachmentContent;
import org.skyve.content.ContentManager;
import org.skyve.content.MimeType;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.metadata.user.SuperUser;
import org.skyve.impl.util.TimeUtil;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.user.User;
import org.skyve.util.FileUtil;
import org.skyve.util.JSON;
import org.skyve.util.logging.Category;
import org.slf4j.Logger;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Base content manager implementation with shared filesystem storage helpers.
 */
public abstract class AbstractContentManager implements ContentManager {

    private static final Logger CONTENT_LOGGER = Category.CONTENT.logger();
    private static final Logger SECURITY_LOGGER = Category.SECURITY.logger();

    public static final String META_JSON = "meta.json";
	public static final String CONTENT = "content";
	public static final String CONTENT_ID = "id";
    protected static final String CONTENT_TYPE = "content_type";
    protected static final String FILEPATH = "filepath";
    protected static final String FILENAME = "filename";
    public static final String LAST_MODIFIED = "last_modified";
    public static final String ATTRIBUTE_NAME = "attribute";
    protected static final String ATTACHMENT = "attachment";
	protected static final String CLUSTER_NAME = "SKYVE_CONTENT";
    protected static final String MARKUP = "markup";

    @SuppressWarnings({"java:S3008", "java:S1444"}) // this is set at Skyve startup and should not be changed at runtime, so it is effectively final
    public static Class<? extends AbstractContentManager> IMPLEMENTATION_CLASS;
	
	/**
	 * Create a new content manager instance using the configured implementation class.
	 *
	 * @return a new {@link AbstractContentManager} instance.
	 * @throws IllegalArgumentException if the configured implementation cannot be instantiated.
	 */
	public static AbstractContentManager get() {
		try {
			return IMPLEMENTATION_CLASS.getDeclaredConstructor().newInstance();
		}
		catch (Exception e) {
			throw new IllegalArgumentException(IMPLEMENTATION_CLASS + " was not a good choice.", e);
		}
	}
	
	/**
	 * Reindex or deindex the given attachment in the backing content store.
	 *
	 * @param attachment The attachment to update in the index.
	 * @param index True to index; false to remove from the index.
	 * @throws Exception if the reindex operation fails.
	 */
	public abstract void reindex(AttachmentContent attachment, boolean index) throws Exception;
	
	/**
	 * Append a balanced folder structure for storing a content file based on it's content ID.
	 * 
	 * @param id The content ID
	 * @param pathToAppendTo	The path to append to.
	 */
	public static void appendBalancedFolderPathFromContentId(String id, StringBuilder pathToAppendTo) {
		pathToAppendTo.append(id.substring(5, 7).toLowerCase()).append('/');
		pathToAppendTo.append(id.substring(10, 12).toLowerCase()).append('/');
		pathToAppendTo.append(id.substring(15, 17).toLowerCase()).append('/');
		pathToAppendTo.append(id.toLowerCase()).append('/');
	}
	
	/**
	 * Call this to get the folder structure of a content where args[0] is the contentId.
	 */
	@SuppressWarnings("java:S106") // allow System.out.println for this utility method
	public static void main(String[] args) {
		if (args.length != 1) {
			throw new IllegalArgumentException("Add the contentId as an argument to the command line");
		}
		StringBuilder result = new StringBuilder(12);
		appendBalancedFolderPathFromContentId(args[0], result);
		System.out.println(result.toString());
	}
	
	/**
	 * Indicates if the current user can read and access the given content or not
	 * 
	 * @param bizCustomer
	 * @param bizModule
	 * @param bizDocument
	 * @param bizDataGroupId
	 * @param bizUserId
	 * @param bizId
	 * @param attributeName
	 * @return true if the content can be read and accessed, otherwise false.
	 */
	public static boolean canAccessContent(String bizCustomer,
											String bizModule,
											String bizDocument,
											String bizDataGroupId,
											String bizUserId,
											String bizId,
											String attributeName) {
		User user = CORE.getUser();
		if (user instanceof SuperUser) {
			return true;
		}

		try {
			if (attributeName == null) {
				return user.canReadBean(bizId, bizModule, bizDocument, bizCustomer, bizDataGroupId, bizUserId);
			}
			return user.canAccessContent(bizId, bizModule, bizDocument, bizCustomer, bizDataGroupId, bizUserId, attributeName);
		}
		catch (@SuppressWarnings("unused") MetaDataException e) {
			// This can happen when a document was indexed but then the customer access was taken away
			if (UtilImpl.SECURITY_TRACE) SECURITY_LOGGER.info("Could not get the document for {}.{}", bizModule, bizDocument);
			return false;
		}
		catch (@SuppressWarnings("unused") DomainException e) {
			// This happens when the data was deleted but the CMS was not kept in sync
			if (UtilImpl.SECURITY_TRACE) SECURITY_LOGGER.info("Could not retrieve bean {}.{} with ID {}", bizModule, bizDocument, bizId);
			return false;
		}
	}
	
	/**
	 * Write a meta-only content entry for external content files.
	 *
	 * @param absoluteContentStoreFolderPath Absolute content store root path.
	 * @param attachment The attachment metadata to write.
	 * @throws IOException if an IO error occurs while writing metadata.
	 */
	public static void writeExternalContentFile(@Nonnull StringBuilder absoluteContentStoreFolderPath,
													@Nonnull AttachmentContent attachment)
	throws IOException {
		String contentId = attachment.getContentId();
		AbstractContentManager.appendBalancedFolderPathFromContentId(contentId, absoluteContentStoreFolderPath);
		String balancedFolderPath = absoluteContentStoreFolderPath.toString();
		File dir = new File(balancedFolderPath);
		dir.mkdirs();
		
		writeContentMeta(balancedFolderPath, attachment);
	}
	
	/**
	 * Write content data and metadata to the content store, optionally omitting a suffix.
	 *
	 * @param absoluteContentStoreFolderPath Absolute content store root path.
	 * @param attachment The attachment metadata to write.
	 * @param content The content stream to persist.
	 * @param noSuffix True to suppress a file suffix; false to derive one.
	 * @throws IOException if an IO error occurs while writing content or metadata.
	 */
	public static void writeContentFiles(@Nonnull StringBuilder absoluteContentStoreFolderPath,
											@Nonnull AttachmentContent attachment,
											@Nonnull InputStream content,
											boolean noSuffix)
	throws IOException {
		String contentId = attachment.getContentId();
		AbstractContentManager.appendBalancedFolderPathFromContentId(contentId, absoluteContentStoreFolderPath);
		String balancedFolderPath = absoluteContentStoreFolderPath.toString();
		File dir = new File(balancedFolderPath);
		dir.mkdirs();

		File file = new File(dir, determineContentFileName(attachment.getFileName(), attachment.getContentType(), noSuffix));
		File old = null;
		if (file.exists()) {
			old = new File(file.getPath() + "_old");
			if (Files.move(file.toPath(), old.toPath(), StandardCopyOption.REPLACE_EXISTING) == null) {
				throw new IOException("Could not rename " + balancedFolderPath + " to " + balancedFolderPath + "_old before file content store operation");
			}
		}
		
		try {
			try (FileOutputStream fos = new FileOutputStream(file)) {
				content.transferTo(fos);
				fos.flush();
			}
			writeContentMeta(balancedFolderPath, attachment);
		}
		catch (Exception e) {
			if ((old != null) && 
					old.exists() && 
					(Files.move(old.toPath(), file.toPath(), StandardCopyOption.REPLACE_EXISTING) == null)) {
				throw new IOException("Could not rename " + balancedFolderPath + "_old to " + balancedFolderPath + "after file content store operation error.", e);
			}
			
			throw e;
		}
		
		// Now delete the old file after success
		if (old != null) {
			Files.deleteIfExists(old.toPath());
		}
	}
	
	/**
	 * Write the content metadata JSON file into the balanced content folder.
	 *
	 * @param absoluteBalancedFolderPath Absolute balanced folder path for the content ID.
	 * @param attachment The attachment metadata to serialize.
	 * @throws IOException if an IO error occurs while writing the metadata file.
	 */
	protected static void writeContentMeta(String absoluteBalancedFolderPath, AttachmentContent attachment) 
	throws IOException {
		Map<String, Object> meta = new TreeMap<>();
		String fileName = attachment.getFileName();
		String externalAbsoluteFilePath = attachment.getExternalAbsoluteFilePath();
		if (externalAbsoluteFilePath != null) {
			meta.put(FILEPATH, externalAbsoluteFilePath);
			if (fileName == null) {
				fileName = FileUtil.safeFileName(FilenameUtils.getName(externalAbsoluteFilePath));
			}
		}
		meta.put(FILENAME, fileName);
		meta.put(LAST_MODIFIED, TimeUtil.formatISODate(attachment.getLastModified(), true));
		meta.put(CONTENT_TYPE, attachment.getContentType());
		meta.put(Bean.CUSTOMER_NAME, attachment.getBizCustomer());
		meta.put(Bean.DATA_GROUP_ID, attachment.getBizDataGroupId());
		meta.put(Bean.USER_ID, attachment.getBizUserId());
		meta.put(Bean.MODULE_KEY, attachment.getBizModule());
		meta.put(Bean.DOCUMENT_KEY, attachment.getBizDocument());
		meta.put(Bean.DOCUMENT_ID, attachment.getBizId());
		meta.put(ATTRIBUTE_NAME, attachment.getAttributeName());
		String markup = attachment.getMarkup();
		if (markup != null) {
			meta.put(MARKUP, markup);
		}

		File dir = new File(absoluteBalancedFolderPath);

		File file = new File(dir, META_JSON);
		File old = null;
		if (file.exists()) {
			old = new File(file.getPath() + "_old");
			if (Files.move(file.toPath(), old.toPath(), StandardCopyOption.REPLACE_EXISTING) == null) {
				throw new IOException("Could not rename " + absoluteBalancedFolderPath + " to " + absoluteBalancedFolderPath + "_old before file content store meta operation");
			}
		}
		try {
			try (FileWriter fw = new FileWriter(file)) {
				fw.write(JSON.marshall(meta));
				fw.flush();
			}
		}
		catch (Exception e) {
			if ((old != null) && 
					old.exists() && 
					Files.move(old.toPath(), file.toPath(), StandardCopyOption.REPLACE_EXISTING) == null) {
				throw new IOException("Could not rename " + absoluteBalancedFolderPath + "_old to " + absoluteBalancedFolderPath + "after file content store meta operation error.", e);
			}
			
			if (e instanceof IOException ioe) {
				throw ioe;
			}
			throw new IOException("Could not write out meta.json", e);
		}
		// Now delete the old file after success
		if (old != null) {
			Files.deleteIfExists(old.toPath());
		}
	}
	
	/**
	 * Read a content entry from the filesystem using its content ID.
	 *
	 * @param absoluteContentStoreFolderPath Absolute content store root path.
	 * @param contentId The content ID to locate.
	 * @param noSuffix True to suppress a file suffix when resolving content file names.
	 * @return The resolved {@link AttachmentContent}, or null if not found.
	 * @throws Exception if reading or parsing the metadata fails.
	 */
	public static AttachmentContent getFromFileSystem(StringBuilder absoluteContentStoreFolderPath,
														String contentId,
														boolean noSuffix) throws Exception {
		AbstractContentManager.appendBalancedFolderPathFromContentId(contentId, absoluteContentStoreFolderPath);
		String path = absoluteContentStoreFolderPath.toString();

		File dir = new File(path);
		if (! dir.exists()) {
			if (UtilImpl.CONTENT_TRACE) CONTENT_LOGGER.info("AbstractContentManager.get({}) - Dir DNE", path);
			return null;
		}
		File metaFile = new File(dir, META_JSON);
		if (! metaFile.exists()) {
			if (UtilImpl.CONTENT_TRACE) CONTENT_LOGGER.info("AbstractContentManager.get({}) - Meta File DNE", metaFile.getPath());
			return null;
		}
		@SuppressWarnings("unchecked")
		Map<String, Object> meta = (Map<String, Object>) JSON.unmarshall(FileUtil.string(metaFile));

		String contentType = (String) meta.get(CONTENT_TYPE);

		String filePath = (String) meta.get(FILEPATH);
		String fileName = (String) meta.get(FILENAME);
		Date lastModified = TimeUtil.parseISODate((String) meta.get(LAST_MODIFIED));

		String bizCustomer = (String) meta.get(Bean.CUSTOMER_NAME);
		String bizModule = (String) meta.get(Bean.MODULE_KEY);
		String bizDocument = (String) meta.get(Bean.DOCUMENT_KEY);
		String bizDataGroupId = (String) meta.get(Bean.DATA_GROUP_ID);
		String bizUserId = (String) meta.get(Bean.USER_ID);
		String bizId = (String) meta.get(Bean.DOCUMENT_ID);
		String binding = (String) meta.get(ATTRIBUTE_NAME);
		String markup = (String) meta.get(MARKUP);

		File contentFile;
		if (filePath != null) {
			contentFile = new File(filePath);
		}
		else {
			contentFile = new File(dir, determineContentFileName(fileName, contentType, noSuffix));
		}
		if (! contentFile.exists()) {
			if (UtilImpl.CONTENT_TRACE) CONTENT_LOGGER.info("AbstractContentManager.get({}) - File DNE", contentFile.getPath());
			return null;
		}

		AttachmentContent result = new AttachmentContent(bizCustomer,
															bizModule,
															bizDocument,
															bizDataGroupId,
															bizUserId,
															bizId,
															binding)
													.markup(markup)
													.attachment(fileName, contentType, contentFile);	
		result.setLastModified(lastModified);
		result.setContentId(contentId);

		if (UtilImpl.CONTENT_TRACE) CONTENT_LOGGER.info("AbstractContentManager.get({}): exists", contentId);

		return result;
	}
	
	/**
	 * Determine the filename to use for stored content based on name/type and suffix rules.
	 *
	 * @param fileName The original file name, if available.
	 * @param contentType The content type, if available.
	 * @param noSuffix True to suppress a derived suffix.
	 * @return The content file name to use.
	 */
	@SuppressWarnings("java:S3776") // not too complex
	private static @Nonnull String determineContentFileName(@Nullable String fileName, @Nullable String contentType, boolean noSuffix) {
		// Determine the suffix if this option is enabled
		String suffix = null;
		if (UtilImpl.CONTENT_FILE_SUFFIXES && (! noSuffix)) {
			// Prefer file suffix
			if (fileName != null) {
				int lastDot = fileName.lastIndexOf('.');
				if (lastDot > -1) {
					suffix = UtilImpl.processStringValue(fileName.substring(lastDot + 1));
				}
			}
	
			// If no suffix, use content type
			if ((suffix == null) && (contentType != null)) {
				MimeType mimeType = MimeType.fromContentType(contentType);
				if (mimeType != null) {
					suffix = mimeType.getStandardFileSuffix();
				}
			}
		}
		
		return (suffix == null) ? CONTENT : new StringBuilder(CONTENT.length() + suffix.length() + 1).append(CONTENT).append('.').append(suffix).toString();
	}
}
