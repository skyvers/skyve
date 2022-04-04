package org.skyve.impl.content;

import java.io.File;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.StandardCopyOption;
import java.util.Date;
import java.util.Map;
import java.util.TreeMap;

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
import org.skyve.impl.content.AbstractContentManager;

public abstract class AbstractContentManager implements ContentManager {
    public static final String META_JSON = "meta.json";
	public static final String CONTENT = "content";
	public static final String CONTENT_ID = "id";
    protected static final String CONTENT_TYPE = "content_type";
    protected static final String FILENAME = "filename";
    public static final String LAST_MODIFIED = "last_modified";
    public static final String ATTRIBUTE_NAME = "attribute";
    protected static final String ATTACHMENT = "attachment";
	protected static final String CLUSTER_NAME = "SKYVE_CONTENT";

    
	public static Class<? extends AbstractContentManager> IMPLEMENTATION_CLASS;
	
	public static AbstractContentManager get() {
		try {
			AbstractContentManager result = IMPLEMENTATION_CLASS.getDeclaredConstructor().newInstance();
			return result;
		}
		catch (Exception e) {
			throw new IllegalArgumentException(IMPLEMENTATION_CLASS + " was not a good choice.", e);
		}
	}
	
	public abstract void reindex(AttachmentContent attachment, boolean index) throws Exception;
	
	/**
	 * Append a balanced folder structure for storing a content file based on it's content ID.
	 * 
	 * @param id The content ID
	 * @param pathToAppendTo	The path to append to.
	 */
	// TODO remove the olSkool parameter once we've transformed all content
	public static void appendBalancedFolderPathFromContentId(String id, StringBuilder pathToAppendTo, boolean olSkool) {
		pathToAppendTo.append(id.substring(5, 7).toLowerCase()).append('/');
		pathToAppendTo.append(id.substring(10, 12).toLowerCase()).append('/');
		pathToAppendTo.append(id.substring(15, 17).toLowerCase()).append('/');
		if (! olSkool) {
			pathToAppendTo.append(id.toLowerCase()).append('/');
		}
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
			if (UtilImpl.SECURITY_TRACE) System.err.println("Could not get the document for " + bizModule + '.' + bizDocument);
			return false;
		}
		catch (@SuppressWarnings("unused") DomainException e) {
			// This happens when the data was deleted but the CMS was not kept in sync
			if (UtilImpl.SECURITY_TRACE) System.err.println("Could not retrieve bean " + bizModule + '.' + bizDocument + " with ID " + bizId);
			return false;
		}
	}
	
	public static void writeContentFiles(StringBuilder absoluteContentStoreFolderPath, AttachmentContent attachment, byte[] content) 
	throws Exception {
		String contentId = attachment.getContentId();
		AbstractContentManager.appendBalancedFolderPathFromContentId(contentId, absoluteContentStoreFolderPath, false);
		String balancedFolderPath = absoluteContentStoreFolderPath.toString();
		File dir = new File(balancedFolderPath);
		dir.mkdirs();
		
		File file = new File(dir, CONTENT);
		File old = null;
		if (file.exists()) {
			old = new File(file.getPath() + "_old");
			if (Files.move(file.toPath(), old.toPath(), StandardCopyOption.REPLACE_EXISTING) == null) {
				throw new IOException("Could not rename " + balancedFolderPath + " to " + balancedFolderPath + "_old before file content store operation");
			}
		}
		try {
			try (FileOutputStream fos = new FileOutputStream(file)) {
				fos.write(content);
				fos.flush();
			}
			writeContentMeta(balancedFolderPath, attachment);
		}
		catch (Exception e) {
			if ((old != null) && old.exists()) {
				if (Files.move(old.toPath(), file.toPath(), StandardCopyOption.REPLACE_EXISTING) == null) {
					throw new IOException("Could not rename " + balancedFolderPath + "_old to " + balancedFolderPath + "after file content store operation error.", e);
				}
			}
			throw e;
		}
		// Now delete the old file after success
		if ((old != null) && old.exists()) {
			old.delete();
		}
	}
	
	protected static void writeContentMeta(String absoluteBalancedFolderPath, AttachmentContent attachment) 
	throws Exception {
		Map<String, Object> meta = new TreeMap<>();
		meta.put(FILENAME, attachment.getFileName());
		meta.put(LAST_MODIFIED, TimeUtil.formatISODate(attachment.getLastModified(), true));
		meta.put(CONTENT_TYPE, attachment.getContentType());
		meta.put(Bean.CUSTOMER_NAME, attachment.getBizCustomer());
		meta.put(Bean.DATA_GROUP_ID, attachment.getBizDataGroupId());
		meta.put(Bean.USER_ID, attachment.getBizUserId());
		meta.put(Bean.MODULE_KEY, attachment.getBizModule());
		meta.put(Bean.DOCUMENT_KEY, attachment.getBizDocument());
		meta.put(Bean.DOCUMENT_ID, attachment.getBizId());
		meta.put(ATTRIBUTE_NAME, attachment.getAttributeName());

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
			if ((old != null) && old.exists()) {
				if (Files.move(old.toPath(), file.toPath(), StandardCopyOption.REPLACE_EXISTING) == null) {
					throw new IOException("Could not rename " + absoluteBalancedFolderPath + "_old to " + absoluteBalancedFolderPath + "after file content store meta operation error.", e);
				}
			}
			throw e;
		}
		// Now delete the old file after success
		if ((old != null) && old.exists()) {
			old.delete();
		}
	}
	
	public static AttachmentContent getFromFileSystem(StringBuilder absoluteContentStoreFolderPath, String contentId) throws Exception {
		AbstractContentManager.appendBalancedFolderPathFromContentId(contentId, absoluteContentStoreFolderPath, false);
		String path = absoluteContentStoreFolderPath.toString();

		File dir = new File(path);
		if (! dir.exists()) {
			if (UtilImpl.CONTENT_TRACE) UtilImpl.LOGGER.info("AbstractContentManager.get(" + path + ") - Dir DNE");
			return null;
		}
		File metaFile = new File(dir, META_JSON);
		if (! metaFile.exists()) {
			if (UtilImpl.CONTENT_TRACE) UtilImpl.LOGGER.info("AbstractContentManager.get(" + metaFile.getPath() + ") - Meta File DNE");
			return null;
		}
		@SuppressWarnings("unchecked")
		Map<String, Object> meta = (Map<String, Object>) JSON.unmarshall(null, FileUtil.getFileAsString(metaFile));

		File file = new File(dir, CONTENT);
		if (! file.exists()) {
			if (UtilImpl.CONTENT_TRACE) UtilImpl.LOGGER.info("AbstractContentManager.get(" + file.getPath() + ") - File DNE");
			return null;
		}
		
		MimeType mimeType = null;
		String contentType = (String) meta.get(CONTENT_TYPE);
		if (contentType != null) {
			mimeType = MimeType.fromContentType(contentType);
		}

		String fileName = (String) meta.get(FILENAME);
		Date lastModified = TimeUtil.parseISODate((String) meta.get(LAST_MODIFIED));

		String bizCustomer = (String) meta.get(Bean.CUSTOMER_NAME);
		String bizModule = (String) meta.get(Bean.MODULE_KEY);
		String bizDocument = (String) meta.get(Bean.DOCUMENT_KEY);
		String bizDataGroupId = (String) meta.get(Bean.DATA_GROUP_ID);
		String bizUserId = (String) meta.get(Bean.USER_ID);
		String bizId = (String) meta.get(Bean.DOCUMENT_ID);
		String binding = (String) meta.get(ATTRIBUTE_NAME);

		AttachmentContent result = new AttachmentContent(bizCustomer,
															bizModule,
															bizDocument,
															bizDataGroupId,
															bizUserId,
															bizId,
															binding,
															fileName,
															mimeType,
															file);
		result.setLastModified(lastModified);
		result.setContentType(contentType);
		result.setContentId(contentId);
		if (UtilImpl.CONTENT_TRACE) UtilImpl.LOGGER.info("AbstractContentManager.get(" + contentId + "): exists");

		return result;
	}
}
