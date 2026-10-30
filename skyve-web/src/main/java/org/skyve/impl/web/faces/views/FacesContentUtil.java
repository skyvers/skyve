package org.skyve.impl.web.faces.views;

import org.apache.commons.io.FilenameUtils;
import org.primefaces.event.FileUploadEvent;
import org.primefaces.model.file.UploadedFile;
import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.content.AttachmentContent;
import org.skyve.content.ContentManager;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.SecurityException;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.model.document.field.Content;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.util.Binder;
import org.skyve.util.Binder.TargetMetaData;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Provides utility methods for storing JSF file uploads in the content repository.
 *
 * <p>The upload methods resolve the content-owning bean from the supplied binding,
 * enforce access for the current user, and create a new attachment for every upload.
 */
public class FacesContentUtil {
	/**
	 * Prevents utility-class instantiation.
	 */
	private FacesContentUtil() {
		// nothing to see here
	}
	
	/**
	 * Stores the file represented by an upload event against a bean content binding.
	 *
	 * <p>Side effects: creates a new attachment in the content repository and may
	 * instantiate intermediate beans when {@code binding} is compound.
	 *
	 * @param event the upload event containing the file name, bytes, and content type
	 * @param bean the bean relative to which the content binding is resolved
	 * @param binding the simple or compound binding of the target content attribute
	 * @return the stored attachment, including its newly assigned content identifier
	 * @throws SecurityException if the current user cannot upload content to the resolved attribute
	 * @throws Exception if the upload cannot be resolved or stored
	 */
	public static @Nonnull AttachmentContent handleFileUpload(@Nonnull FileUploadEvent event,
																@Nonnull Bean bean,
																@Nonnull String binding)
	throws Exception {
		UploadedFile file = event.getFile();
		return storeFileUpload(file.getFileName(), file.getContent(), file.getContentType(), null, bean, binding);
	}
	
	/**
	 * Stores unnamed uploaded bytes against a bean content binding.
	 *
	 * <p>Side effects: creates a new attachment in the content repository and may
	 * instantiate intermediate beans when {@code binding} is compound. The
	 * attachment derives a file name from {@code contentType} when possible and
	 * otherwise uses {@code content}.
	 *
	 * @param fileContents the uploaded file bytes
	 * @param contentType the uploaded content type
	 * @param bean the bean relative to which the content binding is resolved
	 * @param binding the simple or compound binding of the target content attribute
	 * @return the stored attachment, including its newly assigned content identifier
	 * @throws SecurityException if the current user cannot upload content to the resolved attribute
	 * @throws Exception if the upload cannot be resolved or stored
	 */
	public static @Nonnull AttachmentContent handleFileUpload(@Nonnull byte[] fileContents,
																@Nonnull String contentType,
																@Nonnull Bean bean,
																@Nonnull String binding)
	throws Exception {
		return storeFileUpload(null, fileContents, contentType, null, bean, binding);
	}

	/**
	 * Stores named uploaded bytes against a bean content binding, deriving the
	 * content type from the file name when possible.
	 *
	 * <p>Side effects: creates a new attachment in the content repository and may
	 * instantiate intermediate beans when {@code binding} is compound.
	 *
	 * @param filePathOrName the uploaded file name or path
	 * @param fileContents the uploaded file bytes
	 * @param bean the bean relative to which the content binding is resolved
	 * @param binding the simple or compound binding of the target content attribute
	 * @return the stored attachment, including its newly assigned content identifier
	 * @throws SecurityException if the current user cannot upload content to the resolved attribute
	 * @throws Exception if the upload cannot be resolved or stored
	 */
	public static @Nonnull AttachmentContent handleFileUpload(@Nonnull String filePathOrName,
																@Nonnull byte[] fileContents,
																@Nonnull Bean bean,
																@Nonnull String binding)
	throws Exception {
		return storeFileUpload(filePathOrName, fileContents, null, null, bean, binding);
	}
	
	/**
	 * Stores uploaded bytes against a bean content binding.
	 *
	 * <p>Only the final file-name component of {@code filePathOrName} is retained.
	 *
	 * <p>Side effects: resolves and checks content access for the current user,
	 * may instantiate intermediate beans for a compound binding, and inserts a new
	 * attachment into the content repository using the attribute's indexing policy.
	 * The method always creates a new content node rather than replacing an existing
	 * node, so the returned attachment receives a new content identifier.
	 *
	 * @param filePathOrName the uploaded file name or path
	 * @param fileContents the uploaded file bytes
	 * @param contentType the uploaded content type
	 * @param bean the bean relative to which the content binding is resolved
	 * @param binding the simple or compound binding of the target content attribute
	 * @return the stored attachment, including its newly assigned content identifier
	 * @throws SecurityException if the current user cannot upload content to the resolved attribute
	 * @throws IllegalStateException if the owner of a compound content binding cannot be resolved
	 * @throws Exception if metadata resolution or content-repository storage fails
	 */
	public static @Nonnull AttachmentContent handleFileUpload(@Nonnull String filePathOrName,
																@Nonnull byte[] fileContents,
																@Nonnull String contentType,
																@Nonnull Bean bean,
																@Nonnull String binding)
	throws Exception {
		return storeFileUpload(filePathOrName, fileContents, contentType, null, bean, binding);
	}

	/**
	 * Stores an uploaded image against a bean content binding, with SVG markup drawn over it.
	 *
	 * <p>Only the final file-name component of {@code filePathOrName} is retained.
	 * The markup is stored as given, so callers must sanitise it first
	 * (see {@link org.skyve.impl.util.SvgMarkupSanitiser}).
	 *
	 * <p>Side effects: as {@link #handleFileUpload(String, byte[], String, Bean, String)}.
	 *
	 * @param filePathOrName the uploaded file name or path
	 * @param fileContents the uploaded file bytes
	 * @param contentType the uploaded content type, or {@code null} to derive it from the file name
	 * @param markup sanitised SVG markup to store with the image, or {@code null} for none
	 * @param bean the bean relative to which the content binding is resolved
	 * @param binding the simple or compound binding of the target content attribute
	 * @return the stored attachment, including its newly assigned content identifier
	 * @throws SecurityException if the current user cannot upload content to the resolved attribute
	 * @throws IllegalStateException if the owner of a compound content binding cannot be resolved
	 * @throws Exception if metadata resolution or content-repository storage fails
	 */
	public static @Nonnull AttachmentContent handleFileUpload(@Nonnull String filePathOrName,
																@Nonnull byte[] fileContents,
																@Nullable String contentType,
																@Nullable String markup,
																@Nonnull Bean bean,
																@Nonnull String binding)
	throws Exception {
		return storeFileUpload(filePathOrName, fileContents, contentType, markup, bean, binding);
	}

	/**
	 * Stores uploaded bytes while accommodating metadata omitted by an upload event.
	 *
	 * <p>If the content type is unavailable, it is derived from the file name when
	 * possible. If both values are unavailable, the attachment is named
	 * {@code content}. Only the final component of a supplied file path is retained.
	 *
	 * @param filePathOrName the uploaded file name or path, or {@code null} when unavailable
	 * @param fileContents the uploaded file bytes
	 * @param contentType the uploaded content type, or {@code null} when unavailable
	 * @param markup sanitised SVG markup to store with the content, or {@code null} for none
	 * @param bean the bean relative to which the content binding is resolved
	 * @param binding the simple or compound binding of the target content attribute
	 * @return the stored attachment, including its newly assigned content identifier
	 * @throws SecurityException if the current user cannot upload content to the resolved attribute
	 * @throws IllegalStateException if the owner of a compound content binding cannot be resolved
	 * @throws Exception if metadata resolution or content-repository storage fails
	 */
	private static @Nonnull AttachmentContent storeFileUpload(@Nullable String filePathOrName,
																@Nonnull byte[] fileContents,
																@Nullable String contentType,
																@Nullable String markup,
																@Nonnull Bean bean,
																@Nonnull String binding)
	throws Exception {
		User user = CORE.getUser();
		Customer customer = user.getCustomer();
		String fileName = (filePathOrName == null) ? null : FilenameUtils.getName(filePathOrName);
		Bean contentOwner = bean;
		String contentAttributeName = binding;
		
		// If the binding is compound, obtain the contentOwner bean - the penultimate binding
		int contentBindingLastDotIndex = binding.lastIndexOf('.');
		if (contentBindingLastDotIndex >= 0) { // compound binding
			String penultimateBinding = binding.substring(0, contentBindingLastDotIndex);
			contentOwner = (Bean) BindUtil.get(bean, penultimateBinding);
			// Instantiate any intermediate objects along the way if required
			if (contentOwner == null) {
				User u = CORE.getUser();
				Module m = customer.getModule(bean.getBizModule());
				Document d = m.getDocument(customer, bean.getBizDocument());
				contentOwner = (Bean) BindUtil.instantiateAndGet(u, m, d, bean, penultimateBinding);
			}
			if (contentOwner == null) { // should never happen
				throw new IllegalStateException("contentOwner is null");
			}
			contentAttributeName = binding.substring(contentBindingLastDotIndex + 1);
		}

		String contentOwnerBizId = contentOwner.getBizId();
		String contentOwnerBizModule = contentOwner.getBizModule();
		String contentOwnerBizDocument = contentOwner.getBizDocument();
		String contentOwnerBizCustomer = contentOwner.getBizCustomer();
		String contentOwnerBizDataGroupId = contentOwner.getBizDataGroupId();
		String contentOwnerBizUserId = contentOwner.getBizUserId();
		
		// Checks for content permissions/restrictions and tests if bean instance can be read
		if (! user.canAccessContent(contentOwnerBizId,
										contentOwnerBizModule,
										contentOwnerBizDocument,
										contentOwnerBizCustomer,
										contentOwnerBizDataGroupId,
										contentOwnerBizUserId,
										contentAttributeName)) {
			throw new SecurityException("upload this content", user.getName());
		}

		// Always insert a new attachment content node into the content repository on upload.
		// That way, if the change is discarded (not committed), it'll still point to the original attachment.
		// Also, browser caching is simple as the URL is changed (as a consequence of the content id change)
		AttachmentContent content = new AttachmentContent(contentOwnerBizCustomer,
																	contentOwnerBizModule,
																	contentOwnerBizDocument,
																	contentOwnerBizDataGroupId,
																	contentOwnerBizUserId,
																	contentOwnerBizId,
																	contentAttributeName);
		if (contentType == null) {
			content.attachment((fileName == null) ? "content" : fileName, fileContents);
		}
		else {
			content.attachment(fileName, contentType, fileContents);
		}
		if (markup != null) {
			content.setMarkup(markup);
		}
		try (ContentManager cm = EXT.newContentManager()) {
			// Determine if we should index the content or not
			boolean index = true; // default
			Module module = customer.getModule(contentOwner.getBizModule());
			Document document = module.getDocument(customer, contentOwner.getBizDocument());
			// NB - Could be a base document attribute
			TargetMetaData target = Binder.getMetaDataForBinding(customer, module, document, contentAttributeName);
			Attribute attribute = target.getAttribute();
			if (attribute instanceof Content c) {
				index = Content.isTextuallyIndexed(c.getIndex());
			}

			// NB Don't set the content id as we always want a new one
			cm.put(content, index);
		}
		return content; // NB now has a new content id
	}
}
