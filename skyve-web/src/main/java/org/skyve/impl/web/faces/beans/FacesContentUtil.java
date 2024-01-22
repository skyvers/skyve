package org.skyve.impl.web.faces.beans;

import org.apache.commons.io.FilenameUtils;
import org.primefaces.event.FileUploadEvent;
import org.primefaces.model.file.UploadedFile;
import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.content.AttachmentContent;
import org.skyve.content.ContentManager;
import org.skyve.domain.Bean;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.model.document.field.Content;
import org.skyve.impl.metadata.model.document.field.Field.IndexType;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.util.Binder;
import org.skyve.util.Binder.TargetMetaData;

public class FacesContentUtil {
	/**
	 * Handle a file upload event and return the content once uploaded.
	 * @param event the event to handle
	 * @param bean the driving bean
	 * @param binding the binding to upload
	 * @return the uploaded content
	 * @throws Exception
	 */
	public static AttachmentContent handleFileUpload(FileUploadEvent event,
														Bean bean,
														String binding)
	throws Exception {
		UploadedFile file = event.getFile();
		return handleFileUpload(file.getFileName(), file.getContent(), bean, binding);
	}
	
	/**
	 * Handle a file upload event and return the content once uploaded.
	 * @param fileName the name of the uploaded file
	 * @param fileContents	the contents of the uploaded file
	 * @param bean the driving bean
	 * @param binding the binding to upload
	 * @return the uploaded content
	 * @throws Exception
	 */
	public static AttachmentContent handleFileUpload(String filePathOrName,
														byte[] fileContents,
														Bean bean,
														String binding)
	throws Exception {
		Customer customer = CORE.getCustomer();
		String fileName = FilenameUtils.getName(filePathOrName);
		String customerName = customer.getName();
		Bean contentOwner = bean;
		String contentAttributeName = binding;
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

		// Always insert a new attachment content node into the content repository on upload.
		// That way, if the change is discarded (not committed), it'll still point to the original attachment.
		// Also, browser caching is simple as the URL is changed (as a consequence of the content id change)
		AttachmentContent content = new AttachmentContent(customerName, 
															contentOwner.getBizModule(),
															contentOwner.getBizDocument(), 
															contentOwner.getBizDataGroupId(),
															contentOwner.getBizUserId(),
															contentOwner.getBizId(), 
															contentAttributeName, 
															fileName, 
															fileContents);
		try (ContentManager cm = EXT.newContentManager()) {
			// Determine if we should index the content or not
			boolean index = true; // default
			Module module = customer.getModule(contentOwner.getBizModule());
			Document document = module.getDocument(customer, contentOwner.getBizDocument());
			// NB - Could be a base document attribute
			TargetMetaData target = Binder.getMetaDataForBinding(customer, module, document, contentAttributeName);
			Attribute attribute = target.getAttribute();
			if (attribute instanceof Content) {
				IndexType indexType = ((Content) attribute).getIndex();
				index = ((indexType == null) || 
							IndexType.textual.equals(indexType) ||
							IndexType.both.equals(indexType));
			}

			// NB Don't set the content id as we always want a new one
			cm.put(content, index);
		}
		return content; // NB now has a new content id
	}
}
