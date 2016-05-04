package org.skyve.impl.web.faces.beans;

import javax.faces.application.FacesMessage;
import javax.faces.bean.ManagedBean;
import javax.faces.bean.ManagedProperty;
import javax.faces.bean.RequestScoped;
import javax.faces.context.ExternalContext;
import javax.faces.context.FacesContext;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.primefaces.context.RequestContext;
import org.primefaces.event.FileUploadEvent;
import org.primefaces.model.UploadedFile;
import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.content.AttachmentContent;
import org.skyve.content.ContentManager;
import org.skyve.domain.Bean;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.model.document.field.Content;
import org.skyve.impl.metadata.model.document.field.Field.IndexType;
import org.skyve.impl.metadata.user.UserImpl;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.AbstractWebContext;
import org.skyve.impl.web.WebUtil;
import org.skyve.impl.web.faces.FacesAction;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.persistence.Persistence;
import org.skyve.util.Binder;
import org.skyve.util.Binder.TargetMetaData;
import org.skyve.web.WebContext;

@ManagedBean(name = "_skyveContent")
@RequestScoped
public class ContentUpload extends Localisable {
	private static final long serialVersionUID = -6769960348990922565L;

	@ManagedProperty(value = "#{param." + AbstractWebContext.CONTEXT_NAME + "}")
    private String context;
    
    @ManagedProperty(value = "#{param." + AbstractWebContext.BINDING_NAME + "}")
    private String binding;

    @ManagedProperty(value = "#{param." + AbstractWebContext.RESOURCE_FILE_NAME + "}")
    private String contentBinding;

	public void preRender() {
		new FacesAction<Void>() {
			@Override
			public Void callback() throws Exception {
				Persistence p = CORE.getPersistence();
				UserImpl internalUser = (UserImpl) p.getUser();
				initialise(internalUser, FacesContext.getCurrentInstance().getExternalContext().getRequestLocale());
				
				return null;
			}
		}.execute();
	}
	

    public String getContext() {
		return context;
	}

	public void setContext(String context) {
		this.context = UtilImpl.processStringValue(context);
	}

	public String getBinding() {
		return binding;
	}

	public void setBinding(String binding) {
		this.binding = UtilImpl.processStringValue(binding);
	}

	public String getContentBinding() {
		return contentBinding;
	}

	public void setContentBinding(String contentBinding) {
		this.contentBinding = UtilImpl.processStringValue(contentBinding);
	}

	/**
	 * Process the file upload
	 * 
	 * @param event
	 */
	public void handleFileUpload(FileUploadEvent event)
	throws Exception {
		FacesContext fc = FacesContext.getCurrentInstance();

		if ((context == null) || (contentBinding == null)) {
			UtilImpl.LOGGER.warning("FileUpload - Malformed URL on Upload Action - context, binding, or contentBinding is null");
			FacesMessage msg = new FacesMessage("Failure", "Malformed URL");
	        fc.addMessage(null, msg);
	        return;
		}

		ExternalContext ec = fc.getExternalContext();
		HttpServletRequest request = (HttpServletRequest) ec.getRequest();
		HttpServletResponse response = (HttpServletResponse) ec.getResponse();

		UploadedFile file = event.getFile();
		
		AbstractPersistence persistence = AbstractPersistence.get();
		User user = (User) request.getSession().getAttribute(WebContext.USER_SESSION_ATTRIBUTE_NAME);
		persistence.setUser(user);
		persistence.begin();
		try {
			Customer customer = user.getCustomer();
	
			AbstractWebContext webContext = WebUtil.getCachedConversation(context, request, response);
			if (webContext == null) {
				UtilImpl.LOGGER.warning("FileUpload - Malformed URL on Content Upload - context does not exist");
				FacesMessage msg = new FacesMessage("Failure", "Malformed URL");
		        FacesContext.getCurrentInstance().addMessage(null, msg);
		        return;
			}
	
			Bean currentBean = webContext.getCurrentBean();
			Bean bean = currentBean;

			if (binding != null) {
				bean = (Bean) BindUtil.get(bean, binding);
			}

			String fileName = file.getFileName();
			String customerName = customer.getName();
			Bean contentOwner = bean;
			String contentAttributeName = contentBinding;
			int contentBindingLastDotIndex = contentBinding.lastIndexOf('.');
			if (contentBindingLastDotIndex >= 0) { // compound binding
				contentOwner = (Bean) BindUtil.get(bean, contentBinding.substring(0, contentBindingLastDotIndex));
				contentAttributeName = contentBinding.substring(contentBindingLastDotIndex + 1);
			}

			// Always insert a new attachment content node into the content repository on upload.
			// That way, if the change is discarded (not committed), it'll still point to the original attachment.
			// Also, browser caching is simple as the URL is changed (as a consequence of the content id change)
			String contentId = null;
			try (ContentManager cm = EXT.newContentManager()) {
				AttachmentContent content = new AttachmentContent(customerName, 
																	contentOwner.getBizModule(), 
																	contentOwner.getBizDocument(),
																	contentOwner.getBizDataGroupId(), 
																	contentOwner.getBizUserId(), 
																	contentOwner.getBizId(),
																	contentAttributeName,
																	fileName,
																	file.getInputstream());

				// Determine if we should index the content or not
				boolean index = true; // default
				Module module = customer.getModule(contentOwner.getBizModule());
				// NB - Could be a base document attribute
				TargetMetaData target = Binder.getMetaDataForBinding(customer, module, module.getDocument(customer, contentOwner.getBizDocument()), contentAttributeName);
				Attribute attribute = target.getAttribute();
				if (attribute instanceof Content) {
					IndexType indexType = ((Content) attribute).getIndex();
					index = ((indexType == null) || IndexType.textual.equals(indexType) || IndexType.both.equals(indexType));
				}

				// NB Don't set the content id as we always want a new one
				cm.put(content, index);
				contentId = content.getContentId();
			}

			// only put conversation in cache if we have been successful in executing
			WebUtil.putConversationInCache(webContext);
			
			// update the content UUID value on the client and popoff the window on the stack
			RequestContext rc = RequestContext.getCurrentInstance();
			StringBuilder js = new StringBuilder(128);
			js.append("top.WindowStack.getOpener()._vm.setValue('").append(contentBinding.replace('.', '_'));
			js.append("','").append(contentId).append("');top.WindowStack.popoff(false);");
	        rc.execute(js.toString());
		}
		catch (Exception e) {
			persistence.rollback();
			e.printStackTrace();
			FacesMessage msg = new FacesMessage("Failure", e.getMessage());
	        fc.addMessage(null, msg);
		}
		finally {
			persistence.commit(true);
		}
    }
}
