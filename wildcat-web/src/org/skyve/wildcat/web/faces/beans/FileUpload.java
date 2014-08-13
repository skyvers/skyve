package org.skyve.wildcat.web.faces.beans;

import java.text.DecimalFormat;

import javax.faces.application.FacesMessage;
import javax.faces.bean.ManagedBean;
import javax.faces.bean.ManagedProperty;
import javax.faces.bean.RequestScoped;
import javax.faces.context.ExternalContext;
import javax.faces.context.FacesContext;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.primefaces.event.FileUploadEvent;
import org.primefaces.model.UploadedFile;
import org.skyve.content.MimeType;
import org.skyve.domain.Bean;
import org.skyve.metadata.controller.UploadAction;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.web.WebContext;
import org.skyve.wildcat.bind.BindUtil;
import org.skyve.wildcat.metadata.repository.AbstractRepository;
import org.skyve.wildcat.persistence.AbstractPersistence;
import org.skyve.wildcat.util.ThreadSafeFactory;
import org.skyve.wildcat.util.UtilImpl;
import org.skyve.wildcat.web.AbstractWebContext;
import org.skyve.wildcat.web.WebUtil;

@ManagedBean
@RequestScoped
public class FileUpload {
	@ManagedProperty(value = "#{param." + AbstractWebContext.CONTEXT_NAME + "}")
    private String context;
    
    @ManagedProperty(value = "#{param." + AbstractWebContext.BINDING_NAME + "}")
    private String binding;

    @ManagedProperty(value = "#{param." + AbstractWebContext.ACTION_NAME + "}")
    private String action;

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

	public String getAction() {
		return action;
	}

	public void setAction(String action) {
		this.action = UtilImpl.processStringValue(action);
	}

	/**
	 * Process the file upload
	 * 
	 * @param event
	 */
	public void handleFileUpload(FileUploadEvent event)
	throws Exception {
		FacesContext fc = FacesContext.getCurrentInstance();

		if ((context == null) || (action == null)) {
			UtilImpl.LOGGER.warning("FileUpload - Malformed URL on Upload Action - context, binding, or action is null");
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
			AbstractRepository repository = AbstractRepository.get();
			Customer customer = user.getCustomer();
	
			AbstractWebContext webContext = WebUtil.getCachedConversation(context, request, response);
			if (webContext == null) {
				UtilImpl.LOGGER.warning("FileUpload - Malformed URL on Upload Action - context does not exist");
				FacesMessage msg = new FacesMessage("Failure", "Malformed URL");
		        FacesContext.getCurrentInstance().addMessage(null, msg);
		        return;
			}
	
			Bean currentBean = webContext.getCurrentBean();
			Bean bean = currentBean;

			if (binding != null) {
				bean = (Bean) BindUtil.get(bean, binding);
			}

			Module module = customer.getModule(bean.getBizModule());
			Document document = module.getDocument(customer, bean.getBizDocument());
			
			UploadAction<Bean> uploadAction = repository.getUploadAction(customer, 
																			document, 
																			action);
			MimeType mimeType = null;
			try {
				MimeType.valueOf(file.getContentType());
			}
			catch (Exception e) {
				// do nothing
			}
			UploadAction.UploadedFile bizFile = 
					new UploadAction.UploadedFile(file.getFileName(),
													file.getInputstream(),
													mimeType);
			uploadAction.upload(bean, bizFile, webContext);
			
			// only put conversation in cache if we have been successful in executing
			WebUtil.putConversationInCache(webContext);

			long size = file.getSize();
	        StringBuilder message = new StringBuilder(128);
	        message.append(file.getFileName()).append(" is uploaded. File Size is ");

			DecimalFormat format = ThreadSafeFactory.getDecimalFormat("###,##0.00");
			if (size > 1048576) {
	            message.append(format.format(size / 1048576.0)).append(" MB");
	        }
	        else {
	            message.append(format.format(size / 1024.0)).append(" KB");
	        }

	        FacesMessage msg = new FacesMessage("Successful", message.toString());
	        FacesContext.getCurrentInstance().addMessage(null, msg);
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
