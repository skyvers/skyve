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
import org.skyve.CORE;
import org.skyve.content.AttachmentContent;
import org.skyve.domain.Bean;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.user.UserImpl;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.AbstractWebContext;
import org.skyve.impl.web.WebUtil;
import org.skyve.impl.web.faces.FacesAction;
import org.skyve.persistence.Persistence;

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

		AbstractWebContext webContext = WebUtil.getCachedConversation(context, request, response);
		if (webContext == null) {
			UtilImpl.LOGGER.warning("FileUpload - Malformed URL on Content Upload - context does not exist");
			FacesMessage msg = new FacesMessage("Failure", "Malformed URL");
	        FacesContext.getCurrentInstance().addMessage(null, msg);
	        return;
		}

		// NB Persistence has been set with the restore processing inside the SkyvePhaseListener
		Persistence persistence = CORE.getPersistence();
		try {
			Bean currentBean = webContext.getCurrentBean();
			Bean bean = currentBean;

			if (binding != null) {
				bean = (Bean) BindUtil.get(bean, binding);
			}
			
			AttachmentContent content = FacesContentUtil.handleFileUpload(event, bean, BindUtil.unsanitiseBinding(contentBinding));
			String contentId = content.getContentId();

			// only put conversation in cache if we have been successful in executing
			WebUtil.putConversationInCache(webContext);
			
			// update the content UUID value on the client and popoff the window on the stack
			RequestContext rc = RequestContext.getCurrentInstance();
			StringBuilder js = new StringBuilder(128);
			String sanitisedContentBinding = BindUtil.sanitiseBinding(contentBinding);
			// if top.isc is defined then we are using smart client, set the value in the values manager
			js.append("if(top.isc){");
			js.append("top.isc.WindowStack.getOpener()._vm.setValue('").append(sanitisedContentBinding);
			js.append("','").append(contentId).append("');top.isc.WindowStack.popoff(false)");
			// otherwise we are using prime faces, set the hidden input element that ends with "_<binding>"
			js.append("}else if(top.SKYVE){top.SKYVE.afterContentUpload('").append(sanitisedContentBinding);
			js.append("','").append(contentId).append("','");
			js.append(bean.getBizModule()).append('.').append(bean.getBizDocument()).append("','");
			js.append(content.getFileName()).append("')}");
	        rc.execute(js.toString());
		}
		catch (Exception e) {
			persistence.rollback();
			e.printStackTrace();
			FacesMessage msg = new FacesMessage("Failure", e.getMessage());
	        fc.addMessage(null, msg);
		}
		// NB No need to disconnect Persistence as it is done in the SkyvePhaseListener after the response is rendered.
    }
}
