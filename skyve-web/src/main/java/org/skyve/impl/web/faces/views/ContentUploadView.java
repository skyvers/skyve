package org.skyve.impl.web.faces.views;

import org.apache.commons.codec.binary.Base64;
import org.primefaces.PrimeFaces;
import org.primefaces.event.FileUploadEvent;
import org.primefaces.model.file.UploadedFile;
import org.skyve.CORE;
import org.skyve.content.AttachmentContent;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.SecurityException;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.cache.StateUtil;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.AbstractWebContext;
import org.skyve.impl.web.UserAgent;
import org.skyve.impl.web.faces.FacesAction;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.router.UxUi;
import org.skyve.metadata.user.User;
import org.skyve.metadata.user.UserAccess;
import org.skyve.metadata.view.TextOutput.Sanitisation;
import org.skyve.persistence.Persistence;
import org.skyve.util.OWASP;

import jakarta.annotation.PostConstruct;
import jakarta.enterprise.context.RequestScoped;
import jakarta.faces.annotation.ManagedProperty;
import jakarta.faces.application.FacesMessage;
import jakarta.faces.context.ExternalContext;
import jakarta.faces.context.FacesContext;
import jakarta.inject.Inject;
import jakarta.inject.Named;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

@RequestScoped
@Named("_skyveContent")
public class ContentUploadView extends AbstractUploadView {
	private static final long serialVersionUID = -6769960348990922565L;

	@Inject
	@ManagedProperty(value = "#{param." + AbstractWebContext.RESOURCE_FILE_NAME + "}")
	private String contentBinding;

	private String croppedDataUrl;
	private String croppedFileName;

	public ContentUploadView() {
		super(UtilImpl.UPLOADS_CONTENT_WHITELIST_REGEX, UtilImpl.UPLOADS_CONTENT_MAXIMUM_SIZE_IN_MB);
	}

	protected ContentUploadView(String whitelistRegex, int maximumSizeMB) {
		super(whitelistRegex, maximumSizeMB);
	}

	@Override
	@PostConstruct
	public void postConstruct() {
		super.postConstruct();
		contentBinding = OWASP.sanitise(Sanitisation.text, UtilImpl.processStringValue(contentBinding));
	}
	
	public void preRender() {
		new FacesAction<Void>() {
			@Override
			public Void callback() throws Exception {
				initialise();
				return null;
			}
		}.execute();
	}

	public String getContentBinding() {
		return contentBinding;
	}

	public String getCroppedDataUrl() {
		return croppedDataUrl;
	}

	public void setCroppedDataUrl(String croppedDataUrl) {
		this.croppedDataUrl = croppedDataUrl;
	}

	public String getCroppedFileName() {
		return croppedFileName;
	}

	public void setCroppedFileName(String croppedFileName) {
		this.croppedFileName = croppedFileName;
	}

	/**
	 * Process the file upload directly from the PF file upload component
	 * 
	 * @param event
	 */
	public void handleFileUpload(FileUploadEvent event) throws Exception {
		FacesContext fc = FacesContext.getCurrentInstance();
		UploadedFile file = event.getFile();
		if (! validFile(file, fc)) {
			return;
		}
		upload(file.getFileName(), file.getContent(), fc);
	}

	/**
	 * Process the file upload from the croppie plugin.
	 * For use as a remote command with a hidden populated with a data url
	 */
	public void uploadCropped() throws Exception {
		FacesContext fc = FacesContext.getCurrentInstance();
		String base64 = croppedDataUrl.substring(croppedDataUrl.indexOf(','));
		Base64 base64Codec = new Base64();
		upload(croppedFileName, base64Codec.decode(base64), fc);
	}

	/**
	 * Does the upload work.
	 * This method does not use FacesAction because it should show errors/growls under all circumstances
	 * since the upload pages are embedded in iframes.
	 */
	private void upload(String fileName, byte[] fileContents, FacesContext fc) throws Exception {
		// If there is no access, don't process the upload and return to allow the view to render the no access message
		if (! isCanAccess()) {
			return;
		}

		String context = getContext();
		if ((context == null) || (contentBinding == null)) {
			UtilImpl.LOGGER.warning("FileUpload - Malformed URL on Upload Action - context or contentBinding is null");
			FacesMessage msg = new FacesMessage("Failure", "Malformed URL");
			fc.addMessage(null, msg);
			return;
		}

		ExternalContext ec = fc.getExternalContext();
		HttpServletRequest request = (HttpServletRequest) ec.getRequest();
		HttpServletResponse response = (HttpServletResponse) ec.getResponse();

		AbstractWebContext webContext = StateUtil.getCachedConversation(context, request, response);
		if (webContext == null) {
			UtilImpl.LOGGER.warning("FileUpload - Malformed URL on Content Upload - context does not exist");
			FacesMessage msg = new FacesMessage("Failure", "Malformed URL");
			FacesContext.getCurrentInstance().addMessage(null, msg);
			return;
		}

		// NB Persistence has been set with the restore processing inside the SkyveFacesPhaseListener
		Persistence persistence = CORE.getPersistence();
		try {
			Bean currentBean = webContext.getCurrentBean();
			Bean bean = currentBean;

			String binding = getBinding();
			if (binding != null) {
				bean = (Bean) BindUtil.get(bean, binding);
			}
			if (bean == null) { // should never happen
				throw new IllegalStateException("bean is null");
			}
			
			// Check content access
			User user = persistence.getUser();
			String bizModule = bean.getBizModule();
			String bizDocument = bean.getBizDocument();
			UxUi uxui = UserAgent.getUxUi(request);
			String unsanitisedContentBinding = BindUtil.unsanitiseBinding(contentBinding);
			user.checkAccess(UserAccess.content(bizModule, bizDocument, unsanitisedContentBinding), uxui.getName());

			// Check document access
			Customer customer = user.getCustomer();
			Document document = customer.getModule(bizModule).getDocument(customer, bizDocument);
			if (! user.canAccessDocument(document)) {
				throw new SecurityException("view this document", user.getName());
			}
			
			// Add to content 
			// NB This handles compound bindings and checks for content access on the content owning bean
			AttachmentContent content = FacesContentUtil.handleFileUpload(fileName, fileContents, bean, unsanitisedContentBinding);		
			String contentId = content.getContentId();

			// only put conversation in cache if we have been successful in executing
			StateUtil.cacheConversation(webContext);

			// update the content UUID value on the client and popoff the window on the stack
			StringBuilder js = new StringBuilder(128);
			String sanitisedContentBinding = BindUtil.sanitiseBinding(contentBinding);
			// if window.parent.isc is defined then we are using smart client, set the value in the values manager
			js.append("if(window.parent.isc){");
			js.append("window.parent.isc.WindowStack.getOpener()._vm.setValue('").append(sanitisedContentBinding);
			js.append("','").append(contentId).append("');window.parent.isc.WindowStack.popoff(false)");
			// otherwise we are using prime faces, set the hidden input element that ends with "_<binding>"
			// NB Cannot use window.parent here to support nested frames as the script is executed at the top window context.
			js.append("}else if(top.SKYVE){if(top.SKYVE.PF){top.SKYVE.PF.afterContentUpload('").append(sanitisedContentBinding);
			js.append("','").append(contentId).append("','");
			js.append(bean.getBizModule()).append('.').append(bean.getBizDocument()).append("','");
			js.append(OWASP.escapeJsString(content.getFileName(), false, false)).append("')}}");
			PrimeFaces.current().executeScript(js.toString());
		}
		catch (Exception e) {
			persistence.rollback();
			e.printStackTrace();
			FacesMessage msg = new FacesMessage("Failure", e.getMessage());
			fc.addMessage(null, msg);
		}
		// NB No need to disconnect Persistence as it is done in the SkyveFacesPhaseListener after the response is rendered.
	}
}
