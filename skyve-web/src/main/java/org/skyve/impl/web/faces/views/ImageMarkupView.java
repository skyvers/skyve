package org.skyve.impl.web.faces.views;

import java.awt.image.BufferedImage;
import java.io.InputStream;

import org.primefaces.PrimeFaces;
import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.content.AttachmentContent;
import org.skyve.content.ContentManager;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.SecurityException;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.cache.StateUtil;
import org.skyve.impl.metadata.model.document.field.Content;
import org.skyve.impl.metadata.model.document.field.Field.IndexType;
import org.skyve.impl.util.ImageUtil;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.AbstractWebContext;
import org.skyve.impl.web.UserAgent;
import org.skyve.impl.web.faces.FacesAction;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.router.UxUi;
import org.skyve.metadata.user.User;
import org.skyve.metadata.user.UserAccess;
import org.skyve.metadata.view.TextOutput.Sanitisation;
import org.skyve.persistence.Persistence;
import org.skyve.util.OWASP;
import org.skyve.util.Util;
import org.skyve.web.WebContext;

import jakarta.annotation.PostConstruct;
import jakarta.enterprise.context.RequestScoped;
import jakarta.faces.application.FacesMessage;
import jakarta.faces.context.ExternalContext;
import jakarta.faces.context.FacesContext;
import jakarta.inject.Named;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.servlet.http.HttpSession;

/**
 * Used by imageMarkup.xhtml 
 */
@RequestScoped
@Named("_skyveMarkup")
public class ImageMarkupView extends LocalisableView {
	private static final long serialVersionUID = 4573644429247936306L;

	private boolean canAccess = false;
	
	private String contextParameter; // _c
	private String bindingParameter; // _b
	private String contentBindingParameter; // _n
	private String contentIdParameter; // _id

	/**
	 * Setup the no-access banner on render if appropriate
	 */
	@PostConstruct
	public void postConstruct() {
		HttpSession session = (HttpSession) FacesContext.getCurrentInstance().getExternalContext().getSession(false);
		canAccess = (session != null) && (session.getAttribute(WebContext.USER_SESSION_ATTRIBUTE_NAME) != null);
		
	}
		
	@SuppressWarnings("static-method")
	public final String getBaseHref() {
		return Util.getSkyveContextUrl() + '/';
	}

	/**
	 * Initialise and setup the iframe URL to display the content as the background image
	 */
	public void preRender() {
		new FacesAction<Void>() {
			@Override
			public Void callback() throws Exception {
				if (! FacesContext.getCurrentInstance().isPostback()) {
					initialise();
					process(false);
				}
				return null;
			}
		}.execute();
	}

	public boolean isCanAccess() {
		return canAccess;
	}

	public String getContextParameter() {
		return contextParameter;
	}

	public void setContextParameter(String contextParameter) {
		this.contextParameter = OWASP.sanitise(Sanitisation.text, UtilImpl.processStringValue(contextParameter));
	}

	public String getBindingParameter() {
		return bindingParameter;
	}

	public void setBindingParameter(String bindingParameter) {
		this.bindingParameter = OWASP.sanitise(Sanitisation.text, UtilImpl.processStringValue(bindingParameter));

	}

	public String getContentBindingParameter() {
		return contentBindingParameter;
	}

	public void setContentBindingParameter(String contentBindingParameter) {
		this.contentBindingParameter = OWASP.sanitise(Sanitisation.text, UtilImpl.processStringValue(contentBindingParameter));
	}

	public String getContentIdParameter() {
		return contentIdParameter;
	}

	public void setContentIdParameter(String contentIdParameter) {
		this.contentIdParameter = OWASP.sanitise(Sanitisation.text, UtilImpl.processStringValue(contentIdParameter));
	}

	private String moduleDocument;
	
	/**
	 * The <mod>.<doc> for the content request for SVG-Edit (set in process())
	 */
	public String getModuleDocument() {
		return moduleDocument;
	}

	private int imageWidth = 800;
	
	/**
	 * The width of the image for SVG-Edit (set in process())
	 */
	public int getImageWidth() {
		return imageWidth;
	}

	private int imageHeight = 600;
	
	/**
	 * The height of the image for SVG-Edit (set in process())
	 */
	public int getImageHeight() {
		return imageHeight;
	}
	
	// The new content ID to place in the edit view if apply is pressed in SVG-Edit
	private String newContentId;
	
	public String getNewContentId() {
		return newContentId;
	}

	public void setNewContentId(String newContentId) {
		this.newContentId = newContentId;
	}

	// The SVG from SVG-Edit which is placed in the attachment.
	private String svg;
	
	public String getSvg() {
		return svg;
	}

	public void setSvg(String svg) {
		this.svg = svg;
	}

	/**
	 * Called from the apply button press.
	 */
	public void apply() throws Exception {
		process(true);
	}

	/**
	 * Checks well-formed URL, and user has access, then...
	 * Sets the state for the rendering of the SVG-Edit iframe URL and...
	 * If apply is true, makes a new content item from the old and stores the SVG with it.
	 */
	private void process(boolean apply) throws Exception {
		// If there is no access, don't process the upload and return to allow the view to render the no access message
		if (! isCanAccess()) {
			return;
		}

		FacesContext fc = FacesContext.getCurrentInstance();
		if ((contextParameter == null) || (contentBindingParameter == null)) {
			UtilImpl.LOGGER.warning("FileUpload - Malformed URL on Upload Action - context or contentBinding is null");
			FacesMessage msg = new FacesMessage("Failure", "Malformed URL");
			fc.addMessage(null, msg);
			return;
		}

		ExternalContext ec = fc.getExternalContext();
		HttpServletRequest request = (HttpServletRequest) ec.getRequest();
		HttpServletResponse response = (HttpServletResponse) ec.getResponse();

		AbstractWebContext webContext = StateUtil.getCachedConversation(contextParameter, request, response);
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

			if (bindingParameter != null) {
				bean = (Bean) BindUtil.get(bean, bindingParameter);
			}
			if (bean == null) { // should never happen
				throw new IllegalStateException("bean is null");
			}
			
			// Check content access
			User user = persistence.getUser();
			String bizModule = bean.getBizModule();
			String bizDocument = bean.getBizDocument();
			UxUi uxui = UserAgent.getUxUi(request);
			// Ensure no UXUI is set in the request for this page (after UserAgent.getUxUi() call above)
			// This was it will default to what is in the web.xml theme expression
			request.removeAttribute(AbstractWebContext.UXUI);
			String unsanitisedContentBinding = BindUtil.unsanitiseBinding(contentBindingParameter);
			user.checkAccess(UserAccess.content(bizModule, bizDocument, unsanitisedContentBinding), uxui.getName());

			
			// Check document access
			Customer customer = user.getCustomer();
			Document document = customer.getModule(bizModule).getDocument(customer, bizDocument);
			if (! user.canAccessDocument(document)) {
				throw new SecurityException("view this document", user.getName());
			}

			try (ContentManager cm = EXT.newContentManager()) {
				AttachmentContent content = cm.getAttachment(contentIdParameter);
				if (content != null) {
					moduleDocument = content.getBizModule() + '.' + content.getBizDocument();
					// Set the SVG if we are rendering, but if we are applying we want the SVG sent in the request
					if (! apply) {
						svg = content.getMarkup();
					}
					BufferedImage image = null;
					try (InputStream is = content.getContentStream()) {
						image = ImageUtil.read(is, UtilImpl.THUMBNAIL_SUBSAMPLING_MINIMUM_TARGET_SIZE);
						if (image != null) {
							imageWidth = image.getWidth();
							imageHeight = image.getHeight();
						}
					}

					if (apply) {
						// Add a new content with the markup in it.
						// This ensures that if cancel is pressed the old content will remain linked.
						String contentBizModule = content.getBizModule();
						String contentBizDocument = content.getBizDocument();
						String contentAttributeName = content.getAttributeName();
						AttachmentContent newContent = new AttachmentContent(content.getBizCustomer(), 
																				contentBizModule, 
																				contentBizDocument, 
																				content.getBizDataGroupId(), 
																				content.getBizUserId(),
																				content.getBizId(),
																				contentAttributeName,
																				content.getFileName(),
																				content.getMimeType(),
																				content.getContentBytes(),
																				ImageUtil.cleanseSVGEdit(svg));

						// Determine whether we should index the new content by looking at the attribute
						boolean index = false;
						try {
							Module contentModule = customer.getModule(contentBizModule);
							Document contentDocument = contentModule.getDocument(customer, contentBizDocument);
							Attribute attribute = contentDocument.getAttribute(contentAttributeName);
							if (attribute instanceof Content) {
								Content contentAttribute = (Content) attribute;
								IndexType indexType = contentAttribute.getIndex();
								index = (IndexType.textual.equals(indexType) || IndexType.both.equals(indexType));
							}
							else {
								UtilImpl.LOGGER.warning("Could not determine whether to index the new marked up content as the attribute " + 
															contentBizModule + '.' + contentBizDocument + '.' + contentAttributeName + " is not a content attribute.");
							}
						}
						catch (Exception e) {
							UtilImpl.LOGGER.warning("Could not determine whether to index the new marked up content.");
							e.printStackTrace();
						}
						
						// Put the new context
						cm.put(newContent, index);
						newContentId = newContent.getContentId();
		
						// only put conversation in cache if we have been successful in executing
						StateUtil.cacheConversation(webContext);
			
						// update the content UUID value on the client and popoff the window on the stack
						StringBuilder js = new StringBuilder(128);
						String sanitisedContentBinding = BindUtil.sanitiseBinding(contentBindingParameter);
						// if window.parent.isc is defined then we are using smart client, set the value in the values manager
						js.append("if(window.parent.isc){");
						js.append("window.parent.isc.WindowStack.getOpener()._vm.setValue('").append(sanitisedContentBinding);
						js.append("','").append(newContentId).append("');window.parent.isc.WindowStack.popoff(false)");
						// otherwise we are using prime faces, set the hidden input element that ends with "_<binding>"
						// NB Cannot use window.parent here to support nested frames as the script is executed at the top window context.
						js.append("}else if(top.SKYVE){if(top.SKYVE.PF){top.SKYVE.PF.afterMarkupApply('").append(sanitisedContentBinding);
						js.append("','").append(newContentId).append("','");
						js.append(bean.getBizModule()).append('.').append(bean.getBizDocument()).append("','");
						js.append(OWASP.escapeJsString(content.getFileName(), false, false)).append("')}}");
						PrimeFaces.current().executeScript(js.toString());
					}
				}
			}
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
