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
import org.skyve.impl.web.WebErrorUtil;
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

import jakarta.annotation.Nonnull;
import jakarta.annotation.PostConstruct;
import jakarta.enterprise.context.RequestScoped;
import jakarta.faces.application.FacesMessage;
import jakarta.faces.context.ExternalContext;
import jakarta.faces.context.FacesContext;
import jakarta.inject.Named;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpSession;

/**
 * Used by imageMarkup.xhtml 
 */
@RequestScoped
@Named("_skyveMarkup")
@SuppressWarnings("java:S1192") // Repeated literals are deliberate image markup response fragments.
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
		
	/**
	 * Returns the base URL used by this request context.
	 *
	 * @return base URL
	 */
	@SuppressWarnings("static-method")
	public final String getBaseHref() {
		return Util.getBaseUrl();
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

	/**
	 * Indicates whether the current session has sufficient access to use the markup view.
	 *
	 * @return {@code true} when the user session is present
	 */
	public boolean isCanAccess() {
		return canAccess;
	}

	/**
	 * Returns the cached conversation context parameter.
	 *
	 * @return conversation context parameter, or {@code null}
	 */
	public String getContextParameter() {
		return contextParameter;
	}

	/**
	 * Sets the cached web-context key used to restore conversation state.
	 *
	 * @param contextParameter the conversation context key
	 */
	public void setContextParameter(String contextParameter) {
		this.contextParameter = OWASP.sanitise(Sanitisation.text, UtilImpl.processStringValue(contextParameter));
	}

	/**
	 * Returns the optional bean binding parameter.
	 *
	 * @return bean binding parameter, or {@code null}
	 */
	public String getBindingParameter() {
		return bindingParameter;
	}

	/**
	 * Sets the optional binding path to the bean containing the target content attribute.
	 *
	 * @param bindingParameter the binding path parameter
	 */
	public void setBindingParameter(String bindingParameter) {
		this.bindingParameter = OWASP.sanitise(Sanitisation.text, UtilImpl.processStringValue(bindingParameter));

	}

	/**
	 * Returns the content binding parameter.
	 *
	 * @return content binding parameter, or {@code null}
	 */
	public String getContentBindingParameter() {
		return contentBindingParameter;
	}

	/**
	 * Sets the binding path to the content attribute being marked up.
	 *
	 * @param contentBindingParameter the content binding path
	 */
	public void setContentBindingParameter(String contentBindingParameter) {
		this.contentBindingParameter = OWASP.sanitise(Sanitisation.text, UtilImpl.processStringValue(contentBindingParameter));
	}

	/**
	 * Returns the content identifier parameter.
	 *
	 * @return content ID parameter, or {@code null}
	 */
	public String getContentIdParameter() {
		return contentIdParameter;
	}

	/**
	 * Sets the content identifier used to load and update markup payloads.
	 *
	 * @param contentIdParameter the content ID parameter
	 */
	public void setContentIdParameter(String contentIdParameter) {
		this.contentIdParameter = OWASP.sanitise(Sanitisation.text, UtilImpl.processStringValue(contentIdParameter));
	}

	private String moduleDocument;
	
	/**
	 * The <mod>.<doc> for the content request for Excalidraw (set in process())
	 *
	 * @return the module-document identifier for content retrieval
	 */
	public String getModuleDocument() {
		return moduleDocument;
	}

	private int imageWidth = 800;
	
	/**
	 * The width of the image for Excalidraw (set in process())
	 *
	 * @return the background image width
	 */
	public int getImageWidth() {
		return imageWidth;
	}

	private int imageHeight = 600;
	
	/**
	 * The height of the image for Excalidraw (set in process())
	 *
	 * @return the background image height
	 */
	public int getImageHeight() {
		return imageHeight;
	}

	/**
	 * The URL to serve the raw content image (without markup overlay) as the Excalidraw background.
	 *
	 * @return the content download URL used as the Excalidraw background
	 */
	public String getBackgroundUrl() {
		StringBuilder result = new StringBuilder(64);
		result.append(getBaseHref()).append("content?_nm&_n=").append(contentIdParameter);
		result.append("&_doc=").append(moduleDocument);
		result.append("&_b=").append(BindUtil.unsanitiseBinding(contentBindingParameter));
		return result.toString();
	}

	// The new content ID to place in the edit view if apply is pressed in Excalidraw
	private String newContentId;
	
	/**
	 * Returns the newly-created content ID after apply processing.
	 *
	 * @return newly-created content ID, or {@code null}
	 */
	public String getNewContentId() {
		return newContentId;
	}

	/**
	 * Sets the new content ID created when apply is performed.
	 *
	 * @param newContentId the new persisted content ID
	 */
	public void setNewContentId(String newContentId) {
		this.newContentId = newContentId;
	}

	// The SVG from Excalidraw which is placed in the attachment.
	private String svg;
	
	/**
	 * Returns the SVG payload associated with the current markup session.
	 *
	 * @return SVG payload, or {@code null}
	 */
	public String getSvg() {
		return svg;
	}

	/**
	 * Sets the SVG payload posted from Excalidraw.
	 *
	 * @param svg the SVG markup payload
	 */
	public void setSvg(String svg) {
		this.svg = svg;
	}

	/**
	 * Called from the apply button press.
	 *
	 * @throws Exception when processing or persistence updates fail
	 */
	public void apply() throws Exception {
		process(true);
	}

	/**
	 * Checks well-formed URL, and user has access, then...
	 * Sets the state for rendering the Excalidraw editor and...
	 * If apply is true, makes a new content item from the old and stores the SVG with it.
	 *
	 * @param apply whether to persist posted markup updates
	 * @throws Exception when conversation restore, access checks, or persistence operations fail
	 */
	@SuppressWarnings({"java:S3776", "java:S6541"}) // Complexity OK
	private void process(boolean apply) throws Exception {
		// If there is no access, don't process the upload and return to allow the view to render the no access message
		if (! isCanAccess()) {
			return;
		}

		FacesContext fc = FacesContext.getCurrentInstance();
		if ((contextParameter == null) || (contentBindingParameter == null)) {
			LOGGER.warn("FileUpload - Malformed URL on Upload Action - context or contentBinding is null");
			FacesMessage msg = new FacesMessage("Failure", "Malformed URL");
			fc.addMessage(null, msg);
			return;
		}

		ExternalContext ec = fc.getExternalContext();
		HttpServletRequest request = (HttpServletRequest) ec.getRequest();

		AbstractWebContext webContext = StateUtil.getCachedConversation(contextParameter, request);
		if (webContext == null) {
			LOGGER.warn("FileUpload - Malformed URL on Content Upload - context does not exist");
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
			UxUi uxui = UserAgent.getSelection(request).getUxUi();
			String unsanitisedContentBinding = BindUtil.unsanitiseBinding(contentBindingParameter);
			EXT.checkAccess(user, UserAccess.content(bizModule, bizDocument, unsanitisedContentBinding), uxui.getName());
			
			// Check document access
			Customer customer = user.getCustomer();
			Document document = customer.getModule(bizModule).getDocument(customer, bizDocument);
			if (! user.canAccessDocument(document)) {
				throw new SecurityException("view this document", user.getName());
			}

			try (ContentManager cm = EXT.newContentManager()) {
				moduleDocument = bizModule + '.' + bizDocument;
				AttachmentContent content = cm.getAttachment(contentIdParameter);
				if (content != null) {
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
						AttachmentContent newContent = content.cloneNewForPut();
						newContent.setMarkup(svg);
						// Determine whether we should index the new content by looking at the attribute
						boolean index = false;
						try {
							String contentBizModule = content.getBizModule();
							String contentBizDocument = content.getBizDocument();
							String contentAttributeName = content.getAttributeName();
							Module contentModule = customer.getModule(contentBizModule);
							Document contentDocument = contentModule.getDocument(customer, contentBizDocument);
							Attribute attribute = contentDocument.getAttribute(contentAttributeName);
							if (attribute instanceof Content contentAttribute) {
								IndexType indexType = contentAttribute.getIndex();
								index = (IndexType.textual.equals(indexType) || IndexType.both.equals(indexType));
							}
							else {
								LOGGER.warn("Could not determine whether to index the new marked up content as the attribute {}.{}.{} is not a content attribute.",
												contentBizModule, contentBizDocument, contentAttributeName);
							}
						}
						catch (Exception e) {
							LOGGER.warn("Could not determine whether to index the new marked up content.", e);
						}
						
						// Put the new context
						cm.put(newContent, index);
						newContentId = newContent.getContentId();
		
						// only put conversation in cache if we have been successful in executing
						StateUtil.cacheConversation(webContext);
				
						// update the content UUID value on the client and popoff the window on the stack
						String sanitisedContentBinding = BindUtil.sanitiseBinding(contentBindingParameter);
						if (sanitisedContentBinding != null) { // should never happen
							PrimeFaces.current().executeScript(ImageMarkupView.createMarkupSuccessScript(sanitisedContentBinding, newContentId, bean, content));
						}
					}
				}
			}
		}
		catch (Exception e) {
			persistence.rollback();
			String reference = WebErrorUtil.logUnexpectedAndGetReference(LOGGER, "Image markup processing failed", e);
			FacesMessage msg = new FacesMessage("Failure", WebErrorUtil.genericMessage(reference));
			fc.addMessage(null, msg);
		}
		// NB No need to disconnect Persistence as it is done in the SkyveFacesPhaseListener after the response is rendered.
	}

	@Nonnull static String createMarkupSuccessScript(@Nonnull String sanitisedContentBinding,
														@Nonnull String contentId,
														@Nonnull Bean bean,
														@Nonnull AttachmentContent content) {
		StringBuilder js = new StringBuilder(256);
		js.append("var skyveMarkupWindow=SKYVE.Util.findSkyveWindow();");
		js.append("if(skyveMarkupWindow){");
		// if the owning frame has isc defined then we are using smart client, set the value in the values manager
		js.append("if(skyveMarkupWindow.isc){");
		js.append("if(skyveMarkupWindow.isc.BizUtil&&skyveMarkupWindow.isc.BizUtil.afterMarkupApply){");
		js.append("skyveMarkupWindow.isc.BizUtil.afterMarkupApply('").append(sanitisedContentBinding);
		js.append("','").append(contentId).append("','");
		js.append(bean.getBizModule()).append('.').append(bean.getBizDocument()).append("','");
		js.append(OWASP.escapeJsString(content.getFileName(), false, false)).append("');");
		js.append("}else{");
		js.append("skyveMarkupWindow.isc.WindowStack.getOpener()._vm.setValue('").append(sanitisedContentBinding);
		js.append("','").append(contentId).append("');skyveMarkupWindow.isc.WindowStack.popoff(false)");
		js.append('}');
		// otherwise we are using prime faces, set the hidden input element that ends with "_<binding>"
		js.append("}else if(skyveMarkupWindow.SKYVE){if(skyveMarkupWindow.SKYVE.PF){skyveMarkupWindow.SKYVE.PF.afterMarkupApply('").append(sanitisedContentBinding);
		js.append("','").append(contentId).append("','");
		js.append(bean.getBizModule()).append('.').append(bean.getBizDocument()).append("','");
		js.append(OWASP.escapeJsString(content.getFileName(), false, false)).append("')}}");
		js.append('}');
		return js.toString();
	}
}
