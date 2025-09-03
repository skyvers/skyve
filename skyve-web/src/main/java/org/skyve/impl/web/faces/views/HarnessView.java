package org.skyve.impl.web.faces.views;

import java.util.Set;

import org.skyve.CORE;
import org.skyve.domain.messages.SecurityException;
import org.skyve.impl.cache.StateUtil;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.impl.metadata.user.UserImpl;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.AbstractWebContext;
import org.skyve.impl.web.WebUtil;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.repository.ProvidedRepository;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.TextOutput.Sanitisation;
import org.skyve.metadata.view.View.ViewType;
import org.skyve.report.ReportFormat;
import org.skyve.util.OWASP;
import org.skyve.util.Util;
import org.skyve.web.WebAction;
import org.skyve.web.WebContext;

import jakarta.annotation.Nullable;
import jakarta.faces.FacesException;
import jakarta.faces.context.ExternalContext;
import jakarta.faces.context.FacesContext;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpSession;

/**
 * Extension point that establishes state for the running Skyve app Faces Views.
 * Note that no methods should be final in here as a bean could be injected and a proxy needs to be made.
 */
public abstract class HarnessView extends LocalisableView {
	private static final long serialVersionUID = 2805839690076647L;

	private String logoRelativeFileNameUrl;
	public String getLogoRelativeFileNameUrl() {
		return logoRelativeFileNameUrl;
	}

	private String cssRelativeFileNameUrl;
	public String getCssRelativeFileNameUrl() {
		return cssRelativeFileNameUrl;
	}
	
	private String bizModuleParameter;
	public String getBizModuleParameter() {
		return bizModuleParameter;
	}
	public void setBizModuleParameter(String bizModuleParameter) {
		this.bizModuleParameter = OWASP.sanitise(Sanitisation.text, Util.processStringValue(bizModuleParameter));
	}

	private String bizDocumentParameter;
	public String getBizDocumentParameter() {
		return bizDocumentParameter;
	}
	public void setBizDocumentParameter(String bizDocumentParameter) {
		this.bizDocumentParameter = OWASP.sanitise(Sanitisation.text, Util.processStringValue(bizDocumentParameter));
	}
	
	private String queryNameParameter;
	public String getQueryNameParameter() {
		return queryNameParameter;
	}
	public void setQueryNameParameter(String queryNameParameter) {
		this.queryNameParameter = OWASP.sanitise(Sanitisation.text, Util.processStringValue(queryNameParameter));
	}

	private String bizIdParameter;
	public String getBizIdParameter() {
		return bizIdParameter;
	}
	public void setBizIdParameter(String bizIdParameter) {
		this.bizIdParameter = OWASP.sanitise(Sanitisation.text, Util.processStringValue(bizIdParameter));
	}

	private WebAction webActionParameter;
	public WebAction getWebActionParameter() {
		return webActionParameter;
	}
	public void setWebActionParameter(WebAction webActionParameter) {
		this.webActionParameter = webActionParameter;
	}

	private ViewType viewType;
	public ViewType getViewType() {
		return viewType;
	}

	@SuppressWarnings("static-method")
	public String getSkyveVersionComment() {
		StringBuilder result = new StringBuilder(64);
		result.append("<!-- SKYVE FRAMEWORK version is ").append(UtilImpl.SKYVE_VERSION).append(" -->");
		return result.toString();
	}
	
	private String apiScript;
	public String getApiScript() {
		return apiScript;
	}
	
	@SuppressWarnings("static-method")
	public String getBaseHref() {
		return Util.getBaseUrl();
	}

	@SuppressWarnings("static-method")
	public String getMapType() {
		return UtilImpl.MAP_TYPE.toString();
	}

	private String userContactInitials;
	public String getUserContactInitials() {
		return userContactInitials;
	}

	private String userContactImageUrl;
	public String getUserContactImageUrl() {
		return userContactImageUrl;
	}

	private String userContactName;
	public String getUserContactName() {
		return userContactName;
	}

	private String userName;
	public String getUserName() {
		return userName;
	}

	@Override
	public void initialise() {
		super.initialise();

		User user = CORE.getUser();
		if (user == null) {
			return;
		}
	
		userContactImageUrl = user.getContactImageUrl(52, 52);
		userContactInitials = user.getContactAvatarInitials();
		userContactName = user.getContactName();
		userName = user.getName();
		
		Customer customer = user.getCustomer();
		
		StringBuilder sb = new StringBuilder(64);
		sb.append("resources?_n=");
		sb.append(customer.getUiResources().getLogoRelativeFileName());
		logoRelativeFileNameUrl = sb.toString();

		if (bizModuleParameter == null) {
			Set<String> moduleNames = user.getAccessibleModuleNames();
			if (moduleNames.size() == 0) {
				throw new SecurityException("any module", customer.getName() + '/' + user.getName());
			}
			Module homeModule = null;
			bizModuleParameter = user.getHomeModuleName();
			if (bizModuleParameter != null) {
				if (moduleNames.contains(bizModuleParameter)) {
					homeModule = customer.getModule(bizModuleParameter);
				}
			}
			if (homeModule == null) {
				homeModule = customer.getHomeModule();
				bizModuleParameter = homeModule.getName();
				if (! moduleNames.contains(bizModuleParameter)) {
					homeModule = null;
				}
			}
			if (homeModule == null) {
				bizModuleParameter = moduleNames.iterator().next();
				homeModule = customer.getModule(bizModuleParameter);
			}
			bizDocumentParameter = homeModule.getHomeDocumentName();
			
			viewType = homeModule.getHomeRef();
			
			if (ViewType.edit.equals(viewType)) {
				webActionParameter = WebAction.e;

				// Set to document owning module in case the home document is imported in the home module
				Document homeDocument = homeModule.getDocument(customer, bizDocumentParameter);
				bizModuleParameter = homeDocument.getOwningModuleName();
			}
			else {
				if (queryNameParameter == null) {
					queryNameParameter = bizDocumentParameter;
				}
				bizDocumentParameter = null;
				webActionParameter = WebAction.l;
			}
		}
		else {
			if (webActionParameter == null) {
				webActionParameter = (queryNameParameter != null) ? WebAction.l : WebAction.e;
			}
		}

		// Protected bizModule, bizDocument, query from URL tampering
		try {
			if (bizModuleParameter != null) {
				Module m = customer.getModule(bizModuleParameter);
				Document d = null;
				if (bizDocumentParameter != null) {
					d = m.getDocument(customer, bizDocumentParameter);
				}
				if (queryNameParameter != null) {
					if (! queryNameParameter.equals(bizDocumentParameter)) {
						if (m.getMetaDataQuery(queryNameParameter) == null) {
							if (d == null) {
								if (m.getDocument(customer, queryNameParameter) == null) {
									throw new MetaDataException("Query name " + queryNameParameter + " does not exist for module " + bizModuleParameter);
								}
							}
							else {
								if (d.getListModel(customer, queryNameParameter, false) == null) {
									throw new MetaDataException("Model name " + queryNameParameter + " does not exist for module " + bizModuleParameter);
								}
							}
						}
					}
				}
			}
		}
		catch (Exception e) {
			throw new FacesException("Malformed URL", e);
		}
		
		String cssRelativeFileName = customer.getHtmlResources().getCssRelativeFileName();
		if (cssRelativeFileName != null) {
			sb.setLength(0);
			sb.append("resources?_n=").append(cssRelativeFileName);
			cssRelativeFileNameUrl = sb.toString();
		}
		else {
			sb.setLength(0);
			sb.append("skyve/css/basic-min.css?v=").append(UtilImpl.WEB_RESOURCE_FILE_VERSION);
			cssRelativeFileNameUrl = sb.toString();
		}
		
		sb.setLength(0);
		sb.append("var u=SKYVE.Util;u.setTouchCookie();u.customer='").append(customer.getName()).append("';");
		sb.append("u.v='").append(UtilImpl.WEB_RESOURCE_FILE_VERSION).append("';");
		sb.append("u.canFlag=").append(user.canFlag()).append(";");
		if (UtilImpl.GOOGLE_MAPS_V3_API_KEY == null) {
			sb.append("u.googleMapsV3ApiKey=null;");
		}
		else {
			sb.append("u.googleMapsV3ApiKey='").append(UtilImpl.GOOGLE_MAPS_V3_API_KEY).append("';");
		}
		sb.append("u.mapLayers=\"").append(UtilImpl.MAP_LAYERS).append("\";");
		if (UtilImpl.MAP_CENTRE != null) {
			sb.append("u.mapCentre='").append(UtilImpl.MAP_CENTRE).append("';");
		}
		if (UtilImpl.MAP_ZOOM != 1) {
			sb.append("u.mapZoom=").append(UtilImpl.MAP_ZOOM).append(';');
		}
		sb.append("u.ckEditorConfigFileUrl='").append(UtilImpl.CKEDITOR_CONFIG_FILE_URL).append("';");

		// Set report formats for export from Customisations
		sb.append("u.allowedReportFormats=[");
		for (ReportFormat format : CORE.getCustomisations().listGridExportFormats()) {
			sb.append('\'').append(format.name()).append("',");
		}
		sb.setLength(sb.length() - 1); // remove last comma
		sb.append("];");

		apiScript = sb.toString();
	}
	
	@SuppressWarnings("static-method")
	public @Nullable User getUser() {
		ExternalContext ec = FacesContext.getCurrentInstance().getExternalContext();
		return (User) ec.getSessionMap().get(WebContext.USER_SESSION_ATTRIBUTE_NAME);
	}
	
	@SuppressWarnings("static-method")
	public void setUser(String customerName, String userName) {
		UserImpl user = null;
		ProvidedRepository repository = ProvidedRepositoryFactory.get();
		ExternalContext ec = FacesContext.getCurrentInstance().getExternalContext();
		String userPrincipal = null;
		if (ec.getUserPrincipal() == null) { // not logged in
			userPrincipal = new StringBuilder(64).append(customerName).append('/').append(userName).toString();
		}
		else {
			userPrincipal = ec.getUserPrincipal().toString();
		}
		user = repository.retrieveUser(userPrincipal);
		HttpServletRequest request = (HttpServletRequest) ec.getRequest();
		HttpSession session = request.getSession(true);
		if (user != null) {
			WebUtil.setSessionId(user, request);
			session.setAttribute(WebContext.USER_SESSION_ATTRIBUTE_NAME, user);
			StateUtil.addSession(user.getId(), session);
		}

		AbstractPersistence persistence = AbstractPersistence.get();
		persistence.setUser(user);
	}
	
	public boolean isCanTextSearch() {
		User u = getUser();
		return ((u != null) && (u.canTextSearch()));
	}
	
	public boolean isCanSwitchMode() {
		User u = getUser();
		return ((u != null) && (u.canSwitchMode()));
	}
	
	/**
	 * Sets the UX/UI preference in the session.
	 * @param uxui	The UX/UI name.
	 */
	public void setUxUi(String uxui) {
		if (! isCanSwitchMode()) {
			User u = getUser();
			throw new SecurityException("switch modes", (u == null) ? "anonymous" : u.getName());
		}
		
		ExternalContext ec = FacesContext.getCurrentInstance().getExternalContext();
		if (uxui == null) {
			ec.getSessionMap().remove(AbstractWebContext.UXUI);
		}
		else {
			ec.getSessionMap().put(AbstractWebContext.UXUI, uxui);
		}
	}
}
