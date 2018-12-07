package org.skyve.impl.web.faces.beans;

import java.util.Locale;
import java.util.Set;

import javax.faces.context.ExternalContext;
import javax.faces.context.FacesContext;

import org.skyve.impl.domain.messages.SecurityException;
import org.skyve.impl.metadata.repository.AbstractRepository;
import org.skyve.impl.metadata.user.UserImpl;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.AbstractWebContext;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.View.ViewType;
import org.skyve.util.Util;
import org.skyve.web.WebAction;
import org.skyve.web.WebContext;

public abstract class Harness extends Localisable {
	private static final long serialVersionUID = 2805839690076647L;

	private String logoRelativeFileNameUrl;
	public final String getLogoRelativeFileNameUrl() {
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
		this.bizModuleParameter = bizModuleParameter;
	}

	private String bizDocumentParameter;
	public String getBizDocumentParameter() {
		return bizDocumentParameter;
	}
	public void setBizDocumentParameter(String bizDocumentParameter) {
		this.bizDocumentParameter = bizDocumentParameter;
	}
	
	private String queryNameParameter;
	public String getQueryNameParameter() {
		return queryNameParameter;
	}
	public void setQueryNameParameter(String queryNameParameter) {
		this.queryNameParameter = queryNameParameter;
	}

	private String bizIdParameter;
	public String getBizIdParameter() {
		return bizIdParameter;
	}
	public void setBizIdParameter(String bizIdParameter) {
		this.bizIdParameter = bizIdParameter;
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
	public final String getSkyveVersionComment() {
		StringBuilder result = new StringBuilder(64);
		result.append("<!-- SKYVE FRAMEWORK version is ").append(UtilImpl.SKYVE_VERSION).append(" -->");
		return result.toString();
	}
	
	@SuppressWarnings("static-method")
	public final String getWebResourceFileVersion() {
		return UtilImpl.WEB_RESOURCE_FILE_VERSION;
	}
	
	@SuppressWarnings("static-method")
	public final String getBaseHref() {
		return Util.getSkyveContextUrl() + '/';
	}
	
	public final void initialise(Customer customer, UserImpl user, Locale requestLocale) {
		super.initialise(user, requestLocale);
		
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
		
		String cssRelativeFileName = customer.getHtmlResources().getCssRelativeFileName();
		if (cssRelativeFileName != null) {
			sb.setLength(0);
			sb.append("resources?_n=").append(cssRelativeFileName);
			cssRelativeFileNameUrl = sb.toString();
		}
		else {
			sb.setLength(0);
			sb.append("css/basic-min.css?v=").append(UtilImpl.WEB_RESOURCE_FILE_VERSION);
			cssRelativeFileNameUrl = sb.toString();
		}
	}
	
	@SuppressWarnings("static-method")
	public User getUser() {
		ExternalContext ec = FacesContext.getCurrentInstance().getExternalContext();
		return (User) ec.getSessionMap().get(WebContext.USER_SESSION_ATTRIBUTE_NAME);
	}
	
	@SuppressWarnings("static-method")
	public void setUser(String customerName, String userName) {
		User user = null;
		AbstractRepository repository = AbstractRepository.get();
		ExternalContext ec = FacesContext.getCurrentInstance().getExternalContext();
		if (ec.getUserPrincipal() == null) { // not logged in
			user = repository.retrieveUser(new StringBuilder(64).append(customerName).append('/').append(userName).toString());
		}
		else {
			user = repository.retrieveUser(ec.getUserPrincipal().toString());
		}
		ec.getSessionMap().put(WebContext.USER_SESSION_ATTRIBUTE_NAME, user);

		AbstractPersistence persistence = AbstractPersistence.get();
		persistence.setUser(user);
	}
	
	/**
	 * Sets the UX/UI preference in the session.
	 * @param uxui	The UX/UI name.
	 */
	@SuppressWarnings("static-method")
	public void setUxUi(String uxui) {
		ExternalContext ec = FacesContext.getCurrentInstance().getExternalContext();
		if (uxui == null) {
			ec.getSessionMap().remove(AbstractWebContext.UXUI);
		}
		else {
			ec.getSessionMap().put(AbstractWebContext.UXUI, uxui);
		}
	}
}
