package org.skyve.impl.web.faces.views;

import java.io.IOException;

import org.skyve.CORE;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.impl.util.UtilImpl;
import org.skyve.util.Util;

import jakarta.faces.context.ExternalContext;
import jakarta.faces.context.FacesContext;
import jakarta.faces.view.ViewScoped;
import jakarta.inject.Named;
import jakarta.servlet.http.HttpServletResponse;

/**
 * Used to assert a public user from admin.Configuration data for use with public forms.
 * 
 * @author mike
 * @param <T>
 */
@ViewScoped
@Named("skyvePublic")
public class PublicFacesView extends FacesView {
	private static final long serialVersionUID = 6102123422562563165L;

	// this is mainly used for public pages that need to assert a customer
	private String bizCustomerParameter;

	/**
	 * Returns the customer parameter used to resolve public-form user context.
	 *
	 * @return customer name parameter value, or {@code null}
	 */
	public String getBizCustomerParameter() {
		return bizCustomerParameter;
	}
	
	/**
	 * Sets the customer parameter used to resolve public-form user context.
	 *
	 * @param bizCustomerParameter the customer name parameter value
	 */
	public void setBizCustomerParameter(String bizCustomerParameter) {
		this.bizCustomerParameter = bizCustomerParameter;
	}
	
	/**
	 * Ensures a public user is asserted for non-postback requests before delegating to standard view pre-render logic.
	 */
	@Override
	@SuppressWarnings({"javasecurity:S5146", "java:S3776"}) // false positive: see below; Complexity OK
	public void preRender() {
		FacesContext fc = FacesContext.getCurrentInstance();
		if (! fc.isPostback()) {
			ExternalContext ec = fc.getExternalContext();
			if ((ec.getUserPrincipal() == null) && (getUser() == null)) {
				String customerName = (UtilImpl.CUSTOMER == null) ? bizCustomerParameter : UtilImpl.CUSTOMER;
				if (customerName == null) {
					throw new IllegalStateException("Malformed URL - this URL must have a 'c' parameter");
				}
				
				// This will throw if the customerName value ain't a customer name
				try {
					CORE.getRepository().getCustomer(customerName);
				}
				catch (@SuppressWarnings("unused") Exception e) {
					throw new IllegalStateException("Malformed URL - this URL must have a 'c' parameter with a valid customer");
				}
				
				String userName = ProvidedRepositoryFactory.get().retrievePublicUserName(customerName);
				if (userName == null) {
					HttpServletResponse response = (HttpServletResponse) ec.getResponse();
					try {
						// javasecurity:S5146 - false positive: this value is not user controlled.
						response.sendRedirect(response.encodeRedirectURL(Util.getBaseUrl() + "pages/noPublicUser.jsp"));
					}
					catch (IOException e) {
						throw new IllegalStateException("Could not redirect to /pages/noPublicUser.jsp", e);
					}
					return;
				}
				setUser(customerName, userName);
			}
		}
		super.preRender();
	}
}
