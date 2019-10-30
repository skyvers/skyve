package org.skyve.impl.web.faces.beans;

import java.io.IOException;

import javax.faces.bean.ManagedBean;
import javax.faces.bean.ViewScoped;
import javax.faces.context.ExternalContext;
import javax.faces.context.FacesContext;
import javax.servlet.http.HttpServletResponse;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.impl.util.SQLMetaDataUtil;
import org.skyve.impl.util.UtilImpl;
import org.skyve.util.Util;

/**
 * Used to assert a public user from admin.Configuration data for use with public forms.
 * 
 * @author mike
 * @param <T>
 */
@ViewScoped
@ManagedBean(name = "skyvePublic")
public class PublicFacesView <T extends Bean> extends FacesView<T> {
	private static final long serialVersionUID = 6102123422562563165L;

	// this is mainly used for public pages that need to assert a customer
	private String bizCustomerParameter;
	public String getBizCustomerParameter() {
		return bizCustomerParameter;
	}
	public void setBizCustomerParameter(String bizCustomerParameter) {
		this.bizCustomerParameter = bizCustomerParameter;
	}
	
	@Override
	public void preRender() {
		FacesContext fc = FacesContext.getCurrentInstance();
		if (! fc.isPostback()) {
			ExternalContext ec = fc.getExternalContext();
			if (ec.getUserPrincipal() == null) {
				if (getUser() == null) {
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
					
					String userName = SQLMetaDataUtil.retrievePublicUserName(customerName);
					if (userName == null) {
						HttpServletResponse response = (HttpServletResponse) ec.getResponse();
						try {
							response.sendRedirect(response.encodeRedirectURL(Util.getHomeUrl() + "pages/noPublicUser.jsp"));
						}
						catch (IOException e) {
							throw new IllegalStateException("Could not redirect to /pages/noPublicUser.jsp", e);
						}
						return;
					}
					setUser(customerName, userName);
				}
			}
		}
		super.preRender();
	}
}
