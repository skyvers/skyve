package org.skyve.impl.web.faces.beans;

import javax.faces.bean.ManagedBean;
import javax.faces.bean.ViewScoped;
import javax.faces.context.ExternalContext;
import javax.faces.context.FacesContext;

import org.skyve.domain.Bean;
import org.skyve.impl.util.SQLMetaDataUtil;
import org.skyve.impl.util.UtilImpl;

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
					if (customerName != null) {
						String userName = SQLMetaDataUtil.retrievePublicUserName(customerName);
						if (userName != null) {
							setUser(customerName, userName);
						}
					}
				}
			}
		}
		super.preRender();
	}
}
