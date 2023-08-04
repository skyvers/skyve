package org.skyve.admin.web;

import javax.faces.bean.ManagedBean;
import javax.faces.bean.ViewScoped;

import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.faces.beans.PublicFacesView;

import modules.admin.domain.SelfRegistration;


/**
 * Used to assert a public user from admin.Configuration data for use with public forms.
 * 
 * @author Brandon Klar
 * @param <SelfRegistration>
 */
@ViewScoped
@ManagedBean(name = "skyveRegistration")
public class RegistrationPublicFacesView extends PublicFacesView<SelfRegistration> {
	
	private static final long serialVersionUID = -3606463122925431489L;

	@Override
	public String getBizCustomerParameter() {
		return super.getBizCustomerParameter();
	}
	
	@Override
	public void setBizCustomerParameter(java.lang.String bizCustomerParameter) {
		super.setBizCustomerParameter(bizCustomerParameter);
	}

	@Override
	public void preRender() {
		super.preRender();
	}
	
	// retrieves the recaptcha key if it exists
	@SuppressWarnings("static-method")
	public String getRecaptchaKey() {
		if (UtilImpl.GOOGLE_RECAPTCHA_SITE_KEY != null) {
			return UtilImpl.GOOGLE_RECAPTCHA_SITE_KEY;
		}
		return null;
	}

}
