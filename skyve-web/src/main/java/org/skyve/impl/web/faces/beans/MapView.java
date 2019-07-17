package org.skyve.impl.web.faces.beans;

import javax.faces.bean.ManagedBean;
import javax.faces.bean.ViewScoped;

import org.skyve.domain.Bean;

@ViewScoped
@ManagedBean(name = "skyveMap")
public class MapView<T extends Bean> extends FacesView<T> {
	private static final long serialVersionUID = 903537633094849991L;

	private String bindingParameter = null;

	public String getBindingParameter() {
		return bindingParameter;
	}

	public void setBindingParameter(String bindingParameter) {
		this.bindingParameter = bindingParameter;
	}
}
