package org.skyve.impl.web.faces.beans;

import java.io.Serializable;
import java.util.Map;
import java.util.Map.Entry;

import javax.annotation.PostConstruct;
import javax.faces.bean.ManagedBean;
import javax.faces.bean.RequestScoped;
import javax.faces.context.FacesContext;

import org.skyve.util.Util;

/**
 * Backs device.xhtml
 */
@RequestScoped
@ManagedBean(name = "device")
public class Device implements Serializable {
	private static final long serialVersionUID = 4687894848392321390L;

	private String startingDeviceJspUrl;
	
	public String getStartingDeviceJspUrl() {
		return startingDeviceJspUrl;
	}

	private String clickDeviceJspUrl;
	
	public String getClickDeviceJspUrl() {
		return clickDeviceJspUrl;
	}

	private String contextUrl;
	
	public String getContextUrl() {
		if (contextUrl == null) {
			contextUrl = Util.getSkyveContextUrl();
		}
		return contextUrl;
	}

	@PostConstruct
	private void postConstruct() {
		Map<String, String> parameters = FacesContext.getCurrentInstance().getExternalContext().getRequestParameterMap();
		StringBuilder sb = new StringBuilder(128);
		sb.append(getContextUrl()).append("/device.jsp?");
		
		String ua = null;
		for (Entry<String, String> entry : parameters.entrySet()) {
			String name = entry.getKey();
			if ("ua".equals(name)) {
				ua = entry.getValue();
			}
			else {
				sb.append(name).append('=').append(entry.getValue()).append('&');
			}
		}
		
		sb.append("ua=");
		clickDeviceJspUrl = sb.toString();
		
		sb.append((ua == null) ? "phone" : ua);
		startingDeviceJspUrl = sb.toString();
	}
	
	
}
