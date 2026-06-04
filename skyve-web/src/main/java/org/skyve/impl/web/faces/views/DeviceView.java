package org.skyve.impl.web.faces.views;

import java.util.Map;
import java.util.Map.Entry;

import org.skyve.util.Util;

import jakarta.annotation.PostConstruct;
import jakarta.enterprise.context.RequestScoped;
import jakarta.faces.context.FacesContext;
import jakarta.inject.Named;

/**
 * Provides URL state and localisation metadata for {@code device.xhtml}.
 *
 * <p>Thread-confined: request-scoped instances are created and initialised for a
 * single JSF request.
 */
@RequestScoped
@Named("device")
public class DeviceView extends LocalisableView {
	private static final long serialVersionUID = 4687894848392321390L;

	private String startingDeviceJspUrl;
	
	/**
	 * Returns the device JSP URL including the initial user-agent choice.
	 *
	 * @return the URL to load in the device frame; initialised during post construction
	 */
	public String getStartingDeviceJspUrl() {
		return startingDeviceJspUrl;
	}

	private String clickDeviceJspUrl;
	
	/**
	 * Returns the device JSP URL prefix used when switching the user-agent choice.
	 *
	 * @return the URL prefix ending with {@code ua=}; initialised during post construction
	 */
	public String getClickDeviceJspUrl() {
		return clickDeviceJspUrl;
	}

	private String contextUrl;
	
	/**
	 * Returns the Skyve web context URL, resolving it on first access.
	 *
	 * @return the context URL; never {@code null} when Skyve configuration is available
	 */
	public String getContextUrl() {
		if (contextUrl == null) {
			contextUrl = Util.getSkyveContextUrl();
		}
		return contextUrl;
	}

	/**
	 * Initialises inherited localisation state and device JSP URLs for this request.
	 *
	 * <p>Side effects: reads the current Faces request parameters and caches derived
	 * URL strings on this request-scoped view instance.
	 */
	@PostConstruct
	private void postConstruct() {
		initialise();

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
