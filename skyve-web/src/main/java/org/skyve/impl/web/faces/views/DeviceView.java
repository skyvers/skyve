package org.skyve.impl.web.faces.views;

import java.util.Map;
import java.util.Map.Entry;

import org.skyve.impl.web.AbstractWebContext;
import org.skyve.impl.web.WebUtil;
import org.skyve.util.Util;

import jakarta.annotation.Nonnull;
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

	private String previewUrlPrefix;
	private String endPreviewUrl;

	/**
	 * Returns the one-time preview command URL prefix.
	 *
	 * @return the URL prefix; initialised during post construction
	 */
	public @Nonnull String getPreviewUrlPrefix() {
		return previewUrlPrefix;
	}

	/**
	 * Returns the command URL that ends preview without retaining the preview target parameters.
	 *
	 * @return the end-preview URL; initialised during post construction
	 */
	public @Nonnull String getEndPreviewUrl() {
		return endPreviewUrl;
	}

	private String contextUrl;
	
	/**
	 * Returns the Skyve web context URL, resolving it on first access.
	 *
	 * @return the context URL; never {@code null} when Skyve configuration is available
	 */
	public @Nonnull String getContextUrl() {
		if (contextUrl == null) {
			contextUrl = Util.getSkyveContextUrl();
		}
		return contextUrl;
	}

	/**
	 * Initialises inherited localisation state and the device-preview URL for this request.
	 *
	 * <p>Side effects: reads the current Faces request parameters and caches a derived URL string on
	 * this request-scoped view instance. The command is consumed into session state by
	 * {@code device.jsp} before it redirects to the application context root without the command parameter.
	 */
	@PostConstruct
	private void postConstruct() {
		initialise();

		Map<String, String[]> parameters = FacesContext.getCurrentInstance().getExternalContext().getRequestParameterValuesMap();
		StringBuilder sb = new StringBuilder(128);
		sb.append(getContextUrl()).append("/device.jsp");
		for (Entry<String, String[]> entry : parameters.entrySet()) {
			if (! AbstractWebContext.EMULATED_USER_AGENT_TYPE_PARAMETER.equals(entry.getKey())) {
				WebUtil.appendQueryParameter(sb, entry.getKey(), entry.getValue());
			}
		}
		sb.append(sb.indexOf("?") < 0 ? '?' : '&')
			.append(AbstractWebContext.EMULATED_USER_AGENT_TYPE_PARAMETER)
			.append('=');
		previewUrlPrefix = sb.toString();
		endPreviewUrl = getContextUrl() + "/device.jsp?" +
									AbstractWebContext.EMULATED_USER_AGENT_TYPE_PARAMETER + '=';
	}
}
