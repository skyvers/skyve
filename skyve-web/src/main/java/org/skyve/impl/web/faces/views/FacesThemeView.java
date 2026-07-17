package org.skyve.impl.web.faces.views;

import org.skyve.impl.web.UserAgent;
import org.skyve.metadata.router.UxUi;

import jakarta.annotation.PostConstruct;
import jakarta.enterprise.context.RequestScoped;
import jakarta.faces.context.FacesContext;
import jakarta.inject.Named;
import jakarta.servlet.http.HttpServletRequest;

/**
 * Resolves the presentation theme shared by full PrimeFaces pages and headless iframe pages.
 *
 * <p>Threading: request-confined; instances must not be shared across requests.
 */
@RequestScoped
@Named("_skyveTheme")
public class FacesThemeView {
	private static final String DEFAULT_TEMPLATE = "external";

	private UxUi uxui;

	/** Resolves and retains the current request's UX/UI selection. */
	@PostConstruct
	public void postConstruct() {
		FacesContext context = FacesContext.getCurrentInstance();
		HttpServletRequest request = (HttpServletRequest) context.getExternalContext().getRequest();
		uxui = UserAgent.getUxUi(request);
	}

	/**
	 * Returns the PrimeFaces template whose complete resource set should style the page.
	 *
	 * <p>SmartClient profiles do not declare a PrimeFaces layout template, so their
	 * iframe-hosted PrimeFaces pages use the external template while retaining the
	 * profile's configured PrimeFaces component theme.
	 *
	 * @return the configured template name, or {@code external} when none is configured
	 */
	public String getTemplateName() {
		String result = uxui.getPfTemplateName();
		return (result == null) ? DEFAULT_TEMPLATE : result;
	}

	/**
	 * Returns the configured template colour with a family-appropriate fallback.
	 *
	 * @return the configured colour, or the default for the selected template family
	 */
	public String getThemeColour() {
		String result = uxui.getPfThemeColour();
		if (result == null) {
			String templateName = getTemplateName();
			if ("editorial".equals(templateName) || "external".equals(templateName)) {
				result = "blue";
			}
			else if ("diamond".equals(templateName)) {
				result = "indigo-light";
			}
			else {
				result = "indigo";
			}
		}
		return result;
	}

	/**
	 * Indicates whether a forced dark Editorial palette is selected.
	 *
	 * @return {@code true} when the configured colour is {@code dark}
	 */
	public boolean isDark() {
		return "dark".equals(getThemeColour());
	}
}
