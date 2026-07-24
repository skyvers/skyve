package org.skyve.impl.web.faces.views;

import java.net.MalformedURLException;

import org.skyve.impl.web.UserAgent;
import org.skyve.metadata.router.UxUi;
import org.skyve.web.UserAgentType;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.enterprise.context.RequestScoped;
import jakarta.faces.context.FacesContext;
import jakarta.inject.Named;
import jakarta.servlet.ServletContext;
import jakarta.servlet.http.HttpServletRequest;

/**
 * Exposes the selected UX/UI's configured and derived presentation values to Faces rendering.
 *
 * <p>The request-selected {@link UxUi} remains the source of configured values. The first access
 * to a derived value applies family defaults to absent template and colour values,
 * derives family-specific layout values, verifies the selected template's iframe resource pair,
 * and caches the result for the remainder of this bean's request lifetime. Subsequent derived
 * accesses return that snapshot without repeating servlet-resource lookups.
 *
 * <p>Invariant: the request's {@code UxUi} selection must be complete and stable while the view is
 * rendered. Configured component-theme access reads that selected object directly; cached derived
 * values do not reflect later mutation of its template or colour fields.
 *
 * <p>Side effects: first derived-value access performs up to two read-only
 * {@link ServletContext#getResource(String)} lookups and populates this instance's request-local
 * cache. The class performs no file, session, or application-scoped mutation.
 *
 * <p>Thread-confined: instances belong to one request and must not be shared across threads.
 */
@RequestScoped
@Named("_skyveTheme")
public class FacesThemeView {
	/** Diamond template family. */
	private static final String DIAMOND_TEMPLATE = "diamond";
	/** Editorial template family. */
	private static final String EDITORIAL_TEMPLATE = "editorial";
	/** External template family and default full-page fallback. */
	private static final String EXTERNAL_TEMPLATE = "external";
	/** Ultima template family. */
	private static final String ULTIMA_TEMPLATE = "ultima";
	/** Colour token that explicitly selects a dark Skyve-owned palette. */
	private static final String DARK_COLOUR = "dark";
	/** Default Diamond presentation scheme. */
	private static final String LIGHT_SCHEME = "light";
	/** Generic iframe family used when a selected family lacks a complete resource pair. */
	private static final String DEFAULT_IFRAME_TEMPLATE = "iframe";
	/** Default palette for Skyve-owned Editorial and External families. */
	private static final String DEFAULT_BLUE = "blue";
	/** Default palette for other template families. */
	private static final String DEFAULT_INDIGO = "indigo";
	/** Servlet resource root containing the deployable template families. */
	private static final String TEMPLATE_ROOT = "/WEB-INF/pages/templates/";

	/** Explicit request supplied by tests, or {@code null} when Faces supplies the active request. */
	private final HttpServletRequest request;
	/** Indicates that all derived fields below contain one complete request snapshot. */
	private boolean resolved;
	/** Effective full-page template family. */
	private String templateName;
	/** Effective palette base after family-specific parsing. */
	private String baseColour;
	/** Optional family-specific presentation scheme. */
	private String scheme;
	/** Whether the selected Skyve-owned family explicitly forces dark presentation. */
	private boolean forcedDark;
	/** Family supplying the complete iframe head/body resource pair. */
	private String iframeResourceTemplateName;
	/** Complete template-family-specific CSS class fragment. */
	private String layoutStyleClass;

	/**
	 * Creates a JSF-managed facade that obtains the current HTTP request lazily.
	 *
	 * <p>Precondition: calls to request-dependent getters require an active {@link FacesContext}
	 * whose external request is an {@link HttpServletRequest}.
	 */
	public FacesThemeView() {
		request = null;
	}

	/**
	 * Creates a request-bound facade without requiring a current {@link FacesContext}.
	 *
	 * <p>This package-private constructor is a test seam; production construction is managed by CDI.
	 *
	 * @param request active request containing a complete UX/UI selection; must not be {@code null}
	 */
	FacesThemeView(@Nonnull HttpServletRequest request) {
		this.request = request;
	}

	/**
	 * Returns the PrimeFaces template whose complete resource set should style the page.
	 *
	 * <p>Used as {@code _skyveTheme.templateName} by the top-level Faces pages to select
	 * {@code /WEB-INF/pages/templates/<family>/view.xhtml}.
	 * A {@code null} configured name resolves to {@code external}.
	 *
	 * @return the effective template family; never {@code null}
	 */
	public @Nonnull String getTemplateName() {
		resolve();
		return templateName;
	}

	/**
	 * Returns the exact PrimeFaces component-theme resource name.
	 *
	 * <p>Used as {@code _skyveTheme.componentThemeName} by the {@code primefaces.THEME}
	 * context parameter in {@code WEB-INF/web.xml}.
	 * The value is read directly from the selected {@link UxUi} without normalization or caching.
	 *
	 * @return the configured name, or {@code null} when none was configured
	 */
	public @Nullable String getComponentThemeName() {
		return selectedUxUi().getPfThemeName();
	}

	/**
	 * Returns the effective base colour used by layout resources.
	 *
	 * <p>Used as {@code _skyveTheme.baseColour} by the Editorial and External theme
	 * stylesheets, the Ecuador layout stylesheet, and Ultima's browser theme-colour metadata.
	 * A {@code null} colour uses the selected family's default; Diamond's scheme suffix is removed.
	 *
	 * @return the effective base colour; never {@code null}
	 */
	public @Nonnull String getBaseColour() {
		resolve();
		return baseColour;
	}

	/**
	 * Returns the optional opaque presentation-scheme token supplied to the template.
	 *
	 * <p>Used as {@code _skyveTheme.scheme} by Diamond's body resources to select its
	 * {@code layout-<scheme>.css} stylesheet.
	 * Diamond defaults to {@code light} when its colour contains no scheme suffix.
	 *
	 * @return the presentation-scheme token, or {@code null} when the family has no scheme
	 */
	public @Nullable String getScheme() {
		resolve();
		return scheme;
	}

	/**
	 * Indicates whether the server knows that a dark presentation is forced.
	 *
	 * <p>Used as {@code _skyveTheme.forcedDark} by the Editorial and External head resources
	 * to suppress the preference-driven dark stylesheet when dark mode is already explicit.
	 *
	 * @return {@code true} when the resolved theme explicitly forces dark presentation
	 */
	public boolean isForcedDark() {
		resolve();
		return forcedDark;
	}

	/**
	 * Indicates whether the selected UX/UI is backed by SmartClient.
	 *
	 * <p>Faces tools opened from SmartClient use this as a light-presentation boundary because the
	 * SmartClient desktop itself does not follow the browser or operating-system dark preference.
	 *
	 * @return {@code true} when the selected UX/UI configures a SmartClient skin
	 */
	public boolean isSmartClient() {
		return selectedUxUi().getScSkin() != null;
	}

	/**
	 * Returns the family supplying the complete iframe resource pair.
	 *
	 * <p>Used as {@code _skyveTheme.iframeResourceTemplateName} by the shared iframe template
	 * to include that family's {@code head-resources.xhtml} and {@code body-resources.xhtml}.
	 * The selected family is returned only when both resources exist; otherwise this returns
	 * {@code iframe} so rendering never mixes partial resource families.
	 *
	 * @return the iframe resource family; never {@code null}
	 */
	public @Nonnull String getIframeResourceTemplateName() {
		resolve();
		return iframeResourceTemplateName;
	}

	/**
	 * Returns the complete family-specific layout class fragment.
	 *
	 * <p>Used as {@code _skyveTheme.layoutStyleClass} on the Ultima, Diamond, and shared
	 * iframe layout wrappers.
	 *
	 * @return the complete class fragment, or an empty string when the family needs none
	 */
	public @Nonnull String getLayoutStyleClass() {
		resolve();
		return layoutStyleClass;
	}

	/**
	 * Indicates whether the effective request device is a phone.
	 *
	 * <p>Used as {@code _skyveTheme.phone} by the external home, list, edit, and map pages
	 * to hide the mode switch on phones.
	 *
	 * @return {@code true} when the selected user-agent type is {@code phone}
	 */
	public boolean isPhone() {
		return UserAgentType.phone.equals(UserAgent.getSelection(activeRequest()).getUserAgentType());
	}

	/**
	 * Returns the application-owned UX/UI selected for the active request.
	 *
	 * @return the selected configuration object; never {@code null}
	 * @throws IllegalStateException if request UX/UI selection has not completed
	 */
	private @Nonnull UxUi selectedUxUi() {
		return UserAgent.getSelection(activeRequest()).getUxUi();
	}

	/**
	 * Populates all derived rendering fields exactly once for this request-scoped instance.
	 *
	 * <p>The derived {@link #baseColour}, {@link #scheme}, {@link #forcedDark}, and
	 * {@link #layoutStyleClass} fields are EL shortcuts, not independent theme configuration.
	 * Their values could be parsed or assembled in the family-specific XHTML templates, but exposing
	 * simple bean properties avoids repeating string operations and conditional expressions in
	 * Facelets and gives the shared iframe shell one consistent set of layout values. The selected
	 * {@link UxUi} remains authoritative for the configured template, component theme, and colour.
	 *
	 * <p>Maintenance: prefer keeping presentation rules in a family-specific template when only that
	 * template needs them. Add a derived value here only when it materially simplifies EL in multiple
	 * locations or is required by the shared iframe template.
	 *
	 * <p>Side effects: on first invocation, reads the selected {@link UxUi}, performs up to two
	 * servlet-resource lookups, and writes all derived cache fields before setting {@link #resolved}.
	 * Later invocations have no side effects.
	 *
	 * <p>Complexity: O(1) time and space; template and colour strings are scanned at most once.
	 *
	 * @throws IllegalStateException if request UX/UI selection has not completed
	 */
	private void resolve() {
		if (resolved) {
			return;
		}

		HttpServletRequest activeRequest = activeRequest();
		UxUi uxui = UserAgent.getSelection(activeRequest).getUxUi();
		String configuredTemplate = uxui.getPfTemplateName();
		String resolvedTemplateName = (configuredTemplate == null) ? EXTERNAL_TEMPLATE : configuredTemplate;
		boolean skyveFamily = EDITORIAL_TEMPLATE.equals(resolvedTemplateName) ||
				EXTERNAL_TEMPLATE.equals(resolvedTemplateName);
		String resolvedBaseColour = uxui.getPfThemeColour();
		if (resolvedBaseColour == null) {
			resolvedBaseColour = skyveFamily ? DEFAULT_BLUE : DEFAULT_INDIGO;
		}
		String resolvedScheme = null;
		String resolvedLayoutStyleClass = "";
		if (DIAMOND_TEMPLATE.equals(resolvedTemplateName)) {
			int separator = resolvedBaseColour.lastIndexOf('-');
			if (separator > 0) {
				resolvedScheme = resolvedBaseColour.substring(separator + 1);
				resolvedBaseColour = resolvedBaseColour.substring(0, separator);
			}
			else {
				resolvedScheme = LIGHT_SCHEME;
			}
			resolvedLayoutStyleClass = "layout-sidebar-" + resolvedBaseColour;
		}
		else if (ULTIMA_TEMPLATE.equals(resolvedTemplateName)) {
			resolvedLayoutStyleClass = "layout-menu-" + resolvedBaseColour + " layout-topbar-" + resolvedBaseColour;
		}

		templateName = resolvedTemplateName;
		baseColour = resolvedBaseColour;
		scheme = resolvedScheme;
		forcedDark = skyveFamily && DARK_COLOUR.equals(resolvedBaseColour);
		iframeResourceTemplateName = hasCompleteResourcePair(activeRequest.getServletContext(), resolvedTemplateName)
										? resolvedTemplateName
										: DEFAULT_IFRAME_TEMPLATE;
		layoutStyleClass = resolvedLayoutStyleClass;
		resolved = true;
	}

	/**
	 * Indicates whether a template family supplies both required iframe resource fragments.
	 *
	 * <p>A missing servlet context, missing fragment, or malformed resource path is treated as an
	 * unavailable pair. The body lookup is skipped when the head fragment is unavailable.
	 *
	 * @param servletContext deployment resource lookup, or {@code null} outside a servlet context
	 * @param templateName effective non-null, non-blank template family
	 * @return {@code true} only when both head and body resource fragments exist
	 */
	private static boolean hasCompleteResourcePair(@Nullable ServletContext servletContext,
													@Nonnull String templateName) {
		if (servletContext == null) {
			return false;
		}
		String familyRoot = TEMPLATE_ROOT + templateName + '/';
		try {
			return (servletContext.getResource(familyRoot + "head-resources.xhtml") != null) &&
					(servletContext.getResource(familyRoot + "body-resources.xhtml") != null);
		}
		catch (@SuppressWarnings("unused") MalformedURLException e) {
			return false;
		}
	}

	/**
	 * Returns the request explicitly supplied at construction or the current Faces request.
	 *
	 * @return the active HTTP request; never {@code null} in a valid Faces invocation
	 * @throws NullPointerException if no explicit request and no current {@link FacesContext} exist
	 * @throws ClassCastException if the current Faces external request is not an
	 * {@link HttpServletRequest}
	 */
	private @Nonnull HttpServletRequest activeRequest() {
		if (request != null) {
			return request;
		}
		FacesContext context = FacesContext.getCurrentInstance();
		return (HttpServletRequest) context.getExternalContext().getRequest();
	}
}
