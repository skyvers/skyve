package org.skyve.metadata.router;

import java.io.Serializable;

import com.google.common.base.MoreObjects;

/**
 * Represents a UX/UI variant configuration in Skyve, combining a named presentation
 * tier with its associated SmartClient or PrimeFaces theme settings.
 *
 * <p>Skyve supports multiple UX/UI variants within a single deployment (e.g.
 * {@code desktop}, {@code phone}, {@code tablet}). Each variant can render using
 * either the SmartClient ({@link #getScSkin()}) or PrimeFaces ({@link #getPfTemplateName()})
 * component library, and carries an exact PrimeFaces theme plus a colour palette used by
 * the selected template and its PrimeFaces widget overrides.
 *
 * <p>Instances are created via the static factory methods:
 * <ul>
 *   <li>{@link #newPrimeFaces} &mdash; PrimeFaces-backed variant</li>
 *   <li>{@link #newSmartClient} &mdash; SmartClient-backed variant</li>
 * </ul>
 *
 * <p>{@link #DESKTOP_NAME} is the conventional name for the default desktop variant.
 */
public final class UxUi implements Serializable {
	private static final long serialVersionUID = 6408014926938963507L;

	/** The conventional name for the standard desktop UX/UI variant. */
	public static final String DESKTOP_NAME = "desktop";

	private String name;
	private String scSkin;
	private String pfTemplateName;
	private String pfThemeName;
	private String pfThemeColour;
	
	private UxUi(String name) {
		this.name = name;
	}

	/**
	 * Returns the name.
	 * @return the result
	 */
	public String getName() {
		return name;
	}
	/**
	 * Sets the name.
	 * @param name the name
	 */
	public void setName(String name) {
		this.name = name;
	}

	/**
	 * Returns the scSkin.
	 * @return the result
	 */
	public String getScSkin() {
		return scSkin;
	}
	/**
	 * Sets the scSkin.
	 * @param scSkin the scSkin
	 */
	public void setScSkin(String scSkin) {
		this.scSkin = scSkin;
	}

	/**
	 * Returns the pfTemplateName.
	 * @return the result
	 */
	public String getPfTemplateName() {
		return pfTemplateName;
	}
	/**
	 * Sets the pfTemplateName.
	 * @param pfTemplateName the pfTemplateName
	 */
	public void setPfTemplateName(String pfTemplateName) {
		this.pfTemplateName = pfTemplateName;
	}

	/**
	 * Returns the configured PrimeFaces theme name.
	 *
	 * @return the PrimeFaces theme name, or {@code null} when none is configured
	 */
	public String getPfThemeName() {
		return pfThemeName;
	}
	/**
	 * Sets the PrimeFaces theme name.
	 *
	 * @param pfThemeName the PrimeFaces theme name
	 */
	public void setPfThemeName(String pfThemeName) {
		this.pfThemeName = pfThemeName;
	}

	/**
	 * Returns the colour palette selected for the template and PrimeFaces widget overrides.
	 *
	 * @return the PrimeFaces theme colour, or {@code null} when none is configured
	 */
	public String getPfThemeColour() {
		return pfThemeColour;
	}
	/**
	 * Sets the colour palette used by the template and PrimeFaces widget overrides.
	 *
	 * @param pfThemeColour the PrimeFaces theme colour
	 */
	public void setPfThemeColour(String pfThemeColour) {
		this.pfThemeColour = pfThemeColour;
	}

	/**
	 * Creates a PrimeFaces-backed UX/UI variant with a template and theme.
	 *
	 * @param name             the variant name (e.g. {@code "desktop"}); must not be {@code null}
	 * @param pfTemplateName   the PrimeFaces layout template name; must not be {@code null}
	 * @param pfThemeName      the exact PrimeFaces theme identifier; must not be {@code null}
	 * @return a new {@code UxUi} configured for PrimeFaces
	 */
	public static UxUi newPrimeFaces(String name, String pfTemplateName, String pfThemeName) {
		UxUi result = new UxUi(name);
		result.setPfTemplateName(pfTemplateName);
		result.setPfThemeName(pfThemeName);
		return result;
	}

	/**
	 * Creates a PrimeFaces-backed UX/UI variant with a template, theme, and colour palette.
	 *
	 * @param name             the variant name; must not be {@code null}
	 * @param pfTemplateName   the PrimeFaces layout template name; must not be {@code null}
	 * @param pfThemeName      the exact PrimeFaces theme identifier; must not be {@code null}
	 * @param pfThemeColour    the colour used by the template and PrimeFaces widget overrides;
	 *                         must not be {@code null}
	 * @return a new {@code UxUi} configured for PrimeFaces with a colour palette
	 */
	public static UxUi newPrimeFaces(String name, String pfTemplateName, String pfThemeName, String pfThemeColour) {
		UxUi result = newPrimeFaces(name, pfTemplateName, pfThemeName);
		result.setPfThemeColour(pfThemeColour);
		return result;
	}

	/**
	 * Creates a SmartClient-backed UX/UI variant.
	 *
	 * @param name         the variant name; must not be {@code null}
	 * @param scSkin       the SmartClient skin name; must not be {@code null}
	 * @param pfThemeName  a PrimeFaces theme identifier used for components rendered outside
	 *                     the SmartClient layer; must not be {@code null}
	 * @return a new {@code UxUi} configured for SmartClient
	 */
	public static UxUi newSmartClient(String name, String scSkin, String pfThemeName) {
		UxUi result = new UxUi(name);
		result.setScSkin(scSkin);
		result.setPfThemeName(pfThemeName);
		return result;
	}

	/**
	 * Creates a SmartClient-backed UX/UI variant with a PrimeFaces colour palette.
	 *
	 * @param name          the variant name; must not be {@code null}
	 * @param scSkin        the SmartClient skin name; must not be {@code null}
	 * @param pfThemeName   the exact PrimeFaces theme identifier; must not be {@code null}
	 * @param pfThemeColour the colour used by the template and PrimeFaces widget overrides;
	 *                      must not be {@code null}
	 * @return a new {@code UxUi} configured for SmartClient with a PrimeFaces colour palette
	 */
	public static UxUi newSmartClient(String name, String scSkin, String pfThemeName, String pfThemeColour) {
		UxUi result = newSmartClient(name, scSkin, pfThemeName);
		result.setPfThemeColour(pfThemeColour);
		return result;
	}

	/**
	 * Returns a string representation of this instance.
	 * @return the result
	 */
	@Override
	public String toString() {
		return MoreObjects.toStringHelper(this)
							.add("name", name)
							.add("scSkin", scSkin)
							.add("pfTemplateName", pfTemplateName)
							.add("pfThemeName", pfThemeName)
							.add("pfThemeColour", pfThemeColour)
							.toString();
	}
}
