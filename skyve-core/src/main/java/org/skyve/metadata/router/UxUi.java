package org.skyve.metadata.router;

import java.io.Serializable;

public final class UxUi implements Serializable {
	private static final long serialVersionUID = 6408014926938963507L;

	private String name;
	private String scSkin;
	private String pfTemplateName;
	private String pfThemeName;
	private String pfThemeColour;
	
	private UxUi(String name) {
		this.name = name;
	}

	public String getName() {
		return name;
	}
	public void setName(String name) {
		this.name = name;
	}

	public String getScSkin() {
		return scSkin;
	}
	public void setScSkin(String scSkin) {
		this.scSkin = scSkin;
	}

	public String getPfTemplateName() {
		return pfTemplateName;
	}
	public void setPfTemplateName(String pfTemplateName) {
		this.pfTemplateName = pfTemplateName;
	}

	public String getPfThemeName() {
		return pfThemeName;
	}
	public void setPfThemeName(String pfThemeName) {
		this.pfThemeName = pfThemeName;
	}

	public String getPfThemeColour() {
		return pfThemeColour;
	}
	public void setPfThemeColour(String pfThemeColour) {
		this.pfThemeColour = pfThemeColour;
	}
	
	public String getPfTheme() {
		if (pfThemeColour == null) {
			return pfThemeName;
		}
		return String.format("%s-%s", pfThemeName, pfThemeColour);
	}
	
	public static UxUi newPrimeFaces(String name, String pfTemplateName, String pfThemeName) {
		UxUi result = new UxUi(name);
		result.setPfTemplateName(pfTemplateName);
		result.setPfThemeName(pfThemeName);
		return result;
	}

	public static UxUi newPrimeFaces(String name, String pfTemplateName, String pfThemeName, String pfThemeColour) {
		UxUi result = newPrimeFaces(name, pfTemplateName, pfThemeName);
		result.setPfThemeColour(pfThemeColour);
		return result;
	}

	public static UxUi newSmartClient(String name, String scSkin, String pfThemeName) {
		UxUi result = new UxUi(name);
		result.setScSkin(scSkin);
		result.setPfThemeName(pfThemeName);
		return result;
	}

	public static UxUi newSmartClient(String name, String scSkin, String pfThemeName, String pfThemeColour) {
		UxUi result = newPrimeFaces(name, scSkin, pfThemeName);
		result.setPfThemeColour(pfThemeColour);
		return result;
	}
}
