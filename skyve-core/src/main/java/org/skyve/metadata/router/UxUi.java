package org.skyve.metadata.router;

import java.io.Serializable;

public final class UxUi implements Serializable {
	private static final long serialVersionUID = 6408014926938963507L;

	private String name;
	private String scSkin;
	private String pfThemeName;
	private String pfThemeColour;
	
	public UxUi() {
		// nothing to see here
	}

	public UxUi(String name, String pfThemeName) {
		this(name, pfThemeName, null);
	}
	
	public UxUi(String name, String pfThemeName, String pfThemeColour) {
		this(name, null, pfThemeName, pfThemeColour);
	}
	
	public UxUi(String name, String scSkin, String pfThemeName, String pfThemeColour) {
		this.name = name;
		this.scSkin = scSkin;
		this.pfThemeName = pfThemeName;
		this.pfThemeColour = pfThemeColour;
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
}
