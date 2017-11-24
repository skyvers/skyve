package org.skyve.metadata.router;

import java.io.Serializable;

public final class UxUi implements Serializable {
	private static final long serialVersionUID = 6408014926938963507L;

	private String name;
	private String pfTheme;
	
	public UxUi() {
		// nothing to see here
	}

	public UxUi(String name, String pfTheme) {
		super();
		this.name = name;
		this.pfTheme = pfTheme;
	}

	public String getName() {
		return name;
	}
	public void setName(String name) {
		this.name = name;
	}

	public String getPfTheme() {
		return pfTheme;
	}
	public void setPfTheme(String pfTheme) {
		this.pfTheme = pfTheme;
	}
}
