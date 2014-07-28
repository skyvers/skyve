package org.skyve.wildcat.metadata.user;

import org.skyve.metadata.MetaData;

public abstract class Privilege implements MetaData {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = 7901164127754285622L;

	private String name;

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}
}
