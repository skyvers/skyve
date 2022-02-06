package org.skyve.impl.metadata.user;

import org.skyve.metadata.SerializableMetaData;

public abstract class Privilege implements SerializableMetaData {
	private static final long serialVersionUID = 7901164127754285622L;

	private String name;

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}
}
