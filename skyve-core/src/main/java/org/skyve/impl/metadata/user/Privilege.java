package org.skyve.impl.metadata.user;

import java.util.Map;
import java.util.TreeMap;

import org.skyve.metadata.DecoratedMetaData;

public abstract class Privilege implements DecoratedMetaData {
	private static final long serialVersionUID = 7901164127754285622L;

	private String name;
	private Map<String, String> properties = new TreeMap<>();

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}
	
	@Override
	public Map<String, String> getProperties() {
		return properties;
	}
}
