package org.skyve.impl.metadata.user;

import java.util.Map;
import java.util.TreeMap;

import org.skyve.metadata.DecoratedMetaData;

/**
 * Abstract base for permission grants declared in a role.
 *
 * <p>Each privilege has a {@code name} (the document or action name) and an
 * optional property map for renderer-specific extensions. Concrete subclasses
 * are {@link DocumentPrivilege} (CRUD flags) and {@link ActionPrivilege}
 * (a permitted action on a document).
 */
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
