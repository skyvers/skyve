package org.skyve.impl.metadata.user;

import java.util.LinkedHashMap;
import java.util.Map;

import org.skyve.metadata.DecoratedMetaData;

import jakarta.annotation.Nonnull;

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
	private Map<String, String> properties = new LinkedHashMap<>();

	public @Nonnull String getName() {
		return name;
	}

	public void setName(@Nonnull String name) {
		this.name = name;
	}
	
	@Override
	public @Nonnull Map<String, String> getProperties() {
		return properties;
	}
}
