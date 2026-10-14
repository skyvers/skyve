package org.skyve.impl.metadata.user;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;

import org.skyve.impl.metadata.repository.module.ContentPermission;
import org.skyve.impl.metadata.repository.module.ContentRestriction;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.Role;
import org.skyve.metadata.user.UserAccess;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Runtime implementation of {@link org.skyve.metadata.user.Role} that carries
 * the role's document and action privilege set, content restrictions and
 * permissions, and named {@link org.skyve.metadata.user.UserAccess} grants.
 *
 * @see org.skyve.metadata.user.Role
 */
public class RoleImpl implements Role {
	private static final long serialVersionUID = -2841351233211789543L;

	private Module owningModule;
	private String name;
	private String description;
	private List<Privilege> privileges = new ArrayList<>();
	private List<ContentRestriction> contentRestrictions = new ArrayList<>();
	private List<ContentPermission> contentPermissions = new ArrayList<>();
	private Map<UserAccess, Set<String>> accesses = new TreeMap<>();
	private String documentation;
	private Map<String, String> properties = new LinkedHashMap<>();
	
	@Override
	public @Nonnull String getDescription() {
		return description;
	}

	public void setDescription(@Nonnull String description) {
		this.description = description;
	}

	@Override
	public @Nonnull String getName() {
		return name;
	}

	public void setName(@Nonnull String name) {
		this.name = name;
	}

	public @Nonnull List<Privilege> getPrivileges() {
		return privileges;
	}

	public @Nonnull List<ContentRestriction> getContentRestrictions() {
		return contentRestrictions;
	}

	public @Nonnull List<ContentPermission> getContentPermissions() {
		return contentPermissions;
	}

	public @Nonnull Map<UserAccess, Set<String>> getAccesses() {
		return accesses;
	}
	
	@Override
	public @Nonnull Module getOwningModule() {
		return owningModule;
	}

	public void setOwningModule(@Nonnull Module owningModule) {
		this.owningModule = owningModule;
	}

	@Override
	public @Nullable String getDocumentation() {
		return documentation;
	}

	public void setDocumentation(@Nullable String documentation) {
		this.documentation = documentation;
	}
	
	@Override
	public @Nonnull Map<String, String> getProperties() {
		return properties;
	}
}
