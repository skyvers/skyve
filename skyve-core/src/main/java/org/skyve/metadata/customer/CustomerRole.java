package org.skyve.metadata.customer;

import java.util.List;

import org.skyve.metadata.NamedMetaData;
import org.skyve.metadata.user.Role;

public interface CustomerRole extends NamedMetaData {
	public String getDescription();
	public String getDocumentation();
	public List<Role> getModuleRoles();
}
