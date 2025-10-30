package modules.admin.Group;

import org.skyve.CORE;
import org.skyve.persistence.DocumentQuery;

import jakarta.enterprise.inject.Default;
import modules.admin.domain.Group;
import modules.admin.domain.GroupRole;

/**
 * This class acts as a service layer to encapsulate domain logic.
 *
 * Add this line to classes that wish to use it: @Inject private transient GroupService groupService;
 */
@Default
public class GroupService {

	/**
	 * Configure a permissions group with at least the roleNames specified
	 * 
	 * @param name
	 * @param roleNames
	 */
	@SuppressWarnings("static-method")
	public GroupExtension configureGroup(String name, String... roleNames) {
		// Configure required Staff permissions group
		DocumentQuery qGroup = CORE.getPersistence().newDocumentQuery(Group.MODULE_NAME, Group.DOCUMENT_NAME);
		qGroup.getFilter().addEquals(Group.namePropertyName, name);
		boolean saveRequired = false;
		GroupExtension g = qGroup.beanResult();
		if (g == null) {
			g = Group.newInstance();
			g.setName(name);
			saveRequired = true;
		}
		// check roles
		for (String s : roleNames) {
			boolean found = false;
			for (GroupRole gr : g.getRoles()) {
				if (s.equals(gr.getRoleName())) {
					found = true;
					break;
				}
			}
			if (!found) {
				GroupRole gr = GroupRole.newInstance();
				gr.setRoleName(s);
				g.addRolesElement(gr);
				saveRequired = true;
			}
		}

		// and Save the group
		if (saveRequired) {
			g = CORE.getPersistence().save(g);
		}

		return g;
	}
}
