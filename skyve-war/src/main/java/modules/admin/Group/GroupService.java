package modules.admin.Group;

import java.util.List;

import org.skyve.domain.Bean;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;

import jakarta.enterprise.inject.Default;
import jakarta.inject.Inject;
import modules.admin.domain.Group;

/**
 * This class acts as a service layer to encapsulate domain logic.
 *
 * Add this line to classes that wish to use it: @Inject private transient GroupService groupService;
 */
@Default
public class GroupService {
	@Inject
	private transient Persistence persistence;

	/**
	 * Retrieves a GroupExtension by its BizId.
	 *
	 * @param bizId The BizId of the Group.
	 * @return The GroupExtension with the given ID, or null if not found.
	 */
	public GroupExtension get(String bizId) {
		final DocumentQuery query = persistence.newDocumentQuery(Group.MODULE_NAME,
				Group.DOCUMENT_NAME);
		query.getFilter()
				.addEquals(Bean.DOCUMENT_ID, bizId);
		return query.beanResult();
	}

	/**
	 * Retrieves all GroupExtension records.
	 *
	 * @return A list of all PartySessionExtension records in the system.
	 */
	public List<GroupExtension> getAll() {
		final DocumentQuery query = persistence.newDocumentQuery(Group.MODULE_NAME,
				Group.DOCUMENT_NAME);
		return query.beanResults();
	}

	/**
	 * Finds a group by its name.
	 *
	 * @param groupName The name of the group to find
	 * @return The GroupExtension with the given name, or null if not found
	 */
	public GroupExtension findByName(String groupName) {
		final DocumentQuery query = persistence.newDocumentQuery(Group.MODULE_NAME, Group.DOCUMENT_NAME);
		query.getFilter().addEquals(Group.namePropertyName, groupName);
		query.setMaxResults(1);
		return query.beanResult();
	}
}
