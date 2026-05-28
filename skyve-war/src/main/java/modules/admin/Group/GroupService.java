package modules.admin.Group;

import java.util.List;

import org.skyve.domain.Bean;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;

import jakarta.enterprise.inject.Default;
import jakarta.inject.Inject;
import modules.admin.domain.Group;

/**
 * Provides query-based service operations for admin group records.
 */
@Default
public class GroupService {
	@Inject
	@SuppressWarnings("java:S6813") // allow member injection
	private transient Persistence persistence;

	/**
	 * Retrieves one group by document id.
	 *
	 * @param bizId
	 *        the group document identifier
	 * @return the matching group, or {@code null} when no record exists
	 */
	public GroupExtension get(String bizId) {
		final DocumentQuery query = persistence.newDocumentQuery(Group.MODULE_NAME,
				Group.DOCUMENT_NAME);
		query.getFilter()
				.addEquals(Bean.DOCUMENT_ID, bizId);
		return query.beanResult();
	}

	/**
	 * Retrieves all groups visible to the current persistence user.
	 *
	 * @return all group records for the customer context
	 */
	public List<GroupExtension> getAll() {
		final DocumentQuery query = persistence.newDocumentQuery(Group.MODULE_NAME,
				Group.DOCUMENT_NAME);
		return query.beanResults();
	}

	/**
	 * Finds the first group matching the supplied name.
	 *
	 * @param groupName
	 *        the group name to search for
	 * @return the first matching group, or {@code null} when no match exists
	 */
	public GroupExtension findByName(String groupName) {
		final DocumentQuery query = persistence.newDocumentQuery(Group.MODULE_NAME, Group.DOCUMENT_NAME);
		query.getFilter().addEquals(Group.namePropertyName, groupName);
		query.setMaxResults(1);
		return query.beanResult();
	}
}
