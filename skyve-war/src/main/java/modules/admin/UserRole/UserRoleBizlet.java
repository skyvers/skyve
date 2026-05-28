package modules.admin.UserRole;

import java.util.List;

import org.skyve.CORE;
import org.skyve.metadata.model.document.Bizlet;

import jakarta.inject.Inject;
import modules.admin.User.UserService;
import modules.admin.domain.UserRole;

/**
 * Resolves selectable role values for {@link UserRole} maintenance.
 */
public class UserRoleBizlet extends Bizlet<UserRole> {
	@Inject
	@SuppressWarnings("java:S6813") // allow member injection
	private UserService userService;

	/**
	 * Returns role domain values for the {@code roleName} field.
	 *
	 * @param fieldName The field requiring variant values.
	 * @return Module-qualified role values when the role field is requested.
	 * @throws Exception If role resolution fails.
	 */
	@Override
	public List<DomainValue> getVariantDomainValues(String fieldName)
			throws Exception {
		if (UserRole.roleNamePropertyName.equals(fieldName)) {
			return userService.getCustomerRoleValues(CORE.getUser());
		}

		return super.getVariantDomainValues(fieldName);
	}
}
