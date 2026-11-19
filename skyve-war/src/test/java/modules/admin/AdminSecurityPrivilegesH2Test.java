package modules.admin;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.List;
import java.util.Objects;
import java.util.TreeSet;

import org.junit.jupiter.api.Test;
import org.skyve.CORE;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.impl.metadata.user.RoleImpl;
import org.skyve.impl.metadata.user.UserImpl;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.Role;

import util.AbstractH2Test;

/**
 * Who holds the privileges that decide account security in the admin module, read from its roles: only a
 * security administrator creates, deletes, generates passwords for or signs out everywhere other people's
 * users; only DevOps can swap customer (the one place a session user changes customer).
 */
@SuppressWarnings("static-method")
class AdminSecurityPrivilegesH2Test extends AbstractH2Test {
	@Test
	void onlyASecurityAdministratorRunsTheUserActionsThatChangeAnotherUsersAccess() {
		for (String action : List.of("GeneratePassword", "ResendActivation", "SignOutEverywhere", "Check", "New")) {
			assertEquals(List.of("SecurityAdministrator"), rolesExecuting("User", action), action);
		}
	}

	@Test
	void onlyDevOpsCanSwapCustomer() {
		assertEquals(List.of("DevOps"), rolesExecuting("ControlPanel", "SwapCustomer"));
	}

	@Test
	void theOrdinaryUserRolesAreTheOnesThatCanSignOutOtherSessions() {
		assertEquals(List.of("AppUser", "BasicUser", "ViewUser"), rolesExecuting("UserAccount", "SignOutOtherSessions"));
	}

	@Test
	void onlyASecurityAdministratorCanCreateOrDeleteUsersAndRoles() {
		for (String document : List.of("User", "UserRole", "Group")) {
			for (String role : adminRoleNames()) {
				UserImpl user = userWith(role);
				Document d = document(document);
				boolean mayCreateOrDelete = user.canCreateDocument(d) || user.canDeleteDocument(d);
				assertEquals(Boolean.valueOf("SecurityAdministrator".equals(role)), Boolean.valueOf(mayCreateOrDelete),
						role + " on " + document);
			}
		}
	}

	private static List<String> rolesExecuting(String documentName, String actionName) {
		return new TreeSet<>(adminRoleNames()).stream()
				.filter(role -> userWith(role).canExecuteAction(document(documentName), actionName))
				.toList();
	}

	private static List<String> adminRoleNames() {
		return admin().getRoles().stream().map(Role::getName).toList();
	}

	private static Module admin() {
		return CORE.getCustomer().getModule("admin");
	}

	private static Document document(String name) {
		Customer customer = CORE.getCustomer();
		return admin().getDocument(customer, name);
	}

	private static UserImpl userWith(String roleName) {
		UserImpl result = new UserImpl();
		result.setCustomerName(CUSTOMER);
		result.setName("role-" + roleName);
		result.setId("role-" + roleName);
		result.addRole((RoleImpl) Objects.requireNonNull(admin().getRole(roleName)));
		Objects.requireNonNull(ProvidedRepositoryFactory.get()).resetMenus(result);
		return result;
	}
}
