package util.sail;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.List;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.repository.customer.CustomerRoleMetaData;
import org.skyve.impl.metadata.user.RoleImpl;
import org.skyve.impl.metadata.user.UserImpl;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.Role;
import org.skyve.metadata.user.User;

/**
 * Unit tests of the view roles registry and the users it builds the views with.
 */
@SuppressWarnings("static-method")
class SailViewRolesTest {
	@Test
	void roleNamesMatchTheScriptsUserNameThenTheSubstitutedName() {
		SailViewRoles roles = new SailViewRoles().register("client", "todo.ToDoClient")
													.register("jane{run}", "admin.BasicUser", "todo.ToDoClient");
		assertAll(() -> assertEquals(List.of("todo.ToDoClient"), roles.roleNames("client", "client")),
					() -> assertEquals(List.of("admin.BasicUser", "todo.ToDoClient"), roles.roleNames("jane{run}", "janeabc123")),
					() -> assertEquals(List.of("todo.ToDoClient"), roles.roleNames("{var:who}", "client")),
					() -> assertNull(roles.roleNames("admin", "admin")));
	}

	@Test
	void registeringAgainReplacesTheRoles() {
		SailViewRoles roles = new SailViewRoles().register("client", "todo.ToDoClient").register("client", "admin.BasicUser");
		assertEquals(List.of("admin.BasicUser"), roles.roleNames("client", "client"));
	}

	@Test
	void registeringNoRoleOrABlankRoleFails() {
		SailViewRoles roles = new SailViewRoles();
		assertAll(() -> assertThrows(IllegalArgumentException.class, () -> roles.register("client")),
					() -> assertThrows(IllegalArgumentException.class, () -> roles.register("client", "todo.ToDoClient", " ")));
	}

	@Test
	void userWithRolesHasExactlyTheModuleRolesNamed() {
		Fixture f = new Fixture();
		UserImpl user = SailUsers.userWithRoles(f.testUser, List.of("todo.ToDoClient"));
		assertAll(() -> assertTrue(user.isInRole("todo", "ToDoClient")),
					() -> assertFalse(user.isInRole("todo", "ToDoManager")),
					() -> assertFalse(user.isInRole("admin", "BasicUser")),
					() -> assertEquals("bizhub", user.getCustomerName()),
					() -> assertEquals("tester", user.getName()),
					() -> assertEquals("u1", user.getId()),
					() -> assertEquals("c1", user.getContactId()));
	}

	@Test
	void userWithRolesExpandsACustomerRole() {
		Fixture f = new Fixture();
		UserImpl user = SailUsers.userWithRoles(f.testUser, List.of("Clients"));
		assertAll(() -> assertTrue(user.isInRole("todo", "ToDoClient")),
					() -> assertTrue(user.isInRole("admin", "BasicUser")),
					() -> assertFalse(user.isInRole("todo", "ToDoManager")));
	}

	@Test
	void userWithRolesRejectsUnknownRoles() {
		Fixture f = new Fixture();
		assertAll(() -> assertThrows(IllegalArgumentException.class, () -> SailUsers.userWithRoles(f.testUser, List.of("nope.Role"))),
					() -> assertThrows(IllegalArgumentException.class, () -> SailUsers.userWithRoles(f.testUser, List.of("todo.Nope"))),
					() -> assertThrows(IllegalArgumentException.class, () -> SailUsers.userWithRoles(f.testUser, List.of("Nope"))));
	}

	@Test
	void bootstrapLikeUserHasEveryModuleRole() {
		Fixture f = new Fixture();
		UserImpl user = SailUsers.bootstrapLikeUser(f.testUser);
		assertAll(() -> assertTrue(user.isInRole("todo", "ToDoClient")),
					() -> assertTrue(user.isInRole("todo", "ToDoManager")),
					() -> assertTrue(user.isInRole("admin", "BasicUser")));
	}

	/**
	 * A customer {@code bizhub} with modules {@code admin} (BasicUser) and {@code todo} (ToDoClient, ToDoManager), a
	 * customer role {@code Clients} of {@code admin.BasicUser} and {@code todo.ToDoClient}, and a test user of the customer.
	 */
	static final class Fixture {
		final Customer customer = mock(Customer.class);
		final User testUser = mock(User.class);

		Fixture() {
			Module admin = module("admin");
			Module todo = module("todo");
			RoleImpl basicUser = role(admin, "BasicUser");
			RoleImpl client = role(todo, "ToDoClient");
			RoleImpl manager = role(todo, "ToDoManager");
			when(admin.getRoles()).thenReturn(List.of(basicUser));
			when(todo.getRoles()).thenReturn(List.of(client, manager));
			when(admin.getRole("BasicUser")).thenReturn(basicUser);
			when(todo.getRole("ToDoClient")).thenReturn(client);
			when(todo.getRole("ToDoManager")).thenReturn(manager);
			when(customer.getModules()).thenReturn(List.of(admin, todo));
			when(customer.getName()).thenReturn("bizhub");
			CustomerRoleMetaData clients = mock(CustomerRoleMetaData.class);
			List<Role> clientRoles = List.of(basicUser, client);
			when(clients.getModuleRoles(customer)).thenReturn(clientRoles);
			when(customer.getRole("Clients")).thenReturn(clients);

			when(testUser.getCustomer()).thenReturn(customer);
			when(testUser.getCustomerName()).thenReturn("bizhub");
			when(testUser.getName()).thenReturn("tester");
			when(testUser.getId()).thenReturn("u1");
			when(testUser.getContactId()).thenReturn("c1");
		}

		private static Module module(String name) {
			Module result = mock(Module.class);
			when(result.getName()).thenReturn(name);
			return result;
		}

		private static RoleImpl role(Module module, String name) {
			RoleImpl result = new RoleImpl();
			result.setName(name);
			result.setOwningModule(module);
			return result;
		}
	}
}
