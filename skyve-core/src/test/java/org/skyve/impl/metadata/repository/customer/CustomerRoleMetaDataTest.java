package org.skyve.impl.metadata.repository.customer;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.List;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.Role;

@ExtendWith(MockitoExtension.class)
@SuppressWarnings("static-method")
class CustomerRoleMetaDataTest {

	@Test
	void setDescriptionStoredViaProcessStringValue() {
		CustomerRoleMetaData role = new CustomerRoleMetaData();
		role.setDescription("  Admin Role  ");
		// UtilImpl.processStringValue trims whitespace
		assertEquals("Admin Role", role.getDescription());
	}

	@Test
	void setDescriptionNullStoresNull() {
		CustomerRoleMetaData role = new CustomerRoleMetaData();
		role.setDescription(null);
		assertNull(role.getDescription());
	}

	@Test
	void setDescriptionBlankBecomesNull() {
		CustomerRoleMetaData role = new CustomerRoleMetaData();
		role.setDescription("   ");
		assertNull(role.getDescription());
	}

	@Test
	void setDocumentationStoredViaProcessStringValue() {
		CustomerRoleMetaData role = new CustomerRoleMetaData();
		role.setDocumentation("  Some docs  ");
		assertEquals("Some docs", role.getDocumentation());
	}

	@Test
	void setDocumentationNullStoresNull() {
		CustomerRoleMetaData role = new CustomerRoleMetaData();
		role.setDocumentation(null);
		assertNull(role.getDocumentation());
	}

	@Test
	void setDocumentationBlankBecomesNull() {
		CustomerRoleMetaData role = new CustomerRoleMetaData();
		role.setDocumentation("   ");
		assertNull(role.getDocumentation());
	}

	@Test
	void getRolesIsNotNull() {
		CustomerRoleMetaData role = new CustomerRoleMetaData();
		assertNotNull(role.getRoles());
	}

	@Test
	void getPropertiesIsNotNull() {
		CustomerRoleMetaData role = new CustomerRoleMetaData();
		assertNotNull(role.getProperties());
	}

	@Test
	void getModuleRolesReturnsRoleForEachModuleRoleEntry() {
		// Set up the customer role with one module role entry
		CustomerModuleRoleMetaData modRole = new CustomerModuleRoleMetaData();
		modRole.setModuleName("admin");
		modRole.setName("BasicUser");

		CustomerRoleMetaData customerRole = new CustomerRoleMetaData();
		customerRole.getRoles().add(modRole);

		// Mock Customer and Module
		Role mockRole = mock(Role.class);
		Module mockModule = mock(Module.class);
		when(mockModule.getNullSafeRole("BasicUser")).thenReturn(mockRole);
		Customer mockCustomer = mock(Customer.class);
		when(mockCustomer.getModule("admin")).thenReturn(mockModule);

		List<Role> result = customerRole.getModuleRoles(mockCustomer);

		assertNotNull(result);
		assertEquals(1, result.size());
		assertEquals(mockRole, result.get(0));
	}

	@Test
	void getModuleRolesReturnsEmptyListWhenNoRoles() {
		CustomerRoleMetaData customerRole = new CustomerRoleMetaData();
		Customer mockCustomer = mock(Customer.class);
		List<Role> result = customerRole.getModuleRoles(mockCustomer);
		assertNotNull(result);
		assertTrue(result.isEmpty());
	}
}
