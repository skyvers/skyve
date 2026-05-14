package org.skyve.impl.metadata.repository.customer;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;

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
	void getRolesIsNotNull() {
		CustomerRoleMetaData role = new CustomerRoleMetaData();
		assertNotNull(role.getRoles());
	}

	@Test
	void getPropertiesIsNotNull() {
		CustomerRoleMetaData role = new CustomerRoleMetaData();
		assertNotNull(role.getProperties());
	}
}
