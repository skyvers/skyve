package modules.admin.domain;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

/**
 * Tests for the {@link DynamicEntity} admin domain constants.
 * DynamicEntity is abstract with no concrete extension class, so only
 * static constants can be reliably tested.
 */
public class DynamicEntityDomainTest {

	@Test
	@SuppressWarnings("static-method")
	void moduleNameConstant() {
		assertEquals("admin", DynamicEntity.MODULE_NAME);
	}

	@Test
	@SuppressWarnings("static-method")
	void documentNameConstant() {
		assertEquals("DynamicEntity", DynamicEntity.DOCUMENT_NAME);
	}

	@Test
	@SuppressWarnings("static-method")
	void moduleNamePropertyNameConstant() {
		assertEquals("moduleName", DynamicEntity.moduleNamePropertyName);
	}

	@Test
	@SuppressWarnings("static-method")
	void documentNamePropertyNameConstant() {
		assertEquals("documentName", DynamicEntity.documentNamePropertyName);
	}

	@Test
	@SuppressWarnings("static-method")
	void fieldsPropertyNameConstant() {
		assertEquals("fields", DynamicEntity.fieldsPropertyName);
	}

	@Test
	@SuppressWarnings("static-method")
	void relationsPropertyNameConstant() {
		assertEquals("relations", DynamicEntity.relationsPropertyName);
	}
}
