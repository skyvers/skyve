package modules.admin.domain;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

/**
 * Tests for the {@link DynamicRelation} admin domain constants.
 * DynamicRelation is abstract with no concrete extension class, so only
 * static constants can be reliably tested.
 */
public class DynamicRelationDomainTest {

	@Test
	@SuppressWarnings("static-method")
	void moduleNameConstant() {
		assertEquals("admin", DynamicRelation.MODULE_NAME);
	}

	@Test
	@SuppressWarnings("static-method")
	void documentNameConstant() {
		assertEquals("DynamicRelation", DynamicRelation.DOCUMENT_NAME);
	}

	@Test
	@SuppressWarnings("static-method")
	void relatedModuleNamePropertyNameConstant() {
		assertEquals("relatedModuleName", DynamicRelation.relatedModuleNamePropertyName);
	}

	@Test
	@SuppressWarnings("static-method")
	void relatedDocumentNamePropertyNameConstant() {
		assertEquals("relatedDocumentName", DynamicRelation.relatedDocumentNamePropertyName);
	}

	@Test
	@SuppressWarnings("static-method")
	void relatedIdPropertyNameConstant() {
		assertEquals("relatedId", DynamicRelation.relatedIdPropertyName);
	}

	@Test
	@SuppressWarnings("static-method")
	void attributeNamePropertyNameConstant() {
		assertEquals("attributeName", DynamicRelation.attributeNamePropertyName);
	}
}
