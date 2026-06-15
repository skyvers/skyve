package modules.admin.domain;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class DynamicRelationDomainTest {

	private DynamicRelation relation;
	private DynamicEntity entity;

	@BeforeEach
	void setUp() {
		relation = new DynamicRelation() { /* test stub */ };
		entity = new DynamicEntity() { /* test stub */ };
	}

	@Test
	void moduleNameConstant() {
		assertEquals("admin", DynamicRelation.MODULE_NAME);
	}

	@Test
	void documentNameConstant() {
		assertEquals("DynamicRelation", DynamicRelation.DOCUMENT_NAME);
	}

	@Test
	void relatedModuleNamePropertyNameConstant() {
		assertEquals("relatedModuleName", DynamicRelation.relatedModuleNamePropertyName);
	}

	@Test
	void relatedDocumentNamePropertyNameConstant() {
		assertEquals("relatedDocumentName", DynamicRelation.relatedDocumentNamePropertyName);
	}

	@Test
	void relatedIdPropertyNameConstant() {
		assertEquals("relatedId", DynamicRelation.relatedIdPropertyName);
	}

	@Test
	void attributeNamePropertyNameConstant() {
		assertEquals("attributeName", DynamicRelation.attributeNamePropertyName);
	}

	@Test
	void getBizModuleReturnsAdmin() {
		assertEquals("admin", relation.getBizModule());
	}

	@Test
	void getBizDocumentReturnsDynamicRelation() {
		assertEquals("DynamicRelation", relation.getBizDocument());
	}

	@Test
	void relatedModuleNameSetAndGet() {
		relation.setRelatedModuleName("admin");
		assertEquals("admin", relation.getRelatedModuleName());
	}

	@Test
	void relatedDocumentNameSetAndGet() {
		relation.setRelatedDocumentName("User");
		assertEquals("User", relation.getRelatedDocumentName());
	}

	@Test
	void relatedIdSetAndGet() {
		relation.setRelatedId("abc123");
		assertEquals("abc123", relation.getRelatedId());
	}

	@Test
	void attributeNameSetAndGet() {
		relation.setAttributeName("userId");
		assertEquals("userId", relation.getAttributeName());
	}

	@Test
	void ordinalSetAndGet() {
		relation.setOrdinal(Integer.valueOf(5));
		assertEquals(Integer.valueOf(5), relation.getOrdinal());
	}

	@Test
	void bizOrdinalSetAndGet() {
		relation.setBizOrdinal(Integer.valueOf(3));
		assertEquals(Integer.valueOf(3), relation.getBizOrdinal());
	}

	@Test
	void parentInitiallyNull() {
		assertNull(relation.getParent());
	}

	@Test
	void parentSetAndGet() {
		relation.setParent(entity);
		assertEquals(entity, relation.getParent());
	}

	@Test
	void parentCanBeSetToNull() {
		relation.setParent(entity);
		relation.setParent(null);
		assertNull(relation.getParent());
	}
}
