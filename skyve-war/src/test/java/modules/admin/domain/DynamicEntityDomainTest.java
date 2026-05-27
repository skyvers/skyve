package modules.admin.domain;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Assertions;

@SuppressWarnings("static-method")
class DynamicEntityDomainTest {

	private DynamicEntity entity;
	private DynamicRelation relation;

	@BeforeEach
	void setUp() {
		entity = new DynamicEntity() { /* test stub */ };
		relation = new DynamicRelation() { /* test stub */ };
	}

	@Test
	void moduleNameConstant() {
		assertEquals("admin", DynamicEntity.MODULE_NAME);
	}

	@Test
	void documentNameConstant() {
		assertEquals("DynamicEntity", DynamicEntity.DOCUMENT_NAME);
	}

	@Test
	void moduleNamePropertyNameConstant() {
		assertEquals("moduleName", DynamicEntity.moduleNamePropertyName);
	}

	@Test
	void documentNamePropertyNameConstant() {
		assertEquals("documentName", DynamicEntity.documentNamePropertyName);
	}

	@Test
	void fieldsPropertyNameConstant() {
		assertEquals("fields", DynamicEntity.fieldsPropertyName);
	}

	@Test
	void relationsPropertyNameConstant() {
		assertEquals("relations", DynamicEntity.relationsPropertyName);
	}

	@Test
	void getBizModuleReturnsAdmin() {
		assertEquals("admin", entity.getBizModule());
	}

	@Test
	void getBizDocumentReturnsDynamicEntity() {
		assertEquals("DynamicEntity", entity.getBizDocument());
	}

	@Test
	void moduleNameSetAndGet() {
		entity.setModuleName("testModule");
		assertEquals("testModule", entity.getModuleName());
	}

	@Test
	void documentNameSetAndGet() {
		entity.setDocumentName("testDocument");
		assertEquals("testDocument", entity.getDocumentName());
	}

	@Test
	void fieldsSetAndGet() {
		entity.setFields("someFields");
		assertEquals("someFields", entity.getFields());
	}

	@Test
	void relationsListInitiallyEmpty() {
		assertNotNull(entity.getRelations());
		assertTrue(entity.getRelations().isEmpty());
	}

	@Test
	void addRelationsElementSetsParent() {
		boolean added = entity.addRelationsElement(relation);
		assertTrue(added);
		assertEquals(entity, relation.getParent());
		assertEquals(1, entity.getRelations().size());
	}

	@Test
	void addRelationsElementAtIndexSetsParent() {
		entity.addRelationsElement(0, relation);
		assertEquals(entity, relation.getParent());
		assertEquals(1, entity.getRelations().size());
	}

	@Test
	void removeRelationsElementClearsParent() {
		entity.addRelationsElement(relation);
		boolean removed = entity.removeRelationsElement(relation);
		assertTrue(removed);
		assertNull(relation.getParent());
		assertTrue(entity.getRelations().isEmpty());
	}

	@Test
	void getRelationsElementByIdNotFound() {
		assertNull(entity.getRelationsElementById("nonexistent-id"));
	}

	@Test
	void setRelationsElementByIdNoOp() {
		Assertions.assertDoesNotThrow(() -> entity.setRelationsElementById("some-id", relation));
	}

	@Test
	void removeRelationsElementNotPresent() {
		assertFalse(entity.removeRelationsElement(relation));
	}

        @Test
        void getBizKeyNotNull() {
                entity.setModuleName("m");
                entity.setDocumentName("d");
                assertNotNull(entity.getBizKey());
        }

        @Test
        void getRelationsElementByIdFound() {
                entity.addRelationsElement(relation);
                assertNotNull(entity.getRelationsElementById(relation.getBizId()));
        }

        @Test
        void setRelationsElementByIdReplaces() {
                entity.addRelationsElement(relation);
				DynamicRelation replacement = new DynamicRelation() { /* test stub */ };
                replacement.setBizId(relation.getBizId());
                entity.setRelationsElementById(relation.getBizId(), replacement);
                assertTrue(entity.getRelations().contains(replacement));
                assertEquals(1, entity.getRelations().size());
        }

        @Test
        void removeRelationsElementByIndex() {
                entity.addRelationsElement(relation);
                DynamicRelation removed = entity.removeRelationsElement(0);
                assertNotNull(removed);
                assertTrue(entity.getRelations().isEmpty());
        }
}
