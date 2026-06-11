package modules.test.domain;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

import util.AbstractH2Test;

@SuppressWarnings("static-method")
class InverseOneToManyPersistentDomainTest extends AbstractH2Test {

	@Test
	void bizModuleIsTest() {
		InverseOneToManyPersistent bean = InverseOneToManyPersistent.newInstance();
		assertEquals("test", bean.getBizModule());
	}

	@Test
	void bizDocumentIsInverseOneToManyPersistent() {
		InverseOneToManyPersistent bean = InverseOneToManyPersistent.newInstance();
		assertEquals("InverseOneToManyPersistent", bean.getBizDocument());
	}

	@Test
	void getBizKeyNotNull() {
		InverseOneToManyPersistent bean = InverseOneToManyPersistent.newInstance();
		assertNotNull(bean.getBizKey());
	}

	@Test
	void aggAssociationNullByDefault() {
		InverseOneToManyPersistent bean = InverseOneToManyPersistent.newInstance();
		assertNull(bean.getAggAssociation());
	}

	@Test
	void aggAssociationSetAndGet() {
		InverseOneToManyPersistent bean = InverseOneToManyPersistent.newInstance();
		InverseOneToManyPersistent assoc = InverseOneToManyPersistent.newInstance();
		bean.setAggAssociation(assoc);
		assertEquals(assoc, bean.getAggAssociation());
	}

	@Test
	void nullAggAssociationClearsField() {
		InverseOneToManyPersistent bean = InverseOneToManyPersistent.newInstance();
		InverseOneToManyPersistent assoc = InverseOneToManyPersistent.newInstance();
		bean.setAggAssociation(assoc);
		bean.nullAggAssociation();
		assertNull(bean.getAggAssociation());
	}

	@Test
	void invAggAssociationEmptyByDefault() {
		InverseOneToManyPersistent bean = InverseOneToManyPersistent.newInstance();
		assertNotNull(bean.getInvAggAssociation());
		assertTrue(bean.getInvAggAssociation().isEmpty());
	}

	@Test
	void addAndGetInvAggAssociationElement() {
		InverseOneToManyPersistent bean = InverseOneToManyPersistent.newInstance();
		InverseOneToManyPersistent child = InverseOneToManyPersistent.newInstance();
		bean.addInvAggAssociationElement(child);
		assertEquals(1, bean.getInvAggAssociation().size());
	}

	@Test
	void addInvAggAssociationElementAtIndex() {
		InverseOneToManyPersistent bean = InverseOneToManyPersistent.newInstance();
		InverseOneToManyPersistent child1 = InverseOneToManyPersistent.newInstance();
		InverseOneToManyPersistent child2 = InverseOneToManyPersistent.newInstance();
		bean.addInvAggAssociationElement(child1);
		bean.addInvAggAssociationElement(0, child2);
		assertEquals(child2, bean.getInvAggAssociation().get(0));
	}

	@Test
	void removeInvAggAssociationElement() {
		InverseOneToManyPersistent bean = InverseOneToManyPersistent.newInstance();
		InverseOneToManyPersistent child = InverseOneToManyPersistent.newInstance();
		bean.addInvAggAssociationElement(child);
		assertTrue(bean.removeInvAggAssociationElement(child));
		assertTrue(bean.getInvAggAssociation().isEmpty());
	}

	@Test
	void removeInvAggAssociationElementNotPresent() {
		InverseOneToManyPersistent bean = InverseOneToManyPersistent.newInstance();
		InverseOneToManyPersistent child = InverseOneToManyPersistent.newInstance();
		assertFalse(bean.removeInvAggAssociationElement(child));
	}

	@Test
	void removeInvAggAssociationElementByIndex() {
		InverseOneToManyPersistent bean = InverseOneToManyPersistent.newInstance();
		InverseOneToManyPersistent child = InverseOneToManyPersistent.newInstance();
		bean.addInvAggAssociationElement(child);
		InverseOneToManyPersistent removed = bean.removeInvAggAssociationElement(0);
		assertEquals(child, removed);
		assertTrue(bean.getInvAggAssociation().isEmpty());
	}

	@Test
	void getInvAggAssociationElementById() {
		InverseOneToManyPersistent bean = InverseOneToManyPersistent.newInstance();
		InverseOneToManyPersistent child = InverseOneToManyPersistent.newInstance();
		bean.addInvAggAssociationElement(child);
		assertNull(bean.getInvAggAssociationElementById("nonexistent"));
	}
}
