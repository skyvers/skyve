package modules.test.domain;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

import util.AbstractH2Test;

@SuppressWarnings("static-method")
public class AllDynamicAttributesPersistentDomainTest extends AbstractH2Test {

	@Test
	void bizModuleIsTest() throws Exception {
		AllDynamicAttributesPersistent bean = AllDynamicAttributesPersistent.newInstance();
		assertEquals("test", bean.getBizModule());
	}

	@Test
	void bizDocumentIsAllDynamicAttributesPersistent() throws Exception {
		AllDynamicAttributesPersistent bean = AllDynamicAttributesPersistent.newInstance();
		assertEquals("AllDynamicAttributesPersistent", bean.getBizDocument());
	}

	@Test
	void getBizKeyNotNull() throws Exception {
		AllDynamicAttributesPersistent bean = AllDynamicAttributesPersistent.newInstance();
		assertNotNull(bean.getBizKey());
	}

	@Test
	void aggregatedAssociationNullByDefault() throws Exception {
		AllDynamicAttributesPersistent bean = AllDynamicAttributesPersistent.newInstance();
		assertNull(bean.getAggregatedAssociation());
	}

	@Test
	void aggregatedAssociationSetAndGet() throws Exception {
		AllDynamicAttributesPersistent bean = AllDynamicAttributesPersistent.newInstance();
		AllDynamicAttributesPersistent assoc = AllDynamicAttributesPersistent.newInstance();
		bean.setAggregatedAssociation(assoc);
		assertEquals(assoc, bean.getAggregatedAssociation());
	}

	@Test
	void nullAggregatedAssociation() throws Exception {
		AllDynamicAttributesPersistent bean = AllDynamicAttributesPersistent.newInstance();
		AllDynamicAttributesPersistent assoc = AllDynamicAttributesPersistent.newInstance();
		bean.setAggregatedAssociation(assoc);
		bean.nullAggregatedAssociation();
		assertNull(bean.getAggregatedAssociation());
	}

	@Test
	void composedAssociationNullByDefault() throws Exception {
		AllDynamicAttributesPersistent bean = AllDynamicAttributesPersistent.newInstance();
		assertNull(bean.getComposedAssociation());
	}

	@Test
	void composedAssociationSetAndGet() throws Exception {
		AllDynamicAttributesPersistent bean = AllDynamicAttributesPersistent.newInstance();
		AllDynamicAttributesPersistent assoc = AllDynamicAttributesPersistent.newInstance();
		bean.setComposedAssociation(assoc);
		assertEquals(assoc, bean.getComposedAssociation());
	}

	@Test
	void composedCollectionEmptyByDefault() throws Exception {
		AllDynamicAttributesPersistent bean = AllDynamicAttributesPersistent.newInstance();
		assertNotNull(bean.getComposedCollection());
		assertTrue(bean.getComposedCollection().isEmpty());
	}

	@Test
	void addComposedCollectionElement() throws Exception {
		AllDynamicAttributesPersistent bean = AllDynamicAttributesPersistent.newInstance();
		AllDynamicAttributesPersistent child = AllDynamicAttributesPersistent.newInstance();
		assertTrue(bean.addComposedCollectionElement(child));
		assertEquals(1, bean.getComposedCollection().size());
	}

	@Test
	void addComposedCollectionElementAtIndex() throws Exception {
		AllDynamicAttributesPersistent bean = AllDynamicAttributesPersistent.newInstance();
		AllDynamicAttributesPersistent child1 = AllDynamicAttributesPersistent.newInstance();
		AllDynamicAttributesPersistent child2 = AllDynamicAttributesPersistent.newInstance();
		bean.addComposedCollectionElement(child1);
		bean.addComposedCollectionElement(0, child2);
		assertEquals(child2, bean.getComposedCollection().get(0));
	}

	@Test
	void removeComposedCollectionElement() throws Exception {
		AllDynamicAttributesPersistent bean = AllDynamicAttributesPersistent.newInstance();
		AllDynamicAttributesPersistent child = AllDynamicAttributesPersistent.newInstance();
		bean.addComposedCollectionElement(child);
		assertTrue(bean.removeComposedCollectionElement(child));
		assertTrue(bean.getComposedCollection().isEmpty());
	}

	@Test
	void removeComposedCollectionElementNotPresent() throws Exception {
		AllDynamicAttributesPersistent bean = AllDynamicAttributesPersistent.newInstance();
		AllDynamicAttributesPersistent child = AllDynamicAttributesPersistent.newInstance();
		assertFalse(bean.removeComposedCollectionElement(child));
	}

	@Test
	void removeComposedCollectionElementByIndex() throws Exception {
		AllDynamicAttributesPersistent bean = AllDynamicAttributesPersistent.newInstance();
		AllDynamicAttributesPersistent child = AllDynamicAttributesPersistent.newInstance();
		bean.addComposedCollectionElement(child);
		AllDynamicAttributesPersistent removed = bean.removeComposedCollectionElement(0);
		assertEquals(child, removed);
		assertTrue(bean.getComposedCollection().isEmpty());
	}

	@Test
	void inverseAggregatedAssociationEmptyByDefault() throws Exception {
		AllDynamicAttributesPersistent bean = AllDynamicAttributesPersistent.newInstance();
		assertNotNull(bean.getInverseAggregatedAssociation());
		assertTrue(bean.getInverseAggregatedAssociation().isEmpty());
	}

	@Test
	void addInverseAggregatedAssociationElement() throws Exception {
		AllDynamicAttributesPersistent bean = AllDynamicAttributesPersistent.newInstance();
		AllDynamicAttributesPersistent child = AllDynamicAttributesPersistent.newInstance();
		assertTrue(bean.addInverseAggregatedAssociationElement(child));
		assertEquals(1, bean.getInverseAggregatedAssociation().size());
	}

	@Test
	void removeInverseAggregatedAssociationElement() throws Exception {
		AllDynamicAttributesPersistent bean = AllDynamicAttributesPersistent.newInstance();
		AllDynamicAttributesPersistent child = AllDynamicAttributesPersistent.newInstance();
		bean.addInverseAggregatedAssociationElement(child);
		assertTrue(bean.removeInverseAggregatedAssociationElement(child));
		assertTrue(bean.getInverseAggregatedAssociation().isEmpty());
	}

	@Test
	void getInverseAggregatedAssociationElementByIdReturnsNull() throws Exception {
		AllDynamicAttributesPersistent bean = AllDynamicAttributesPersistent.newInstance();
		assertNull(bean.getInverseAggregatedAssociationElementById("nonexistent"));
	}

        @Test
        void embeddedAssociationSetAndGet() throws Exception {
                AllDynamicAttributesPersistent bean = AllDynamicAttributesPersistent.newInstance();
                assertNull(bean.getEmbeddedAssociation());
                AllAttributesEmbedded embedded = new AllAttributesEmbedded();
                bean.setEmbeddedAssociation(embedded);
                assertEquals(embedded, bean.getEmbeddedAssociation());
        }

        @Test
        void composedCollectionGetById() throws Exception {
                AllDynamicAttributesPersistent bean = AllDynamicAttributesPersistent.newInstance();
                assertNull(bean.getComposedCollectionElementById("nonexistent"));
                AllDynamicAttributesPersistent element = new AllDynamicAttributesPersistent();
                bean.addComposedCollectionElement(element);
                assertNotNull(bean.getComposedCollectionElementById(element.getBizId()));
        }

        @Test
        void inverseAggregatedAssociationAddAtIndexAndRemoveByIndex() throws Exception {
                AllDynamicAttributesPersistent bean = AllDynamicAttributesPersistent.newInstance();
                AllDynamicAttributesPersistent child = new AllDynamicAttributesPersistent();
                bean.addInverseAggregatedAssociationElement(0, child);
                assertEquals(1, bean.getInverseAggregatedAssociation().size());
                bean.removeInverseAggregatedAssociationElement(0);
                assertTrue(bean.getInverseAggregatedAssociation().isEmpty());
        }

        @Test
        void composedCollectionSetElementById() throws Exception {
                AllDynamicAttributesPersistent bean = AllDynamicAttributesPersistent.newInstance();
                AllDynamicAttributesPersistent element = new AllDynamicAttributesPersistent();
                bean.addComposedCollectionElement(element);
                AllDynamicAttributesPersistent replacement = new AllDynamicAttributesPersistent();
                replacement.setBizId(element.getBizId());
                bean.setComposedCollectionElementById(element.getBizId(), replacement);
                assertTrue(bean.getComposedCollection().contains(replacement));
                assertEquals(1, bean.getComposedCollection().size());
        }

        @Test
        void inverseAggregatedAssociationSetElementById() throws Exception {
                AllDynamicAttributesPersistent bean = AllDynamicAttributesPersistent.newInstance();
                AllDynamicAttributesPersistent element = new AllDynamicAttributesPersistent();
                bean.addInverseAggregatedAssociationElement(element);
                AllDynamicAttributesPersistent replacement = new AllDynamicAttributesPersistent();
                replacement.setBizId(element.getBizId());
                bean.setInverseAggregatedAssociationElementById(element.getBizId(), replacement);
                assertTrue(bean.getInverseAggregatedAssociation().contains(replacement));
                assertEquals(1, bean.getInverseAggregatedAssociation().size());
        }

        @Test
        void inverseAggregatedAssociationGetElementByIdFound() throws Exception {
                AllDynamicAttributesPersistent bean = AllDynamicAttributesPersistent.newInstance();
                AllDynamicAttributesPersistent element = new AllDynamicAttributesPersistent();
                bean.addInverseAggregatedAssociationElement(element);
                assertEquals(element, bean.getInverseAggregatedAssociationElementById(element.getBizId()));
        }
}

