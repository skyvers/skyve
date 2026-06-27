package util;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.CORE;
import org.skyve.impl.bind.BindUtil;
import org.skyve.persistence.Persistence;

import modules.test.domain.AllAttributesPersistent;

/**
 * H2-backed tests for BindUtil collection manipulation methods that require
 * a real persistence context (CORE.getCustomer()).
 *
 * Covers: addElementToCollection, removeElementFromCollection,
 * ensureElementIsInCollection, getElementInCollection (Bean variant),
 * and orderByMetaData.
 */
class BindUtilCollectionH2Test extends AbstractH2Test {

        private static final String COLLECTION_BINDING = AllAttributesPersistent.aggregatedCollectionPropertyName;

        private AllAttributesPersistent parent;
        private AllAttributesPersistent child1;
        private AllAttributesPersistent child2;

        @BeforeEach
        void createBeans() {
                Persistence p = CORE.getPersistence();

                parent = AllAttributesPersistent.newInstance();
                parent.setText("parent");
                parent = p.save(parent);

                child1 = AllAttributesPersistent.newInstance();
                child1.setText("child1");
                child1 = p.save(child1);

                child2 = AllAttributesPersistent.newInstance();
                child2.setText("child2");
                child2 = p.save(child2);
        }

        // ---- addElementToCollection(Bean, String, Bean) ----

        @Test
        void addElementToCollectionAddsChildToParent() {
                boolean added = BindUtil.addElementToCollection(parent, COLLECTION_BINDING, child1);
                assertTrue(added, "addElementToCollection should return true when element is added");
                assertEquals(1, parent.getAggregatedCollection().size());
                assertEquals(child1.getBizId(), parent.getAggregatedCollection().get(0).getBizId());
        }

        @Test
        void addElementToCollectionAddsMultipleChildren() {
                BindUtil.addElementToCollection(parent, COLLECTION_BINDING, child1);
                BindUtil.addElementToCollection(parent, COLLECTION_BINDING, child2);
                assertEquals(2, parent.getAggregatedCollection().size());
        }

        // ---- addElementToCollection(Bean, String, int, Bean) ----

        @Test
        void addElementToCollectionAtIndexInsertsAtCorrectPosition() {
                // Add child1 first, then insert child2 at index 0
                BindUtil.addElementToCollection(parent, COLLECTION_BINDING, child1);
                BindUtil.addElementToCollection(parent, COLLECTION_BINDING, 0, child2);
                assertEquals(2, parent.getAggregatedCollection().size());
                assertEquals(child2.getBizId(), parent.getAggregatedCollection().get(0).getBizId(),
                                "child2 should be at index 0 after inserting at 0");
                assertEquals(child1.getBizId(), parent.getAggregatedCollection().get(1).getBizId(),
                                "child1 should be shifted to index 1");
        }

        // ---- removeElementFromCollection(Bean, String, Bean) ----

        @Test
        void removeElementFromCollectionRemovesChildByReference() {
                BindUtil.addElementToCollection(parent, COLLECTION_BINDING, child1);
                BindUtil.addElementToCollection(parent, COLLECTION_BINDING, child2);

                boolean removed = BindUtil.removeElementFromCollection(parent, COLLECTION_BINDING, child1);
                assertTrue(removed, "removeElementFromCollection should return true when element was present");
                assertEquals(1, parent.getAggregatedCollection().size());
                assertEquals(child2.getBizId(), parent.getAggregatedCollection().get(0).getBizId());
        }

        // ---- removeElementFromCollection(Bean, String, int) ----

        @Test
        void removeElementFromCollectionAtIndexRemovesCorrectElement() {
                BindUtil.addElementToCollection(parent, COLLECTION_BINDING, child1);
                BindUtil.addElementToCollection(parent, COLLECTION_BINDING, child2);

                AllAttributesPersistent removed = BindUtil.removeElementFromCollection(parent, COLLECTION_BINDING, 0);
                assertNotNull(removed);
                assertEquals(child1.getBizId(), removed.getBizId(), "element at index 0 should be child1");
                assertEquals(1, parent.getAggregatedCollection().size(), "collection should have 1 element after remove");
        }

        // ---- ensureElementIsInCollection ----

        @Test
        void ensureElementIsInCollectionAddsWhenAbsent() {
                AllAttributesPersistent result = (AllAttributesPersistent) BindUtil.ensureElementIsInCollection(
                                parent, COLLECTION_BINDING, child1);
                assertNotNull(result);
                assertEquals(child1.getBizId(), result.getBizId());
                assertEquals(1, parent.getAggregatedCollection().size());
        }

        @Test
        void ensureElementIsInCollectionReturnsExistingWhenPresent() {
                BindUtil.addElementToCollection(parent, COLLECTION_BINDING, child1);

                AllAttributesPersistent result = (AllAttributesPersistent) BindUtil.ensureElementIsInCollection(
                                parent, COLLECTION_BINDING, child1);
                assertNotNull(result);
                assertEquals(child1.getBizId(), result.getBizId());
                // Should still have only 1 element (not duplicated)
                assertEquals(1, parent.getAggregatedCollection().size());
        }

        // ---- getElementInCollection(Bean, String, String) ----

        @Test
        void getElementInCollectionBeanVariantReturnsMatchingElement() {
                BindUtil.addElementToCollection(parent, COLLECTION_BINDING, child1);
                BindUtil.addElementToCollection(parent, COLLECTION_BINDING, child2);

                AllAttributesPersistent found = (AllAttributesPersistent) BindUtil.getElementInCollection(
                                parent, COLLECTION_BINDING, child1.getBizId());
                assertNotNull(found);
                assertEquals(child1.getBizId(), found.getBizId());
        }

        @Test
        void getElementInCollectionBeanVariantReturnsNullWhenNotFound() {
                BindUtil.addElementToCollection(parent, COLLECTION_BINDING, child1);

                AllAttributesPersistent notFound = (AllAttributesPersistent) BindUtil.getElementInCollection(
                                parent, COLLECTION_BINDING, "nonexistent-id");
                assertNull(notFound);
        }

        // ---- setElementInCollection(List, T) ----

        @Test
        void setElementInCollectionReplacesExistingElement() {
                BindUtil.addElementToCollection(parent, COLLECTION_BINDING, child1);

                // Create a replacement with the same bizId
                AllAttributesPersistent replacement = AllAttributesPersistent.newInstance();
                replacement.setBizId(child1.getBizId());
                replacement.setText("replaced");

                BindUtil.setElementInCollection(parent.getAggregatedCollection(), replacement);
                assertEquals(1, parent.getAggregatedCollection().size());
                assertEquals("replaced", parent.getAggregatedCollection().get(0).getText());
        }
}
