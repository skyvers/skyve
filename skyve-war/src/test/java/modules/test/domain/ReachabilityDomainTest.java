package modules.test.domain;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

import util.AbstractH2Test;

@SuppressWarnings("static-method")
class ReachabilityDomainTest extends AbstractH2Test {

        @Test
        void bizModuleAndDocument() throws Exception {
                Reachability bean = Reachability.newInstance();
                assertEquals("test", bean.getBizModule());
                assertEquals("Reachability", bean.getBizDocument());
        }

        @Test
        void getBizKeyNotNull() {
                assertNotNull(new Reachability().getBizKey());
        }

        @Test
        void textSetAndGet() {
                Reachability bean = new Reachability();
                bean.setText("hello");
                assertEquals("hello", bean.getText());
        }

        @Test
        void nonPersistentAggregatedAssociationSetAndGet() {
                Reachability bean = new Reachability();
                assertNull(bean.getNonPersistentAggregatedAssociation());
                AllAttributesPersistent assoc = new AllAttributesPersistent();
                bean.setNonPersistentAggregatedAssociation(assoc);
                assertEquals(assoc, bean.getNonPersistentAggregatedAssociation());
        }

        @Test
        void persistentAggregatedAssociationSetAndGet() {
                Reachability bean = new Reachability();
                assertNull(bean.getPersistentAggregatedAssociation());
                AllAttributesPersistent assoc = new AllAttributesPersistent();
                bean.setPersistentAggregatedAssociation(assoc);
                assertEquals(assoc, bean.getPersistentAggregatedAssociation());
        }

        @Test
        void nonPersistentAggregatedCollectionAddAndGet() {
                Reachability bean = new Reachability();
                assertTrue(bean.getNonPersistentAggregatedCollection().isEmpty());
                AllAttributesPersistent element = new AllAttributesPersistent();
                bean.addNonPersistentAggregatedCollectionElement(element);
                assertEquals(1, bean.getNonPersistentAggregatedCollection().size());
                assertNotNull(bean.getNonPersistentAggregatedCollectionElementById(element.getBizId()));
        }

        @Test
        void persistentAggregatedCollectionAddAndGet() {
                Reachability bean = new Reachability();
                assertTrue(bean.getPersistentAggregatedCollection().isEmpty());
                AllAttributesPersistent element = new AllAttributesPersistent();
                bean.addPersistentAggregatedCollectionElement(element);
                assertEquals(1, bean.getPersistentAggregatedCollection().size());
        }

        @Test
        void nonPersistentAggregatedCollectionRemove() {
                Reachability bean = new Reachability();
                AllAttributesPersistent element = new AllAttributesPersistent();
                bean.addNonPersistentAggregatedCollectionElement(element);
                assertTrue(bean.removeNonPersistentAggregatedCollectionElement(element));
                assertTrue(bean.getNonPersistentAggregatedCollection().isEmpty());
        }
}
