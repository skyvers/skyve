package modules.test.domain;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

import util.AbstractH2Test;

@SuppressWarnings("static-method")
class InverseManyToManyPersistentDomainTest extends AbstractH2Test {

        @Test
        void bizModuleAndDocument() throws Exception {
                InverseManyToManyPersistent bean = InverseManyToManyPersistent.newInstance();
                assertEquals("test", bean.getBizModule());
                assertEquals("InverseManyToManyPersistent", bean.getBizDocument());
        }

        @Test
        void getBizKeyNotNull() {
                assertNotNull(new InverseManyToManyPersistent().getBizKey());
        }

        @Test
        void aggCollectionEmptyByDefault() {
                assertTrue(new InverseManyToManyPersistent().getAggCollection().isEmpty());
        }

        @Test
        void aggCollectionAddAndGet() {
                InverseManyToManyPersistent bean = new InverseManyToManyPersistent();
                InverseManyToManyPersistent element = new InverseManyToManyPersistent();
                bean.addAggCollectionElement(element);
                assertEquals(1, bean.getAggCollection().size());
                assertNotNull(bean.getAggCollectionElementById(element.getBizId()));
        }

        @Test
        void aggCollectionRemove() {
                InverseManyToManyPersistent bean = new InverseManyToManyPersistent();
                InverseManyToManyPersistent element = new InverseManyToManyPersistent();
                bean.addAggCollectionElement(element);
                assertTrue(bean.removeAggCollectionElement(element));
                assertTrue(bean.getAggCollection().isEmpty());
        }

        @Test
        void aggCollectionAddAtIndexAndRemoveByIndex() {
                InverseManyToManyPersistent bean = new InverseManyToManyPersistent();
                InverseManyToManyPersistent element = new InverseManyToManyPersistent();
                bean.addAggCollectionElement(0, element);
                assertEquals(1, bean.getAggCollection().size());
                assertNotNull(bean.removeAggCollectionElement(0));
                assertTrue(bean.getAggCollection().isEmpty());
        }

        @Test
        void invAggCollectionEmptyByDefault() {
                assertTrue(new InverseManyToManyPersistent().getInvAggCollection().isEmpty());
        }

        @Test
        void invAggCollectionAddAndRemove() {
                InverseManyToManyPersistent bean = new InverseManyToManyPersistent();
                InverseManyToManyPersistent element = new InverseManyToManyPersistent();
                bean.addInvAggCollectionElement(element);
                assertEquals(1, bean.getInvAggCollection().size());
                assertNotNull(bean.getInvAggCollectionElementById(element.getBizId()));
                assertTrue(bean.removeInvAggCollectionElement(element));
        }

        @Test
        void invAggCollectionAddAtIndexAndRemoveByIndex() {
                InverseManyToManyPersistent bean = new InverseManyToManyPersistent();
                InverseManyToManyPersistent element = new InverseManyToManyPersistent();
                bean.addInvAggCollectionElement(0, element);
                assertEquals(1, bean.getInvAggCollection().size());
                assertNull(bean.getInvAggCollectionElementById("nonexistent"));
                assertNotNull(bean.removeInvAggCollectionElement(0));
        }
}
