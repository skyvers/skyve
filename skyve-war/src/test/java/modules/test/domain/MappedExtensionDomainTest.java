package modules.test.domain;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

import modules.test.MappedExtensionSingleStrategy.MappedExtensionSingleStrategyExtension;
import modules.test.MappedExtensionJoinedStrategy.MappedExtensionJoinedStrategyExtension;
import util.AbstractH2Test;

@SuppressWarnings("static-method")
class MappedExtensionDomainTest extends AbstractH2Test {

        // --- MappedExtensionSingleStrategy (via extension) ---

        @Test
        void singleStrategyBizModuleAndDocument() throws Exception {
                MappedExtensionSingleStrategy bean = MappedExtensionSingleStrategy.newInstance();
                assertEquals("test", bean.getBizModule());
                assertEquals("MappedExtensionSingleStrategy", bean.getBizDocument());
        }

        @Test
        void singleStrategyGetBizKeyNotNull() throws Exception {
                MappedExtensionSingleStrategy bean = MappedExtensionSingleStrategy.newInstance();
                assertNotNull(bean.getBizKey());
        }

        @Test
        void singleStrategyAggAssociationSetAndGet() throws Exception {
                MappedExtensionSingleStrategy bean = MappedExtensionSingleStrategy.newInstance();
                assertNull(bean.getAggregatedAssociation());
                MappedExtensionSingleStrategyExtension assoc = new MappedExtensionSingleStrategyExtension();
                bean.setAggregatedAssociation(assoc);
                assertEquals(assoc, bean.getAggregatedAssociation());
        }

        @Test
        void singleStrategyNullAggAssociation() throws Exception {
                MappedExtensionSingleStrategy bean = MappedExtensionSingleStrategy.newInstance();
                MappedExtensionSingleStrategyExtension assoc = new MappedExtensionSingleStrategyExtension();
                bean.setAggregatedAssociation(assoc);
                bean.nullAggregatedAssociation();
                assertNull(bean.getAggregatedAssociation());
        }

        @Test
        void singleStrategyAggCollectionAddAndGet() throws Exception {
                MappedExtensionSingleStrategy bean = MappedExtensionSingleStrategy.newInstance();
                assertTrue(bean.getAggregatedCollection().isEmpty());
                MappedExtensionSingleStrategyExtension element = new MappedExtensionSingleStrategyExtension();
                bean.addAggregatedCollectionElement(element);
                assertEquals(1, bean.getAggregatedCollection().size());
        }

        @Test
        void singleStrategyComposedCollectionAddAndGet() throws Exception {
                MappedExtensionSingleStrategy bean = MappedExtensionSingleStrategy.newInstance();
                assertTrue(bean.getComposedCollection().isEmpty());
                MappedExtensionSingleStrategyExtension element = new MappedExtensionSingleStrategyExtension();
                bean.addComposedCollectionElement(element);
                assertEquals(1, bean.getComposedCollection().size());
        }

        // --- MappedExtensionJoinedStrategy (via extension) ---

        @Test
        void joinedStrategyBizModuleAndDocument() throws Exception {
                MappedExtensionJoinedStrategy bean = MappedExtensionJoinedStrategy.newInstance();
                assertEquals("test", bean.getBizModule());
                assertEquals("MappedExtensionJoinedStrategy", bean.getBizDocument());
        }

        @Test
        void joinedStrategyAggAssociationSetAndGet() throws Exception {
                MappedExtensionJoinedStrategy bean = MappedExtensionJoinedStrategy.newInstance();
                assertNull(bean.getAggregatedAssociation());
                MappedExtensionJoinedStrategyExtension assoc = new MappedExtensionJoinedStrategyExtension();
                bean.setAggregatedAssociation(assoc);
                assertEquals(assoc, bean.getAggregatedAssociation());
        }

        @Test
        void joinedStrategyComposedCollectionAddAndGet() throws Exception {
                MappedExtensionJoinedStrategy bean = MappedExtensionJoinedStrategy.newInstance();
                assertTrue(bean.getComposedCollection().isEmpty());
                MappedExtensionJoinedStrategyExtension element = new MappedExtensionJoinedStrategyExtension();
                bean.addComposedCollectionElement(element);
                assertEquals(1, bean.getComposedCollection().size());
        }
}
