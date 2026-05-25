package modules.test.domain;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

import util.AbstractH2Test;

@SuppressWarnings("static-method")
class TestDomainMiscTest extends AbstractH2Test {

        // --- Hierarchical ---

        @Test
        void hierarchicalBizModuleAndDocument() throws Exception {
                Hierarchical bean = Hierarchical.newInstance();
                assertEquals("test", bean.getBizModule());
                assertEquals("Hierarchical", bean.getBizDocument());
        }

        @Test
        void hierarchicalGetBizKeyNotNull() {
                assertNotNull(new Hierarchical().getBizKey());
        }

        @Test
        void hierarchicalTextSetAndGet() {
                Hierarchical bean = new Hierarchical();
                bean.setText("root");
                assertEquals("root", bean.getText());
        }

        @Test
        void hierarchicalParentIdSetAndGet() {
                Hierarchical bean = new Hierarchical();
                assertNull(bean.getBizParentId());
                bean.setBizParentId("parent-id");
                assertEquals("parent-id", bean.getBizParentId());
        }

        @Test
        void hierarchicalChildrenEmptyByDefault() {
                Hierarchical bean = new Hierarchical();
                assertTrue(bean.getChildren().isEmpty());
        }

        // --- ArcOneToOne ---

        @Test
        void arcOneToOneBizModuleAndDocument() throws Exception {
                ArcOneToOne bean = ArcOneToOne.newInstance();
                assertEquals("test", bean.getBizModule());
                assertEquals("ArcOneToOne", bean.getBizDocument());
        }

        @Test
        void arcOneToOneArcSetAndGet() {
                ArcOneToOne bean = new ArcOneToOne();
                assertNull(bean.getArc());
                AnyBase arc = new AnyBase();
                bean.setArc(arc);
                assertEquals(arc, bean.getArc());
        }

        // --- DeleteDuringPostDelete ---

        @Test
        void deleteDuringPostDeleteBizModuleAndDocument() throws Exception {
                DeleteDuringPostDelete bean = DeleteDuringPostDelete.newInstance();
                assertEquals("test", bean.getBizModule());
                assertEquals("DeleteDuringPostDelete", bean.getBizDocument());
        }

        @Test
        void deleteDuringPostDeleteAggAssociationSetAndGet() {
                DeleteDuringPostDelete bean = new DeleteDuringPostDelete();
                assertNull(bean.getAggregatedAssociation());
                AllAttributesPersistent assoc = new AllAttributesPersistent();
                bean.setAggregatedAssociation(assoc);
                assertEquals(assoc, bean.getAggregatedAssociation());
        }

        // --- ArcOneToMany ---

        @Test
        void arcOneToManyBizModuleAndDocument() throws Exception {
                ArcOneToMany bean = ArcOneToMany.newInstance();
                assertEquals("test", bean.getBizModule());
                assertEquals("ArcOneToMany", bean.getBizDocument());
        }

        @Test
        void arcOneToManyArcsAddAndGet() {
                ArcOneToMany bean = new ArcOneToMany();
                assertTrue(bean.getArcs().isEmpty());
                AnyBase arc = new AnyBase();
                bean.addArcsElement(arc);
                assertEquals(1, bean.getArcs().size());
                assertNotNull(bean.getArcsElementById(arc.getBizId()));
        }

        @Test
        void arcOneToManyArcsRemove() {
                ArcOneToMany bean = new ArcOneToMany();
                AnyBase arc = new AnyBase();
                bean.addArcsElement(arc);
                assertTrue(bean.removeArcsElement(arc));
                assertTrue(bean.getArcs().isEmpty());
        }

        @Test
        void arcOneToManyArcsAddAtIndexAndRemoveByIndex() {
                ArcOneToMany bean = new ArcOneToMany();
                AnyBase arc = new AnyBase();
                bean.addArcsElement(0, arc);
                assertEquals(1, bean.getArcs().size());
                bean.removeArcsElement(0);
                assertTrue(bean.getArcs().isEmpty());
        }
}
