package modules.whosin.domain;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;
import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.GeometryFactory;

import util.AbstractH2Test;

@SuppressWarnings("static-method")
class OfficeDomainTest extends AbstractH2Test {

        @Test
        void bizModuleIsWhosin() {
                assertEquals("whosin", new Office().getBizModule());
        }

        @Test
        void bizDocumentIsOffice() {
                assertEquals("Office", new Office().getBizDocument());
        }

        @Test
        void moduleNameConstant() {
                assertEquals("whosin", Office.MODULE_NAME);
        }

        @Test
        void documentNameConstant() {
                assertEquals("Office", Office.DOCUMENT_NAME);
        }

        @Test
        void propertyNameConstants() {
                assertEquals("levelUnit", Office.levelUnitPropertyName);
                assertEquals("buildingName", Office.buildingNamePropertyName);
                assertEquals("streetAddress", Office.streetAddressPropertyName);
                assertEquals("suburb", Office.suburbPropertyName);
                assertEquals("postCode", Office.postCodePropertyName);
                assertEquals("phone", Office.phonePropertyName);
                assertEquals("description", Office.descriptionPropertyName);
                assertEquals("demoData", Office.demoDataPropertyName);
        }

        @Test
        void levelUnitSetAndGet() {
                Office bean = new Office();
                bean.setLevelUnit("Level 3");
                assertEquals("Level 3", bean.getLevelUnit());
        }

        @Test
        void buildingNameSetAndGet() {
                Office bean = new Office();
                bean.setBuildingName("HQ Building");
                assertEquals("HQ Building", bean.getBuildingName());
        }

        @Test
        void streetAddressSetAndGet() {
                Office bean = new Office();
                bean.setStreetAddress("123 Main St");
                assertEquals("123 Main St", bean.getStreetAddress());
        }

        @Test
        void suburbSetAndGet() {
                Office bean = new Office();
                bean.setSuburb("Springfield");
                assertEquals("Springfield", bean.getSuburb());
        }

        @Test
        void postCodeSetAndGet() {
                Office bean = new Office();
                bean.setPostCode("5000");
                assertEquals("5000", bean.getPostCode());
        }

        @Test
        void phoneSetAndGet() {
                Office bean = new Office();
                bean.setPhone("+61 8 1234 5678");
                assertEquals("+61 8 1234 5678", bean.getPhone());
        }

        @Test
        void descriptionSetAndGet() {
                Office bean = new Office();
                bean.setDescription("Main HQ");
                assertEquals("Main HQ", bean.getDescription());
        }

        @Test
        void demoDataSetAndGet() {
                Office bean = new Office();
                bean.setDemoData(Boolean.TRUE);
                assertEquals(Boolean.TRUE, bean.getDemoData());
        }

        @Test
        void getBizKeyReturnsNotNull() {
                assertNotNull(new Office().getBizKey());
        }

        @Test
        void boundaryNullByDefault() {
                assertNull(new Office().getBoundary());
        }

        @Test
        void boundarySetAndGet() {
                Office bean = new Office();
                var geom = new GeometryFactory().createPoint(new Coordinate(138.6, -34.9));
                bean.setBoundary(geom);
                assertEquals(geom, bean.getBoundary());
        }

        @Test
        void isManagerAndIsNotManagerAreOpposite() {
                Office bean = new Office();
                assertEquals(!bean.isManager(), bean.isNotManager());
        }
}

