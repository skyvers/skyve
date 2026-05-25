package modules.test.domain;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;

import modules.test.domain.AllAttributesEmbedded.Enum3;
import util.AbstractH2Test;

@SuppressWarnings("static-method")
class AllAttributesEmbeddedDomainTest extends AbstractH2Test {

        @Test
        void bizModuleAndDocument() throws Exception {
                AllAttributesEmbedded bean = AllAttributesEmbedded.newInstance();
                assertEquals("test", bean.getBizModule());
                assertEquals("AllAttributesEmbedded", bean.getBizDocument());
        }

        @Test
        void getBizKeyNotNull() {
                assertNotNull(new AllAttributesEmbedded().getBizKey());
        }

        @Test
        void enum3SetAndGet() {
                AllAttributesEmbedded bean = new AllAttributesEmbedded();
                bean.setEnum3(Enum3.two);
                assertEquals(Enum3.two, bean.getEnum3());
        }

        @Test
        void enum3FromCodeAndFromLocalisedDescription() {
                assertNotNull(Enum3.fromCode(Enum3.one.toCode()));
                assertNull(Enum3.fromCode("notexist"));
                assertNotNull(Enum3.fromLocalisedDescription(Enum3.one.toLocalisedDescription()));
                assertNull(Enum3.fromLocalisedDescription("notexist"));
        }

        @Test
        void enum3ToDomainValues() {
                assertNotNull(Enum3.toDomainValues());
                assertEquals(3, Enum3.toDomainValues().size());
        }

        @Test
        void parentSetAndGet() {
                AllAttributesEmbedded bean = new AllAttributesEmbedded();
                assertNull(bean.getParent());
                AllAttributesPersistent parent = new AllAttributesPersistent();
                bean.setParent(parent);
                assertEquals(parent, bean.getParent());
        }

        @Test
        void enum3ToCodeAndToDomainValue() {
                assertEquals("one", Enum3.one.toCode());
                assertNotNull(Enum3.one.toDomainValue());
                assertEquals("one", Enum3.one.toDomainValue().getCode());
        }
}
