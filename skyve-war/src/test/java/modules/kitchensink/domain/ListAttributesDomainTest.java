package modules.kitchensink.domain;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;

import modules.kitchensink.domain.ListAttributes.ConstantEnum;
import util.AbstractH2Test;

@SuppressWarnings("static-method")
class ListAttributesDomainTest extends AbstractH2Test {

        @Test
        void bizModuleAndDocument() throws Exception {
                ListAttributes bean = ListAttributes.newInstance();
                assertEquals("kitchensink", bean.getBizModule());
                assertEquals("ListAttributes", bean.getBizDocument());
        }

        @Test
        void getBizKeyNotNull() {
                assertNotNull(new ListAttributes().getBizKey());
        }

        @Test
        void constantEnumFromCodeAndFromLocalisedDescription() {
                assertEquals(ConstantEnum.one1, ConstantEnum.fromCode("one"));
                assertNull(ConstantEnum.fromCode("notexist"));
                assertNotNull(ConstantEnum.fromLocalisedDescription(ConstantEnum.one1.toLocalisedDescription()));
                assertNull(ConstantEnum.fromLocalisedDescription("notexist"));
        }

        @Test
        void constantEnumToDomainValues() {
                assertNotNull(ConstantEnum.toDomainValues());
                assertEquals(3, ConstantEnum.toDomainValues().size());
        }

        @Test
        void constantEnumToCodeAndToDomainValue() {
                assertEquals("one", ConstantEnum.one1.toCode());
                assertNotNull(ConstantEnum.one1.toDomainValue());
                assertEquals("one", ConstantEnum.one1.toDomainValue().getCode());
        }
}

