package modules.kitchensink.domain;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;

import modules.kitchensink.domain.InlineGrid.ConstantEnum;
import modules.kitchensink.domain.OrderedGrid.Enum3;
import util.AbstractH2Test;

@SuppressWarnings("static-method")
class KitchenSinkChildDomainTest extends AbstractH2Test {

        // --- InlineGrid ---

        @Test
        void inlineGridBizModuleAndDocument() {
                InlineGrid bean = new InlineGrid();
                assertEquals("kitchensink", bean.getBizModule());
                assertEquals("InlineGrid", bean.getBizDocument());
        }

        @Test
        void inlineGridGetBizKeyNotNull() {
                assertNotNull(new InlineGrid().getBizKey());
        }

        @Test
        void inlineGridConstantEnumSetAndGet() {
                InlineGrid bean = new InlineGrid();
                bean.setConstantEnum(ConstantEnum.one1);
                assertEquals(ConstantEnum.one1, bean.getConstantEnum());
        }

        @Test
        void inlineGridConstantEnumFromCodeAndToDomainValues() {
                assertEquals(ConstantEnum.one1, ConstantEnum.fromCode("one"));
                assertNull(ConstantEnum.fromCode("notexist"));
                assertNull(ConstantEnum.fromLocalisedDescription("notexist"));
                assertNotNull(ConstantEnum.fromLocalisedDescription(ConstantEnum.one1.toLocalisedDescription()));
                assertNotNull(ConstantEnum.toDomainValues());
                assertEquals(3, ConstantEnum.toDomainValues().size());
        }

        @Test
        void inlineGridFieldsSetAndGet() {
                InlineGrid bean = new InlineGrid();
                bean.setConstantDomain("cd");
                bean.setVariantDomain("vd");
                bean.setDynamicDomain("dd");
                bean.setBooleanFlag(Boolean.TRUE);
                bean.setColour("#ffffff");
                assertEquals("cd", bean.getConstantDomain());
                assertEquals("vd", bean.getVariantDomain());
                assertEquals("dd", bean.getDynamicDomain());
                assertEquals(Boolean.TRUE, bean.getBooleanFlag());
                assertEquals("#ffffff", bean.getColour());
        }

        @Test
        void inlineGridParentSetAndGet() {
                InlineGrid bean = new InlineGrid();
                assertNull(bean.getParent());
                KitchenSink parent = new KitchenSink();
                bean.setParent(parent);
                assertEquals(parent, bean.getParent());
        }

        // --- OrderedGrid ---

        @Test
        void orderedGridBizModuleAndDocument() {
                OrderedGrid bean = new OrderedGrid();
                assertEquals("kitchensink", bean.getBizModule());
                assertEquals("OrderedGrid", bean.getBizDocument());
        }

        @Test
        void orderedGridGetBizKeyNotNull() {
                assertNotNull(new OrderedGrid().getBizKey());
        }

        @Test
        void orderedGridEnum3SetAndGet() {
                OrderedGrid bean = new OrderedGrid();
                bean.setEnum3(Enum3.two);
                assertEquals(Enum3.two, bean.getEnum3());
        }

        @Test
        void orderedGridEnum3FromCodeAndToDomainValues() {
                assertEquals(Enum3.one, Enum3.fromCode("one"));
                assertNull(Enum3.fromCode("notexist"));
                assertNull(Enum3.fromLocalisedDescription("notexist"));
                assertNotNull(Enum3.fromLocalisedDescription(Enum3.one.toLocalisedDescription()));
                assertNotNull(Enum3.toDomainValues());
                assertEquals(3, Enum3.toDomainValues().size());
        }

        @Test
        void orderedGridFieldsSetAndGet() {
                OrderedGrid bean = new OrderedGrid();
                bean.setBooleanFlag(Boolean.FALSE);
                bean.setColour("#000000");
                assertEquals(Boolean.FALSE, bean.getBooleanFlag());
                assertEquals("#000000", bean.getColour());
        }

        @Test
        void orderedGridParentAndOrdinalSetAndGet() {
                OrderedGrid bean = new OrderedGrid();
                assertNull(bean.getParent());
                KitchenSink parent = new KitchenSink();
                bean.setParent(parent);
                assertEquals(parent, bean.getParent());
		bean.setBizOrdinal(Integer.valueOf(1));
                assertEquals(Integer.valueOf(1), bean.getBizOrdinal());
        }

        // --- DataRepeater ---

        @Test
        void dataRepeaterBizModuleAndDocument() {
                DataRepeater bean = new DataRepeater();
                assertEquals("kitchensink", bean.getBizModule());
                assertEquals("DataRepeater", bean.getBizDocument());
        }

        @Test
        void dataRepeaterBlurbSetAndGet() {
                DataRepeater bean = new DataRepeater();
                bean.setBlurb("test blurb");
                assertEquals("test blurb", bean.getBlurb());
        }

        @Test
        void dataRepeaterParentSetAndGet() {
                DataRepeater bean = new DataRepeater();
                assertNull(bean.getParent());
                KitchenSink parent = new KitchenSink();
                bean.setParent(parent);
                assertEquals(parent, bean.getParent());
        }

        // --- ContainerGrid ---

        @Test
        void containerGridBizModuleAndDocument() {
                ContainerGrid bean = ContainerGrid.newInstance();
                assertEquals("kitchensink", bean.getBizModule());
                assertEquals("ContainerGrid", bean.getBizDocument());
        }

        @Test
        void containerGridGetBizKeyNotNull() {
                assertNotNull(new ContainerGrid().getBizKey());
        }
}
