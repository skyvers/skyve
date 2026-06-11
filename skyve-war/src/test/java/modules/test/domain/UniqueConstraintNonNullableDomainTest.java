package modules.test.domain;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;

import modules.test.domain.UniqueConstraintNonNullable.Enum3;
import util.AbstractH2Test;

@SuppressWarnings("static-method")
class UniqueConstraintNonNullableDomainTest extends AbstractH2Test {

        @Test
        void bizModuleAndDocument() {
                UniqueConstraintNonNullable bean = UniqueConstraintNonNullable.newInstance();
                assertEquals("test", bean.getBizModule());
                assertEquals("UniqueConstraintNonNullable", bean.getBizDocument());
        }

        @Test
        void getBizKeyNotNull() {
                assertNotNull(new UniqueConstraintNonNullable().getBizKey());
        }

        @Test
        void booleanFlagSetAndGet() {
                UniqueConstraintNonNullable bean = new UniqueConstraintNonNullable();
                bean.setBooleanFlag(Boolean.TRUE);
                assertEquals(Boolean.TRUE, bean.getBooleanFlag());
        }

        @Test
        void textSetAndGet() {
                UniqueConstraintNonNullable bean = new UniqueConstraintNonNullable();
                bean.setText("hello");
                assertEquals("hello", bean.getText());
        }

        @Test
        void enum3SetAndGet() {
                UniqueConstraintNonNullable bean = new UniqueConstraintNonNullable();
                bean.setEnum3(Enum3.three);
                assertEquals(Enum3.three, bean.getEnum3());
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
        void enum3ToCodeAndToDomainValue() {
                assertEquals("one", Enum3.one.toCode());
                assertNotNull(Enum3.one.toDomainValue());
        }
}
