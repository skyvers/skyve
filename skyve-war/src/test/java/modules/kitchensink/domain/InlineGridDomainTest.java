package modules.kitchensink.domain;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;
import org.skyve.domain.types.DateOnly;
import org.skyve.domain.types.Decimal5;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import modules.kitchensink.domain.InlineGrid.ConstantEnum;
import util.AbstractH2Test;

/**
 * Tests for the {@link InlineGrid} kitchensink domain bean.
 */
@SuppressWarnings("static-method")
public class InlineGridDomainTest extends AbstractH2Test {

	@Test
	void dataBuilderCreatesBean() throws Exception {
		InlineGrid bean = new DataBuilder()
				.fixture(FixtureType.crud)
				.build(InlineGrid.MODULE_NAME, InlineGrid.DOCUMENT_NAME);
		assertNotNull(bean);
	}

	@Test
	void moduleAndDocumentNames() throws Exception {
		InlineGrid bean = InlineGrid.newInstance();
		assertEquals(InlineGrid.MODULE_NAME, bean.getBizModule());
		assertEquals(InlineGrid.DOCUMENT_NAME, bean.getBizDocument());
	}

	@Test
	void setAndGetProperties() throws Exception {
		InlineGrid bean = InlineGrid.newInstance();
		bean.setConstantEnum(ConstantEnum.two2);
		bean.setConstantDomain("constVal");
		bean.setVariantDomain("varVal");
		bean.setDynamicDomain("dynVal");
		bean.setBooleanFlag(Boolean.TRUE);
		bean.setColour("blue");
		bean.setDate(new DateOnly());
		bean.setDecimal5(new Decimal5("1.23456"));
		bean.setBizOrdinal(Integer.valueOf(2));

		assertEquals(ConstantEnum.two2, bean.getConstantEnum());
		assertEquals("constVal", bean.getConstantDomain());
		assertEquals("varVal", bean.getVariantDomain());
		assertEquals("dynVal", bean.getDynamicDomain());
		assertEquals(Boolean.TRUE, bean.getBooleanFlag());
		assertEquals("blue", bean.getColour());
		assertNotNull(bean.getDate());
		assertNotNull(bean.getDecimal5());
		assertEquals(Integer.valueOf(2), bean.getBizOrdinal());
	}

	@Test
	void constantEnumFromCodeReturnsCorrectValue() {
		assertEquals(ConstantEnum.one1, ConstantEnum.fromCode("one"));
		assertEquals(ConstantEnum.two2, ConstantEnum.fromCode("two"));
		assertEquals(ConstantEnum.three3, ConstantEnum.fromCode("three"));
		assertNull(ConstantEnum.fromCode("nonexistent"));
	}

	@Test
	void constantEnumFromLocalisedDescriptionReturnsCorrectValue() {
		assertNotNull(ConstantEnum.fromLocalisedDescription(ConstantEnum.one1.toLocalisedDescription()));
		assertNull(ConstantEnum.fromLocalisedDescription("nonexistent"));
		assertNotNull(ConstantEnum.toDomainValues());
		assertEquals(3, ConstantEnum.toDomainValues().size());
	}

	@Test
	void getBizKeyNotNull() throws Exception {
		InlineGrid bean = InlineGrid.newInstance();
		assertNotNull(bean.getBizKey());
	}
}
