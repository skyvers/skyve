package modules.kitchensink.domain;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;
import org.skyve.domain.types.DateOnly;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import modules.kitchensink.domain.OrderedGrid.Enum3;
import util.AbstractH2Test;

/**
 * Tests for the {@link OrderedGrid} kitchensink domain bean.
 */
@SuppressWarnings("static-method")
class OrderedGridDomainTest extends AbstractH2Test {

	@Test
	void dataBuilderCreatesBean() {
		OrderedGrid bean = new DataBuilder()
				.fixture(FixtureType.crud)
				.build(OrderedGrid.MODULE_NAME, OrderedGrid.DOCUMENT_NAME);
		assertNotNull(bean);
	}

	@Test
	void moduleAndDocumentNames() {
		OrderedGrid bean = OrderedGrid.newInstance();
		assertEquals(OrderedGrid.MODULE_NAME, bean.getBizModule());
		assertEquals(OrderedGrid.DOCUMENT_NAME, bean.getBizDocument());
	}

	@Test
	void setAndGetProperties() {
		OrderedGrid bean = OrderedGrid.newInstance();
		bean.setBooleanFlag(Boolean.TRUE);
		bean.setColour("red");
		bean.setDate(new DateOnly());
		bean.setEnum3(Enum3.two);
		bean.setBizOrdinal(Integer.valueOf(1));

		assertEquals(Boolean.TRUE, bean.getBooleanFlag());
		assertEquals("red", bean.getColour());
		assertNotNull(bean.getDate());
		assertEquals(Enum3.two, bean.getEnum3());
		assertEquals(Integer.valueOf(1), bean.getBizOrdinal());
	}

	@Test
	void enum3FromCodeReturnsCorrectValue() {
		assertEquals(Enum3.one, Enum3.fromCode("one"));
		assertEquals(Enum3.two, Enum3.fromCode("two"));
		assertEquals(Enum3.three, Enum3.fromCode("three"));
		assertNull(Enum3.fromCode("nonexistent"));
	}

	@Test
	void enum3FromLocalisedDescriptionReturnsCorrectValue() {
		assertNotNull(Enum3.fromLocalisedDescription(Enum3.one.toLocalisedDescription()));
		assertNull(Enum3.fromLocalisedDescription("nonexistent"));
		assertNotNull(Enum3.toDomainValues());
	}
}
