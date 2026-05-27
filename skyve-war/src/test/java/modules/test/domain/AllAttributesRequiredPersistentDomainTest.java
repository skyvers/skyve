package modules.test.domain;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;
import org.skyve.domain.types.DateOnly;
import org.skyve.domain.types.DateTime;
import org.skyve.domain.types.Decimal10;
import org.skyve.domain.types.Decimal2;
import org.skyve.domain.types.Decimal5;
import org.skyve.domain.types.TimeOnly;
import org.skyve.domain.types.Timestamp;

import modules.test.domain.AllAttributesRequiredPersistent.Enum3;
import util.AbstractH2Test;

@SuppressWarnings("static-method")
class AllAttributesRequiredPersistentDomainTest extends AbstractH2Test {

	@Test
	void bizModuleIsTest() {
		AllAttributesRequiredPersistent bean = AllAttributesRequiredPersistent.newInstance();
		assertEquals("test", bean.getBizModule());
	}

	@Test
	void bizDocumentIsAllAttributesRequiredPersistent() {
		AllAttributesRequiredPersistent bean = AllAttributesRequiredPersistent.newInstance();
		assertEquals("AllAttributesRequiredPersistent", bean.getBizDocument());
	}

	@Test
	void getBizKeyNotNull() {
		AllAttributesRequiredPersistent bean = AllAttributesRequiredPersistent.newInstance();
		assertNotNull(bean.getBizKey());
	}

	@Test
	void enum3DefaultIsNull() {
		AllAttributesRequiredPersistent bean = AllAttributesRequiredPersistent.newInstance();
		assertNull(bean.getEnum3());
	}

	@Test
	void enum3SetAndGet() {
		AllAttributesRequiredPersistent bean = AllAttributesRequiredPersistent.newInstance();
		bean.setEnum3(Enum3.one);
		assertEquals(Enum3.one, bean.getEnum3());
	}

	@Test
	void enum3ToCode() {
		assertEquals("one", Enum3.one.toCode());
		assertEquals("two", Enum3.two.toCode());
		assertEquals("three", Enum3.three.toCode());
	}

	@Test
	void enum3ToLocalisedDescription() {
		assertNotNull(Enum3.one.toLocalisedDescription());
		assertNotNull(Enum3.two.toLocalisedDescription());
	}

	@Test
	void enum3ToDomainValue() {
		assertNotNull(Enum3.one.toDomainValue());
		assertEquals("one", Enum3.one.toDomainValue().getCode());
	}

	@Test
	void enum3ToDomainValues() {
		assertEquals(3, Enum3.toDomainValues().size());
	}

	@Test
	void enum3FromCode() {
		assertEquals(Enum3.two, Enum3.fromCode("two"));
	}

	@Test
	void enum3FromCodeUnknownReturnsNull() {
		assertNull(Enum3.fromCode("notexist"));
	}

	@Test
	void enum3FromLocalisedDescription() {
		assertNotNull(Enum3.fromLocalisedDescription("one"));
	}

	@Test
	void enum3FromLocalisedDescriptionUnknownReturnsNull() {
		assertNull(Enum3.fromLocalisedDescription("notexist"));
	}

	@Test
	void booleanFlagSetAndGet() {
		AllAttributesRequiredPersistent bean = AllAttributesRequiredPersistent.newInstance();
		bean.setBooleanFlag(Boolean.TRUE);
		assertEquals(Boolean.TRUE, bean.getBooleanFlag());
	}

	@Test
	void colourSetAndGet() {
		AllAttributesRequiredPersistent bean = AllAttributesRequiredPersistent.newInstance();
		bean.setColour("#FF0000");
		assertEquals("#FF0000", bean.getColour());
	}

	@Test
	void idSetAndGet() {
		AllAttributesRequiredPersistent bean = AllAttributesRequiredPersistent.newInstance();
		bean.setId("someId");
		assertEquals("someId", bean.getId());
	}

	@Test
	void markupSetAndGet() {
		AllAttributesRequiredPersistent bean = AllAttributesRequiredPersistent.newInstance();
		bean.setMarkup("<b>bold</b>");
		assertEquals("<b>bold</b>", bean.getMarkup());
	}

	@Test
	void memoSetAndGet() {
		AllAttributesRequiredPersistent bean = AllAttributesRequiredPersistent.newInstance();
		bean.setMemo("memo text");
		assertEquals("memo text", bean.getMemo());
	}

	@Test
	void textSetAndGet() {
		AllAttributesRequiredPersistent bean = AllAttributesRequiredPersistent.newInstance();
		bean.setText("hello");
		assertEquals("hello", bean.getText());
	}

	@Test
	void normalIntegerSetAndGet() {
		AllAttributesRequiredPersistent bean = AllAttributesRequiredPersistent.newInstance();
		bean.setNormalInteger(Integer.valueOf(42));
		assertEquals(Integer.valueOf(42), bean.getNormalInteger());
	}

	@Test
	void longIntegerSetAndGet() {
		AllAttributesRequiredPersistent bean = AllAttributesRequiredPersistent.newInstance();
		bean.setLongInteger(Long.valueOf(99L));
		assertEquals(Long.valueOf(99L), bean.getLongInteger());
	}

	@Test
	void decimal10SetAndGet() {
		AllAttributesRequiredPersistent bean = AllAttributesRequiredPersistent.newInstance();
		Decimal10 val = new Decimal10("1.5");
		bean.setDecimal10(val);
		assertEquals(val, bean.getDecimal10());
	}

	@Test
	void decimal2SetAndGet() {
		AllAttributesRequiredPersistent bean = AllAttributesRequiredPersistent.newInstance();
		Decimal2 val = new Decimal2("3.14");
		bean.setDecimal2(val);
		assertEquals(val, bean.getDecimal2());
	}

	@Test
	void decimal5SetAndGet() {
		AllAttributesRequiredPersistent bean = AllAttributesRequiredPersistent.newInstance();
		Decimal5 val = new Decimal5("2.71828");
		bean.setDecimal5(val);
		assertEquals(val, bean.getDecimal5());
	}

	@Test
	void dateSetAndGet() {
		AllAttributesRequiredPersistent bean = AllAttributesRequiredPersistent.newInstance();
		DateOnly d = new DateOnly();
		bean.setDate(d);
		assertEquals(d, bean.getDate());
	}

	@Test
	void dateTimeSetAndGet() {
		AllAttributesRequiredPersistent bean = AllAttributesRequiredPersistent.newInstance();
		DateTime dt = new DateTime();
		bean.setDateTime(dt);
		assertEquals(dt, bean.getDateTime());
	}

	@Test
	void timeSetAndGet() {
		AllAttributesRequiredPersistent bean = AllAttributesRequiredPersistent.newInstance();
		TimeOnly t = new TimeOnly();
		bean.setTime(t);
		assertEquals(t, bean.getTime());
	}

	@Test
	void timestampSetAndGet() {
		AllAttributesRequiredPersistent bean = AllAttributesRequiredPersistent.newInstance();
		Timestamp ts = new Timestamp();
		bean.setTimestamp(ts);
		assertEquals(ts, bean.getTimestamp());
	}

	@Test
	void nullAggregatedAssociation() {
		AllAttributesRequiredPersistent bean = AllAttributesRequiredPersistent.newInstance();
		AllAttributesRequiredPersistent assoc = AllAttributesRequiredPersistent.newInstance();
		bean.setAggregatedAssociation(assoc);
		assertNotNull(bean.getAggregatedAssociation());
		bean.nullAggregatedAssociation();
		assertNull(bean.getAggregatedAssociation());
	}

        @Test
        void aggregatedCollectionAddAndGet() {
                AllAttributesRequiredPersistent bean = AllAttributesRequiredPersistent.newInstance();
                assertTrue(bean.getAggregatedCollection().isEmpty());
                AllAttributesRequiredPersistent element = new AllAttributesRequiredPersistent();
                bean.addAggregatedCollectionElement(element);
                assertEquals(1, bean.getAggregatedCollection().size());
                assertNotNull(bean.getAggregatedCollectionElementById(element.getBizId()));
        }

        @Test
        void aggregatedCollectionRemove() {
                AllAttributesRequiredPersistent bean = AllAttributesRequiredPersistent.newInstance();
                AllAttributesRequiredPersistent element = new AllAttributesRequiredPersistent();
                bean.addAggregatedCollectionElement(element);
                assertTrue(bean.removeAggregatedCollectionElement(element));
                assertTrue(bean.getAggregatedCollection().isEmpty());
        }

        @Test
        void aggregatedCollectionAddAtIndexAndRemoveByIndex() {
                AllAttributesRequiredPersistent bean = AllAttributesRequiredPersistent.newInstance();
                AllAttributesRequiredPersistent element = new AllAttributesRequiredPersistent();
                bean.addAggregatedCollectionElement(0, element);
                assertEquals(1, bean.getAggregatedCollection().size());
                assertNotNull(bean.removeAggregatedCollectionElement(0));
        }

        @Test
        void geometrySetAndGet() {
                AllAttributesRequiredPersistent bean = AllAttributesRequiredPersistent.newInstance();
                var geom = new org.locationtech.jts.geom.GeometryFactory()
                                .createPoint(new org.locationtech.jts.geom.Coordinate(138.6, -34.9));
                bean.setGeometry(geom);
                assertEquals(geom, bean.getGeometry());
        }

        @Test
        void inverseAggregatedAssociationAddAndGet() {
                AllAttributesRequiredPersistent bean = AllAttributesRequiredPersistent.newInstance();
                assertTrue(bean.getInverseAggregatedAssociation().isEmpty());
                AllAttributesRequiredPersistent element = new AllAttributesRequiredPersistent();
                bean.addInverseAggregatedAssociationElement(element);
                assertEquals(1, bean.getInverseAggregatedAssociation().size());
        }

        @Test
        void aggregatedCollectionGetAndSetById() {
                AllAttributesRequiredPersistent bean = AllAttributesRequiredPersistent.newInstance();
                AllAttributesRequiredPersistent element = new AllAttributesRequiredPersistent();
                bean.addAggregatedCollectionElement(element);
                assertNotNull(bean.getAggregatedCollectionElementById(element.getBizId()));
                assertNull(bean.getAggregatedCollectionElementById("nonexistent"));
                AllAttributesRequiredPersistent replacement = new AllAttributesRequiredPersistent();
                replacement.setBizId(element.getBizId());
                bean.setAggregatedCollectionElementById(element.getBizId(), replacement);
                assertTrue(bean.getAggregatedCollection().contains(replacement));
        }

        @Test
        void inverseAggregatedAssociationRemove() {
                AllAttributesRequiredPersistent bean = AllAttributesRequiredPersistent.newInstance();
                AllAttributesRequiredPersistent element = new AllAttributesRequiredPersistent();
                bean.addInverseAggregatedAssociationElement(element);
                assertTrue(bean.removeInverseAggregatedAssociationElement(element));
                assertTrue(bean.getInverseAggregatedAssociation().isEmpty());
        }

        @Test
        void inverseAggregatedAssociationAddAtIndexAndRemoveByIndex() {
                AllAttributesRequiredPersistent bean = AllAttributesRequiredPersistent.newInstance();
                AllAttributesRequiredPersistent element = new AllAttributesRequiredPersistent();
                bean.addInverseAggregatedAssociationElement(0, element);
                assertEquals(1, bean.getInverseAggregatedAssociation().size());
                assertNotNull(bean.removeInverseAggregatedAssociationElement(0));
                assertTrue(bean.getInverseAggregatedAssociation().isEmpty());
        }

        @Test
        void inverseAggregatedAssociationGetAndSetById() {
                AllAttributesRequiredPersistent bean = AllAttributesRequiredPersistent.newInstance();
                AllAttributesRequiredPersistent element = new AllAttributesRequiredPersistent();
                bean.addInverseAggregatedAssociationElement(element);
                assertNotNull(bean.getInverseAggregatedAssociationElementById(element.getBizId()));
                assertNull(bean.getInverseAggregatedAssociationElementById("nonexistent"));
                AllAttributesRequiredPersistent replacement = new AllAttributesRequiredPersistent();
                replacement.setBizId(element.getBizId());
                bean.setInverseAggregatedAssociationElementById(element.getBizId(), replacement);
                assertTrue(bean.getInverseAggregatedAssociation().contains(replacement));
        }
}
