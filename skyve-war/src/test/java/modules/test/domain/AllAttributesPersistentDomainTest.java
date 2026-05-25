package modules.test.domain;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
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

import modules.test.domain.AllAttributesEmbedded;
import modules.test.domain.AllAttributesPersistent.Enum3;
import util.AbstractH2Test;

@SuppressWarnings("static-method")
public class AllAttributesPersistentDomainTest extends AbstractH2Test {

	@Test
	void bizModuleIsTest() throws Exception {
		AllAttributesPersistent bean = AllAttributesPersistent.newInstance();
		assertEquals("test", bean.getBizModule());
	}

	@Test
	void bizDocumentIsAllAttributesPersistent() throws Exception {
		AllAttributesPersistent bean = AllAttributesPersistent.newInstance();
		assertEquals("AllAttributesPersistent", bean.getBizDocument());
	}

	@Test
	void getBizKeyNotNull() throws Exception {
		AllAttributesPersistent bean = AllAttributesPersistent.newInstance();
		assertNotNull(bean.getBizKey());
	}

	@Test
	void enum3DefaultIsOne() throws Exception {
		AllAttributesPersistent bean = AllAttributesPersistent.newInstance();
		assertEquals(Enum3.one, bean.getEnum3());
	}

	@Test
	void enum3SetAndGet() throws Exception {
		AllAttributesPersistent bean = AllAttributesPersistent.newInstance();
		bean.setEnum3(Enum3.two);
		assertEquals(Enum3.two, bean.getEnum3());
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
		assertNotNull(Enum3.three.toLocalisedDescription());
	}

	@Test
	void enum3ToDomainValue() {
		assertNotNull(Enum3.one.toDomainValue());
		assertEquals("one", Enum3.one.toDomainValue().getCode());
	}

	@Test
	void enum3ToDomainValues() {
		assertNotNull(Enum3.toDomainValues());
		assertEquals(3, Enum3.toDomainValues().size());
	}

	@Test
	void enum3FromCode() {
		assertEquals(Enum3.one, Enum3.fromCode("one"));
		assertEquals(Enum3.two, Enum3.fromCode("two"));
		assertEquals(Enum3.three, Enum3.fromCode("three"));
	}

	@Test
	void enum3FromCodeUnknownReturnsNull() {
		assertNull(Enum3.fromCode("nonexistent_xyz"));
	}

	@Test
	void enum3FromLocalisedDescription() {
		assertNotNull(Enum3.fromLocalisedDescription(Enum3.one.toLocalisedDescription()));
	}

	@Test
	void enum3FromLocalisedDescriptionUnknownReturnsNull() {
		assertNull(Enum3.fromLocalisedDescription("nonexistent_xyz_description"));
	}

	@Test
	void colourSetAndGet() throws Exception {
		AllAttributesPersistent bean = AllAttributesPersistent.newInstance();
		bean.setColour("#FF0000");
		assertEquals("#FF0000", bean.getColour());
	}

	@Test
	void idSetAndGet() throws Exception {
		AllAttributesPersistent bean = AllAttributesPersistent.newInstance();
		bean.setId("testId123");
		assertEquals("testId123", bean.getId());
	}

	@Test
	void markupSetAndGet() throws Exception {
		AllAttributesPersistent bean = AllAttributesPersistent.newInstance();
		bean.setMarkup("<b>bold</b>");
		assertEquals("<b>bold</b>", bean.getMarkup());
	}

	@Test
	void memoSetAndGet() throws Exception {
		AllAttributesPersistent bean = AllAttributesPersistent.newInstance();
		bean.setMemo("some memo text");
		assertEquals("some memo text", bean.getMemo());
	}

	@Test
	void textSetAndGet() throws Exception {
		AllAttributesPersistent bean = AllAttributesPersistent.newInstance();
		bean.setText("hello world");
		assertEquals("hello world", bean.getText());
	}

	@Test
	void normalIntegerSetAndGet() throws Exception {
		AllAttributesPersistent bean = AllAttributesPersistent.newInstance();
		bean.setNormalInteger(Integer.valueOf(42));
		assertEquals(Integer.valueOf(42), bean.getNormalInteger());
	}

	@Test
	void longIntegerSetAndGet() throws Exception {
		AllAttributesPersistent bean = AllAttributesPersistent.newInstance();
		bean.setLongInteger(Long.valueOf(123456789L));
		assertEquals(Long.valueOf(123456789L), bean.getLongInteger());
	}

	@Test
	void booleanFlagSetAndGet() throws Exception {
		AllAttributesPersistent bean = AllAttributesPersistent.newInstance();
		bean.setBooleanFlag(Boolean.TRUE);
		assertEquals(Boolean.TRUE, bean.getBooleanFlag());
	}

	@Test
	void decimal10SetAndGet() throws Exception {
		AllAttributesPersistent bean = AllAttributesPersistent.newInstance();
		Decimal10 val = new Decimal10("1234567890.1234567890");
		bean.setDecimal10(val);
		assertEquals(val, bean.getDecimal10());
	}

	@Test
	void decimal2SetAndGet() throws Exception {
		AllAttributesPersistent bean = AllAttributesPersistent.newInstance();
		Decimal2 val = new Decimal2("12.34");
		bean.setDecimal2(val);
		assertEquals(val, bean.getDecimal2());
	}

	@Test
	void decimal5SetAndGet() throws Exception {
		AllAttributesPersistent bean = AllAttributesPersistent.newInstance();
		Decimal5 val = new Decimal5("123.45678");
		bean.setDecimal5(val);
		assertEquals(val, bean.getDecimal5());
	}

	@Test
	void dateSetAndGet() throws Exception {
		AllAttributesPersistent bean = AllAttributesPersistent.newInstance();
		DateOnly date = new DateOnly();
		bean.setDate(date);
		assertEquals(date, bean.getDate());
	}

	@Test
	void dateTimeSetAndGet() throws Exception {
		AllAttributesPersistent bean = AllAttributesPersistent.newInstance();
		DateTime dateTime = new DateTime();
		bean.setDateTime(dateTime);
		assertEquals(dateTime, bean.getDateTime());
	}

	@Test
	void timeSetAndGet() throws Exception {
		AllAttributesPersistent bean = AllAttributesPersistent.newInstance();
		TimeOnly time = new TimeOnly();
		bean.setTime(time);
		assertEquals(time, bean.getTime());
	}

	@Test
	void timestampSetAndGet() throws Exception {
		AllAttributesPersistent bean = AllAttributesPersistent.newInstance();
		Timestamp ts = new Timestamp();
		bean.setTimestamp(ts);
		assertEquals(ts, bean.getTimestamp());
	}

	@Test
	void isConditionAlwaysTrue() throws Exception {
		AllAttributesPersistent bean = AllAttributesPersistent.newInstance();
		assertTrue(bean.isCondition());
	}

	@Test
	void isNotConditionAlwaysFalse() throws Exception {
		AllAttributesPersistent bean = AllAttributesPersistent.newInstance();
		assertFalse(bean.isNotCondition());
	}

        @Test
        void aggregatedAssociationSetAndGet() throws Exception {
                AllAttributesPersistent bean = AllAttributesPersistent.newInstance();
                assertNull(bean.getAggregatedAssociation());
                AllAttributesPersistent assoc = new AllAttributesPersistent();
                bean.setAggregatedAssociation(assoc);
                assertEquals(assoc, bean.getAggregatedAssociation());
        }

        @Test
        void nullAggregatedAssociationClearsField() throws Exception {
                AllAttributesPersistent bean = AllAttributesPersistent.newInstance();
                AllAttributesPersistent assoc = new AllAttributesPersistent();
                bean.setAggregatedAssociation(assoc);
                bean.nullAggregatedAssociation();
                assertNull(bean.getAggregatedAssociation());
        }

        @Test
        void composedAssociationSetAndGet() throws Exception {
                AllAttributesPersistent bean = AllAttributesPersistent.newInstance();
                assertNull(bean.getComposedAssociation());
                AllAttributesPersistent assoc = new AllAttributesPersistent();
                bean.setComposedAssociation(assoc);
                assertEquals(assoc, bean.getComposedAssociation());
        }

        @Test
        void embeddedAssociationSetAndGet() throws Exception {
                AllAttributesPersistent bean = AllAttributesPersistent.newInstance();
                assertNull(bean.getEmbeddedAssociation());
                AllAttributesEmbedded embedded = new AllAttributesEmbedded();
                bean.setEmbeddedAssociation(embedded);
                assertEquals(embedded, bean.getEmbeddedAssociation());
        }

        @Test
        void geometrySetAndGet() throws Exception {
                AllAttributesPersistent bean = AllAttributesPersistent.newInstance();
                var geom = new org.locationtech.jts.geom.GeometryFactory()
                                .createPoint(new org.locationtech.jts.geom.Coordinate(138.6, -34.9));
                bean.setGeometry(geom);
                assertEquals(geom, bean.getGeometry());
        }

        @Test
        void aggregatedCollectionAddAndGet() throws Exception {
                AllAttributesPersistent bean = AllAttributesPersistent.newInstance();
                assertTrue(bean.getAggregatedCollection().isEmpty());
                AllAttributesPersistent element = new AllAttributesPersistent();
                bean.addAggregatedCollectionElement(element);
                assertEquals(1, bean.getAggregatedCollection().size());
        }

        @Test
        void composedCollectionAddAndGet() throws Exception {
                AllAttributesPersistent bean = AllAttributesPersistent.newInstance();
                assertTrue(bean.getComposedCollection().isEmpty());
                AllAttributesPersistent element = new AllAttributesPersistent();
                bean.addComposedCollectionElement(element);
                assertEquals(1, bean.getComposedCollection().size());
        }

        @Test
        void inverseAggregatedAssociationAddAndGet() throws Exception {
                AllAttributesPersistent bean = AllAttributesPersistent.newInstance();
                assertTrue(bean.getInverseAggregatedAssociation().isEmpty());
                AllAttributesPersistent element = new AllAttributesPersistent();
                bean.addInverseAggregatedAssociationElement(element);
                assertEquals(1, bean.getInverseAggregatedAssociation().size());
                assertNotNull(bean.getInverseAggregatedAssociationElementById(element.getBizId()));
        }

        @Test
        void inverseAggregatedAssociationRemove() throws Exception {
                AllAttributesPersistent bean = AllAttributesPersistent.newInstance();
                AllAttributesPersistent element = new AllAttributesPersistent();
                bean.addInverseAggregatedAssociationElement(element);
                assertTrue(bean.removeInverseAggregatedAssociationElement(element));
                assertTrue(bean.getInverseAggregatedAssociation().isEmpty());
        }

        @Test
        void inverseAggregatedAssociationAddAtIndexAndRemoveByIndex() throws Exception {
                AllAttributesPersistent bean = AllAttributesPersistent.newInstance();
                AllAttributesPersistent element = new AllAttributesPersistent();
                bean.addInverseAggregatedAssociationElement(0, element);
                assertEquals(1, bean.getInverseAggregatedAssociation().size());
                assertNotNull(bean.removeInverseAggregatedAssociationElement(0));
        }

        @Test
        void aggregatedCollectionRemoveAndIndex() throws Exception {
                AllAttributesPersistent bean = AllAttributesPersistent.newInstance();
                AllAttributesPersistent element = new AllAttributesPersistent();
                bean.addAggregatedCollectionElement(element);
                assertNotNull(bean.getAggregatedCollectionElementById(element.getBizId()));
                assertTrue(bean.removeAggregatedCollectionElement(element));
        }

        @Test
        void aggregatedCollectionAddAtIndexAndRemoveByIndex() throws Exception {
                AllAttributesPersistent bean = AllAttributesPersistent.newInstance();
                AllAttributesPersistent element = new AllAttributesPersistent();
                bean.addAggregatedCollectionElement(0, element);
                assertEquals(1, bean.getAggregatedCollection().size());
                assertNotNull(bean.removeAggregatedCollectionElement(0));
                assertTrue(bean.getAggregatedCollection().isEmpty());
        }

        @Test
        void aggregatedCollectionSetById() throws Exception {
                AllAttributesPersistent bean = AllAttributesPersistent.newInstance();
                AllAttributesPersistent element = new AllAttributesPersistent();
                bean.addAggregatedCollectionElement(element);
                AllAttributesPersistent replacement = new AllAttributesPersistent();
                replacement.setBizId(element.getBizId());
                bean.setAggregatedCollectionElementById(element.getBizId(), replacement);
                assertTrue(bean.getAggregatedCollection().contains(replacement));
                assertEquals(1, bean.getAggregatedCollection().size());
        }

        @Test
        void composedCollectionGetById() throws Exception {
                AllAttributesPersistent bean = AllAttributesPersistent.newInstance();
                AllAttributesPersistent element = new AllAttributesPersistent();
                bean.addComposedCollectionElement(element);
                assertNotNull(bean.getComposedCollectionElementById(element.getBizId()));
                assertNull(bean.getComposedCollectionElementById("nonexistent"));
        }

        @Test
        void composedCollectionSetById() throws Exception {
                AllAttributesPersistent bean = AllAttributesPersistent.newInstance();
                AllAttributesPersistent element = new AllAttributesPersistent();
                bean.addComposedCollectionElement(element);
                AllAttributesPersistent replacement = new AllAttributesPersistent();
                replacement.setBizId(element.getBizId());
                bean.setComposedCollectionElementById(element.getBizId(), replacement);
                assertTrue(bean.getComposedCollection().contains(replacement));
                assertEquals(1, bean.getComposedCollection().size());
        }

        @Test
        void composedCollectionAddAtIndexAndRemove() throws Exception {
                AllAttributesPersistent bean = AllAttributesPersistent.newInstance();
                AllAttributesPersistent element = new AllAttributesPersistent();
                bean.addComposedCollectionElement(0, element);
                assertEquals(1, bean.getComposedCollection().size());
                assertTrue(bean.removeComposedCollectionElement(element));
                assertTrue(bean.getComposedCollection().isEmpty());
        }

        @Test
        void composedCollectionRemoveByIndex() throws Exception {
                AllAttributesPersistent bean = AllAttributesPersistent.newInstance();
                AllAttributesPersistent element = new AllAttributesPersistent();
                bean.addComposedCollectionElement(element);
                assertNotNull(bean.removeComposedCollectionElement(0));
                assertTrue(bean.getComposedCollection().isEmpty());
        }
}

