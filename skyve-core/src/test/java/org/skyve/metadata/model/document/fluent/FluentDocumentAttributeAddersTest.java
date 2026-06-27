package org.skyve.metadata.model.document.fluent;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;

import org.junit.jupiter.api.Test;

/**
 * Tests for FluentDocument attribute-adding methods not covered by the primary builders test.
 */
@SuppressWarnings("static-method")
class FluentDocumentAttributeAddersTest {

	@Test
	void addTimeIncreasesAttributeCount() {
		FluentDocument d = new FluentDocument();
		FluentDocument result = d.addTime(new FluentTime().name("startTime"));
		assertSame(d, result);
		assertEquals(1, d.get().getAttributes().size());
	}

	@Test
	void addTimestampIncreasesAttributeCount() {
		FluentDocument d = new FluentDocument();
		FluentDocument result = d.addTimestamp(new FluentTimestamp().name("createdAt"));
		assertSame(d, result);
		assertEquals(1, d.get().getAttributes().size());
	}

	@Test
	void addIntegerIncreasesAttributeCount() {
		FluentDocument d = new FluentDocument();
		FluentDocument result = d.addInteger(new FluentInteger().name("count"));
		assertSame(d, result);
		assertEquals(1, d.get().getAttributes().size());
	}

	@Test
	void addLongIntegerIncreasesAttributeCount() {
		FluentDocument d = new FluentDocument();
		FluentDocument result = d.addLongInteger(new FluentLongInteger().name("bigCount"));
		assertSame(d, result);
		assertEquals(1, d.get().getAttributes().size());
	}

	@Test
	void addDecimal2IncreasesAttributeCount() {
		FluentDocument d = new FluentDocument();
		FluentDocument result = d.addDecimal2(new FluentDecimal2().name("price"));
		assertSame(d, result);
		assertEquals(1, d.get().getAttributes().size());
	}

	@Test
	void addDecimal5IncreasesAttributeCount() {
		FluentDocument d = new FluentDocument();
		FluentDocument result = d.addDecimal5(new FluentDecimal5().name("ratio"));
		assertSame(d, result);
		assertEquals(1, d.get().getAttributes().size());
	}

	@Test
	void addDecimal10IncreasesAttributeCount() {
		FluentDocument d = new FluentDocument();
		FluentDocument result = d.addDecimal10(new FluentDecimal10().name("bigRatio"));
		assertSame(d, result);
		assertEquals(1, d.get().getAttributes().size());
	}

	@Test
	void addEnumerationIncreasesAttributeCount() {
		FluentDocument d = new FluentDocument();
		FluentDocument result = d.addEnumeration(new FluentEnumeration().name("status"));
		assertSame(d, result);
		assertEquals(1, d.get().getAttributes().size());
	}

	@Test
	void addColourIncreasesAttributeCount() {
		FluentDocument d = new FluentDocument();
		FluentDocument result = d.addColour(new FluentColour().name("backgroundColour"));
		assertSame(d, result);
		assertEquals(1, d.get().getAttributes().size());
	}

	@Test
	void addContentIncreasesAttributeCount() {
		FluentDocument d = new FluentDocument();
		FluentDocument result = d.addContent(new FluentContent().name("attachment"));
		assertSame(d, result);
		assertEquals(1, d.get().getAttributes().size());
	}

	@Test
	void addGeometryIncreasesAttributeCount() {
		FluentDocument d = new FluentDocument();
		FluentDocument result = d.addGeometry(new FluentGeometry().name("location"));
		assertSame(d, result);
		assertEquals(1, d.get().getAttributes().size());
	}

	@Test
	void addIdIncreasesAttributeCount() {
		FluentDocument d = new FluentDocument();
		FluentDocument result = d.addId(new FluentId().name("externalId"));
		assertSame(d, result);
		assertEquals(1, d.get().getAttributes().size());
	}

	@Test
	void addAssociationIncreasesAttributeCount() {
		FluentDocument d = new FluentDocument();
		FluentDocument result = d.addAssociation(new FluentAssociation().name("contact"));
		assertSame(d, result);
		assertEquals(1, d.get().getAttributes().size());
	}

	@Test
	void addImplementingInterfaceAndRemoveRoundtrip() {
		FluentDocument d = new FluentDocument();
		d.addImplementingInterface("com.example.Serializable");
		assertEquals(1, d.get().getImplements().size());
		d.removeImplementingInterface("com.example.Serializable");
		assertEquals(0, d.get().getImplements().size());
	}

	@Test
	void addConditionAndClearRoundtrip() {
		FluentDocument d = new FluentDocument();
		d.addCondition(new FluentCondition().name("isActive"));
		assertEquals(1, d.get().getConditions().size());
		FluentDocument cleared = d.clearConditions();
		assertSame(d, cleared);
		assertEquals(0, d.get().getConditions().size());
	}

	@Test
	void removeConditionByName() {
		FluentDocument d = new FluentDocument();
		d.addCondition(new FluentCondition().name("isActive"));
		d.addCondition(new FluentCondition().name("isAdmin"));
		FluentDocument result = d.removeCondition("isActive");
		assertSame(d, result);
		assertEquals(1, d.get().getConditions().size());
	}

	@Test
	void addAttributeAndRemoveRoundtrip() {
		FluentDocument d = new FluentDocument();
		d.addAttribute(new FluentText().name("myField").length(100));
		assertEquals(1, d.get().getAttributes().size());
		FluentDocument removed = d.removeAttribute("myField");
		assertSame(d, removed);
		assertEquals(0, d.get().getAttributes().size());
	}

	@Test
	void clearAttributesEmptiesCollection() {
		FluentDocument d = new FluentDocument();
		d.addText(new FluentText().name("a").length(50));
		d.addBoolean(new FluentBoolean().name("b"));
		FluentDocument cleared = d.clearAttributes();
		assertSame(d, cleared);
		assertEquals(0, d.get().getAttributes().size());
	}

	@Test
	void addUniqueConstraintAndFindRoundtrip() {
		FluentDocument d = new FluentDocument();
		d.addUniqueConstraint(new FluentDocumentUniqueConstraint().name("UC_name"));
		assertNotNull(d.findUniqueConstraint("UC_name"));
	}

	@Test
	void removeUniqueConstraintByName() {
		FluentDocument d = new FluentDocument();
		d.addUniqueConstraint(new FluentDocumentUniqueConstraint().name("UC_a"));
		d.addUniqueConstraint(new FluentDocumentUniqueConstraint().name("UC_b"));
		FluentDocument result = d.removeUniqueConstraint("UC_a");
		assertSame(d, result);
		assertEquals(1, d.get().getUniqueConstraints().size());
	}

	@Test
	void clearUniqueConstraintsEmptiesCollection() {
		FluentDocument d = new FluentDocument();
		d.addUniqueConstraint(new FluentDocumentUniqueConstraint().name("UC_a"));
		FluentDocument cleared = d.clearUniqueConstraint();
		assertSame(d, cleared);
		assertEquals(0, d.get().getUniqueConstraints().size());
	}
}
