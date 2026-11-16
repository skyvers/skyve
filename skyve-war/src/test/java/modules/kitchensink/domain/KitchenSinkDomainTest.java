package modules.kitchensink.domain;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertFalse;

import java.util.List;

import org.junit.jupiter.api.Test;
import org.skyve.domain.types.DateOnly;
import org.skyve.domain.types.DateTime;
import org.skyve.domain.types.Decimal10;
import org.skyve.domain.types.Decimal2;
import org.skyve.domain.types.Decimal5;
import org.skyve.domain.types.TimeOnly;
import org.skyve.domain.types.Timestamp;

/**
 * Tests for the KitchenSink transient domain bean and its inner child beans.
 * These are transient beans (no H2/CORE required).
 */
@SuppressWarnings("static-method")
class KitchenSinkDomainTest {

	// ---- Constants ----

	@Test
	void moduleNameConstant() {
		assertEquals("kitchensink", KitchenSink.MODULE_NAME);
	}

	@Test
	void documentNameConstant() {
		assertEquals("KitchenSink", KitchenSink.DOCUMENT_NAME);
	}

	@Test
	void propertyNameConstants() {
		assertNotNull(KitchenSink.booleanFlagPropertyName);
		assertNotNull(KitchenSink.colourPropertyName);
		assertNotNull(KitchenSink.datePropertyName);
		assertNotNull(KitchenSink.textPropertyName);
	}

	// ---- Constructor ----

	@Test
	void defaultConstructorCreatesInstance() {
		KitchenSink ks = new KitchenSink();
		assertNotNull(ks);
	}

	@Test
	void getBizModuleReturnsKitchensink() {
		assertEquals("kitchensink", new KitchenSink().getBizModule());
	}

	@Test
	void getBizDocumentReturnsKitchenSink() {
		assertEquals("KitchenSink", new KitchenSink().getBizDocument());
	}

	// ---- Boolean field ----

	@Test
	void booleanFlagNullByDefault() {
		assertNull(new KitchenSink().getBooleanFlag());
	}

	@Test
	void setBooleanFlagStoresValue() {
		KitchenSink ks = new KitchenSink();
		ks.setBooleanFlag(Boolean.TRUE);
		assertEquals(Boolean.TRUE, ks.getBooleanFlag());
	}

	@Test
	void setBooleanFlagToFalse() {
		KitchenSink ks = new KitchenSink();
		ks.setBooleanFlag(Boolean.FALSE);
		assertEquals(Boolean.FALSE, ks.getBooleanFlag());
	}

	// ---- Colour field ----

	@Test
	void colourNullByDefault() {
		assertNull(new KitchenSink().getColour());
	}

	@Test
	void setColourStoresValue() {
		KitchenSink ks = new KitchenSink();
		ks.setColour("#FF0000");
		assertEquals("#FF0000", ks.getColour());
	}

	// ---- Date field ----

	@Test
	void dateNullByDefault() {
		assertNull(new KitchenSink().getDate());
	}

	@Test
	void setDateStoresValue() {
		KitchenSink ks = new KitchenSink();
		DateOnly date = new DateOnly();
		ks.setDate(date);
		assertEquals(date, ks.getDate());
	}

	// ---- DateTime field ----

	@Test
	void dateTimeNullByDefault() {
		assertNull(new KitchenSink().getDateTime());
	}

	@Test
	void setDateTimeStoresValue() {
		KitchenSink ks = new KitchenSink();
		DateTime dt = new DateTime();
		ks.setDateTime(dt);
		assertEquals(dt, ks.getDateTime());
	}

	// ---- Decimal fields ----

	@Test
	void decimal10NullByDefault() {
		assertNull(new KitchenSink().getDecimal10());
	}

	@Test
	void setDecimal10StoresValue() {
		KitchenSink ks = new KitchenSink();
		Decimal10 d = new Decimal10("1.5");
		ks.setDecimal10(d);
		assertEquals(d, ks.getDecimal10());
	}

	@Test
	void decimal2NullByDefault() {
		assertNull(new KitchenSink().getDecimal2());
	}

	@Test
	void setDecimal2StoresValue() {
		KitchenSink ks = new KitchenSink();
		Decimal2 d = new Decimal2("2.5");
		ks.setDecimal2(d);
		assertEquals(d, ks.getDecimal2());
	}

	@Test
	void decimal5NullByDefault() {
		assertNull(new KitchenSink().getDecimal5());
	}

	@Test
	void setDecimal5StoresValue() {
		KitchenSink ks = new KitchenSink();
		Decimal5 d = new Decimal5("3.5");
		ks.setDecimal5(d);
		assertEquals(d, ks.getDecimal5());
	}

	// ---- Enum fields ----

	@Test
	void comboNullByDefault() {
		assertNull(new KitchenSink().getCombo());
	}

	@Test
	void setComboStoresValue() {
		KitchenSink ks = new KitchenSink();
		ks.setCombo(KitchenSink.Combo.one);
		assertEquals(KitchenSink.Combo.one, ks.getCombo());
	}

	@Test
	void radioNullByDefault() {
		assertNull(new KitchenSink().getRadio());
	}

	@Test
	void setRadioStoresValue() {
		KitchenSink ks = new KitchenSink();
		ks.setRadio(KitchenSink.Radio.one);
		assertEquals(KitchenSink.Radio.one, ks.getRadio());
	}

	// ---- Integer fields ----

	@Test
	void normalIntegerNullByDefault() {
		assertNull(new KitchenSink().getNormalInteger());
	}

	@Test
	void setNormalIntegerStoresValue() {
		KitchenSink ks = new KitchenSink();
		ks.setNormalInteger(Integer.valueOf(42));
		assertEquals(Integer.valueOf(42), ks.getNormalInteger());
	}

	@Test
	void spinnerNullByDefault() {
		assertNull(new KitchenSink().getSpinner());
	}

	@Test
	void setSpinnerStoresValue() {
		KitchenSink ks = new KitchenSink();
		ks.setSpinner(Integer.valueOf(10));
		assertEquals(Integer.valueOf(10), ks.getSpinner());
	}

	@Test
	void sliderNullByDefault() {
		assertNull(new KitchenSink().getSlider());
	}

	@Test
	void setSliderStoresValue() {
		KitchenSink ks = new KitchenSink();
		ks.setSlider(Integer.valueOf(5));
		assertEquals(Integer.valueOf(5), ks.getSlider());
	}

	// ---- Long field ----

	@Test
	void longIntegerNullByDefault() {
		assertNull(new KitchenSink().getLongInteger());
	}

	@Test
	void setLongIntegerStoresValue() {
		KitchenSink ks = new KitchenSink();
		ks.setLongInteger(Long.valueOf(100L));
		assertEquals(Long.valueOf(100L), ks.getLongInteger());
	}

	// ---- String fields ----

	@Test
	void htmlNullByDefault() {
		assertNull(new KitchenSink().getHtml());
	}

	@Test
	void setHtmlStoresValue() {
		KitchenSink ks = new KitchenSink();
		ks.setHtml("<p>Hello</p>");
		assertEquals("<p>Hello</p>", ks.getHtml());
	}

	@Test
	void markupNullByDefault() {
		assertNull(new KitchenSink().getMarkup());
	}

	@Test
	void setMarkupStoresValue() {
		KitchenSink ks = new KitchenSink();
		ks.setMarkup("# Heading");
		assertEquals("# Heading", ks.getMarkup());
	}

	@Test
	void memoNullByDefault() {
		assertNull(new KitchenSink().getMemo());
	}

	@Test
	void setMemoStoresValue() {
		KitchenSink ks = new KitchenSink();
		ks.setMemo("A memo");
		assertEquals("A memo", ks.getMemo());
	}

	@Test
	void textNullByDefault() {
		assertNull(new KitchenSink().getText());
	}

	@Test
	void setTextStoresValue() {
		KitchenSink ks = new KitchenSink();
		ks.setText("Hello World");
		assertEquals("Hello World", ks.getText());
	}

	// ---- Time and Timestamp fields ----

	@Test
	void timeNullByDefault() {
		assertNull(new KitchenSink().getTime());
	}

	@Test
	void setTimeStoresValue() {
		KitchenSink ks = new KitchenSink();
		TimeOnly t = new TimeOnly();
		ks.setTime(t);
		assertEquals(t, ks.getTime());
	}

	@Test
	void timestampNullByDefault() {
		assertNull(new KitchenSink().getTimestamp());
	}

	@Test
	void setTimestampStoresValue() {
		KitchenSink ks = new KitchenSink();
		Timestamp ts = new Timestamp();
		ks.setTimestamp(ts);
		assertEquals(ts, ks.getTimestamp());
	}

	// ---- Content fields ----

	@Test
	void contentLinkNullByDefault() {
		assertNull(new KitchenSink().getContentLink());
	}

	@Test
	void setContentLinkStoresValue() {
		KitchenSink ks = new KitchenSink();
		ks.setContentLink("abc-123");
		assertEquals("abc-123", ks.getContentLink());
	}

	@Test
	void contentImageNullByDefault() {
		assertNull(new KitchenSink().getContentImage());
	}

	@Test
	void setContentImageStoresValue() {
		KitchenSink ks = new KitchenSink();
		ks.setContentImage("img-456");
		assertEquals("img-456", ks.getContentImage());
	}

	@Test
	void contentSignatureNullByDefault() {
		assertNull(new KitchenSink().getContentSignature());
	}

	@Test
	void setContentSignatureStoresValue() {
		KitchenSink ks = new KitchenSink();
		ks.setContentSignature("sig-789");
		assertEquals("sig-789", ks.getContentSignature());
	}

	// ---- ID field ----

	@Test
	void idNullByDefault() {
		assertNull(new KitchenSink().getId());
	}

	@Test
	void setIdStoresValue() {
		KitchenSink ks = new KitchenSink();
		ks.setId("my-id");
		assertEquals("my-id", ks.getId());
	}

	// ---- Child collections ----

	@Test
	void containerGridEmptyByDefault() {
		List<ContainerGrid> cg = new KitchenSink().getContainerGrid();
		assertNotNull(cg);
		assertTrue(cg.isEmpty());
	}

	@Test
	void addContainerGridElementAddsToList() {
		KitchenSink ks = new KitchenSink();
		ContainerGrid cg = new ContainerGrid();
		boolean result = ks.addContainerGridElement(cg);
		assertTrue(result);
		assertEquals(1, ks.getContainerGrid().size());
	}

	@Test
	void addContainerGridElementSetsParent() {
		KitchenSink ks = new KitchenSink();
		ContainerGrid cg = new ContainerGrid();
		ks.addContainerGridElement(cg);
		assertEquals(ks, cg.getParent());
	}

	@Test
	void removeContainerGridElementRemovesFromList() {
		KitchenSink ks = new KitchenSink();
		ContainerGrid cg = new ContainerGrid();
		ks.addContainerGridElement(cg);
		boolean result = ks.removeContainerGridElement(cg);
		assertTrue(result);
		assertTrue(ks.getContainerGrid().isEmpty());
	}

	@Test
	void removeContainerGridElementClearsParent() {
		KitchenSink ks = new KitchenSink();
		ContainerGrid cg = new ContainerGrid();
		ks.addContainerGridElement(cg);
		ks.removeContainerGridElement(cg);
		assertNull(cg.getParent());
	}

	@Test
	void addContainerGridElementAtIndexInsertsAtCorrectPosition() {
		KitchenSink ks = new KitchenSink();
		ContainerGrid cg1 = new ContainerGrid();
		ContainerGrid cg2 = new ContainerGrid();
		ks.addContainerGridElement(cg1);
		ks.addContainerGridElement(0, cg2);
		assertEquals(cg2, ks.getContainerGrid().get(0));
	}

	@Test
	void removeContainerGridByIndexRemovesElement() {
		KitchenSink ks = new KitchenSink();
		ContainerGrid cg = new ContainerGrid();
		ks.addContainerGridElement(cg);
		ContainerGrid removed = ks.removeContainerGridElement(0);
		assertEquals(cg, removed);
		assertTrue(ks.getContainerGrid().isEmpty());
	}

	@Test
	void getContainerGridElementByIdFindsElement() {
		KitchenSink ks = new KitchenSink();
		ContainerGrid cg = new ContainerGrid();
		ks.addContainerGridElement(cg);
		ContainerGrid found = ks.getContainerGridElementById(cg.getBizId());
		assertEquals(cg, found);
	}

	@Test
	void setContainerGridElementByIdUpdatesElement() {
		KitchenSink ks = new KitchenSink();
		ContainerGrid cg1 = new ContainerGrid();
		ContainerGrid cg2 = new ContainerGrid();
		cg2.setBizId(cg1.getBizId());
		ks.addContainerGridElement(cg1);
		ks.setContainerGridElementById(cg1.getBizId(), cg2);
		assertEquals(cg2, ks.getContainerGrid().get(0));
	}

	// ---- InlineGrid collection field (list accessible, but child bean needs CORE to instantiate) ----

	@Test
	void inlineGridEmptyByDefault() {
		assertTrue(new KitchenSink().getInlineGrid().isEmpty());
	}

	// ---- OrderedGrid child ----

	@Test
	void orderedGridEmptyByDefault() {
		assertTrue(new KitchenSink().getOrderedGrid().isEmpty());
	}

	@Test
	void addOrderedGridElementAddsToList() {
		KitchenSink ks = new KitchenSink();
		OrderedGrid og = new OrderedGrid();
		assertTrue(ks.addOrderedGridElement(og));
		assertEquals(1, ks.getOrderedGrid().size());
	}

	@Test
	void addOrderedGridElementSetsParent() {
		KitchenSink ks = new KitchenSink();
		OrderedGrid og = new OrderedGrid();
		ks.addOrderedGridElement(og);
		assertEquals(ks, og.getParent());
	}

	@Test
	void removeOrderedGridElementClearsParent() {
		KitchenSink ks = new KitchenSink();
		OrderedGrid og = new OrderedGrid();
		ks.addOrderedGridElement(og);
		ks.removeOrderedGridElement(og);
		assertNull(og.getParent());
	}

	@Test
	void addOrderedGridAtIndex() {
		KitchenSink ks = new KitchenSink();
		OrderedGrid og1 = new OrderedGrid();
		OrderedGrid og2 = new OrderedGrid();
		ks.addOrderedGridElement(og1);
		ks.addOrderedGridElement(0, og2);
		assertEquals(og2, ks.getOrderedGrid().get(0));
	}

	@Test
	void removeOrderedGridByIndex() {
		KitchenSink ks = new KitchenSink();
		OrderedGrid og = new OrderedGrid();
		ks.addOrderedGridElement(og);
		OrderedGrid removed = ks.removeOrderedGridElement(0);
		assertEquals(og, removed);
	}

	@Test
	void getOrderedGridElementByIdFindsElement() {
		KitchenSink ks = new KitchenSink();
		OrderedGrid og = new OrderedGrid();
		ks.addOrderedGridElement(og);
		assertEquals(og, ks.getOrderedGridElementById(og.getBizId()));
	}

	@Test
	void setOrderedGridElementById() {
		KitchenSink ks = new KitchenSink();
		OrderedGrid og1 = new OrderedGrid();
		OrderedGrid og2 = new OrderedGrid();
		og2.setBizId(og1.getBizId());
		ks.addOrderedGridElement(og1);
		ks.setOrderedGridElementById(og1.getBizId(), og2);
		assertEquals(og2, ks.getOrderedGrid().get(0));
	}

	// ---- DataRepeater child ----

	@Test
	void dataRepeaterEmptyByDefault() {
		assertTrue(new KitchenSink().getDataRepeater().isEmpty());
	}

	@Test
	void addDataRepeaterElementAddsToList() {
		KitchenSink ks = new KitchenSink();
		DataRepeater dr = new DataRepeater();
		assertTrue(ks.addDataRepeaterElement(dr));
		assertEquals(1, ks.getDataRepeater().size());
	}

	@Test
	void addDataRepeaterElementSetsParent() {
		KitchenSink ks = new KitchenSink();
		DataRepeater dr = new DataRepeater();
		ks.addDataRepeaterElement(dr);
		assertEquals(ks, dr.getParent());
	}

	@Test
	void removeDataRepeaterElementClearsParent() {
		KitchenSink ks = new KitchenSink();
		DataRepeater dr = new DataRepeater();
		ks.addDataRepeaterElement(dr);
		ks.removeDataRepeaterElement(dr);
		assertNull(dr.getParent());
	}

	@Test
	void addDataRepeaterAtIndex() {
		KitchenSink ks = new KitchenSink();
		DataRepeater dr1 = new DataRepeater();
		DataRepeater dr2 = new DataRepeater();
		ks.addDataRepeaterElement(dr1);
		ks.addDataRepeaterElement(0, dr2);
		assertEquals(dr2, ks.getDataRepeater().get(0));
	}

	@Test
	void removeDataRepeaterByIndex() {
		KitchenSink ks = new KitchenSink();
		DataRepeater dr = new DataRepeater();
		ks.addDataRepeaterElement(dr);
		DataRepeater removed = ks.removeDataRepeaterElement(0);
		assertEquals(dr, removed);
	}

	@Test
	void getDataRepeaterElementByIdFindsElement() {
		KitchenSink ks = new KitchenSink();
		DataRepeater dr = new DataRepeater();
		ks.addDataRepeaterElement(dr);
		assertEquals(dr, ks.getDataRepeaterElementById(dr.getBizId()));
	}

	@Test
	void setDataRepeaterElementById() {
		KitchenSink ks = new KitchenSink();
		DataRepeater dr1 = new DataRepeater();
		DataRepeater dr2 = new DataRepeater();
		dr2.setBizId(dr1.getBizId());
		ks.addDataRepeaterElement(dr1);
		ks.setDataRepeaterElementById(dr1.getBizId(), dr2);
		assertEquals(dr2, ks.getDataRepeater().get(0));
	}

	// ---- LookupDescription ----

	@Test
	void lookupDescriptionNullByDefault() {
		assertNull(new KitchenSink().getLookupDescription());
	}

	// ---- Combo enum ----

	@Test
	void comboEnumValues() {
		KitchenSink.Combo[] values = KitchenSink.Combo.values();
		assertTrue(values.length > 0);
	}

	@Test
	void comboToLocalisedDescription() {
		assertNotNull(KitchenSink.Combo.one.toLocalisedDescription());
	}

	@Test
	void comboFromLocalisedDescriptionReturnsValue() {
		KitchenSink.Combo c = KitchenSink.Combo.fromLocalisedDescription(KitchenSink.Combo.one.toLocalisedDescription());
		assertEquals(KitchenSink.Combo.one, c);
	}

	@Test
	void comboToDomainValuesNotEmpty() {
		assertFalse(KitchenSink.Combo.toDomainValues().isEmpty());
	}

        @Test
        void comboFromCodeReturnsValue() {
                assertEquals(KitchenSink.Combo.one, KitchenSink.Combo.fromCode("one"));
                assertNull(KitchenSink.Combo.fromCode("notexist"));
        }

        @Test
        void comboFromLocalisedDescriptionReturnsNullForUnknown() {
                assertNull(KitchenSink.Combo.fromLocalisedDescription("notexist"));
        }


        @Test
        void radioToLocalisedDescription() {
		assertNotNull(KitchenSink.Radio.one.toLocalisedDescription());
	}

	@Test
	void radioFromLocalisedDescriptionReturnsValue() {
		KitchenSink.Radio r = KitchenSink.Radio.fromLocalisedDescription(KitchenSink.Radio.one.toLocalisedDescription());
		assertEquals(KitchenSink.Radio.one, r);
	}

	@Test
	void radioToDomainValuesNotEmpty() {
		assertFalse(KitchenSink.Radio.toDomainValues().isEmpty());
	}

        @Test
        void radioFromCodeReturnsValue() {
                assertEquals(KitchenSink.Radio.one, KitchenSink.Radio.fromCode("one"));
                assertNull(KitchenSink.Radio.fromCode("notexist"));
        }

        @Test
        void radioFromLocalisedDescriptionReturnsNullForUnknown() {
                assertNull(KitchenSink.Radio.fromLocalisedDescription("notexist"));
        }

        @Test
        void comboToCodeAndToDomainValue() {
                assertEquals("one", KitchenSink.Combo.one.toCode());
                assertNotNull(KitchenSink.Combo.one.toDomainValue());
                assertEquals("one", KitchenSink.Combo.one.toDomainValue().getCode());
        }

        @Test
        void radioToCodeAndToDomainValue() {
                assertEquals("one", KitchenSink.Radio.one.toCode());
                assertNotNull(KitchenSink.Radio.one.toDomainValue());
                assertEquals("one", KitchenSink.Radio.one.toDomainValue().getCode());
        }
}

