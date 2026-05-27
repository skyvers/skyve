package modules.kitchensink.domain;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.assertFalse;

import java.util.List;

import org.junit.Test;
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
public class KitchenSinkDomainTest {

	// ---- Constants ----

	@Test
	public void moduleNameConstant() {
		assertEquals("kitchensink", KitchenSink.MODULE_NAME);
	}

	@Test
	public void documentNameConstant() {
		assertEquals("KitchenSink", KitchenSink.DOCUMENT_NAME);
	}

	@Test
	public void propertyNameConstants() {
		assertNotNull(KitchenSink.booleanFlagPropertyName);
		assertNotNull(KitchenSink.colourPropertyName);
		assertNotNull(KitchenSink.datePropertyName);
		assertNotNull(KitchenSink.textPropertyName);
	}

	// ---- Constructor ----

	@Test
	public void defaultConstructorCreatesInstance() {
		KitchenSink ks = new KitchenSink();
		assertNotNull(ks);
	}

	@Test
	public void getBizModuleReturnsKitchensink() {
		assertEquals("kitchensink", new KitchenSink().getBizModule());
	}

	@Test
	public void getBizDocumentReturnsKitchenSink() {
		assertEquals("KitchenSink", new KitchenSink().getBizDocument());
	}

	// ---- Boolean field ----

	@Test
	public void booleanFlagNullByDefault() {
		assertNull(new KitchenSink().getBooleanFlag());
	}

	@Test
	public void setBooleanFlagStoresValue() {
		KitchenSink ks = new KitchenSink();
		ks.setBooleanFlag(Boolean.TRUE);
		assertEquals(Boolean.TRUE, ks.getBooleanFlag());
	}

	@Test
	public void setBooleanFlagToFalse() {
		KitchenSink ks = new KitchenSink();
		ks.setBooleanFlag(Boolean.FALSE);
		assertEquals(Boolean.FALSE, ks.getBooleanFlag());
	}

	// ---- Colour field ----

	@Test
	public void colourNullByDefault() {
		assertNull(new KitchenSink().getColour());
	}

	@Test
	public void setColourStoresValue() {
		KitchenSink ks = new KitchenSink();
		ks.setColour("#FF0000");
		assertEquals("#FF0000", ks.getColour());
	}

	// ---- Date field ----

	@Test
	public void dateNullByDefault() {
		assertNull(new KitchenSink().getDate());
	}

	@Test
	public void setDateStoresValue() {
		KitchenSink ks = new KitchenSink();
		DateOnly date = new DateOnly();
		ks.setDate(date);
		assertEquals(date, ks.getDate());
	}

	// ---- DateTime field ----

	@Test
	public void dateTimeNullByDefault() {
		assertNull(new KitchenSink().getDateTime());
	}

	@Test
	public void setDateTimeStoresValue() {
		KitchenSink ks = new KitchenSink();
		DateTime dt = new DateTime();
		ks.setDateTime(dt);
		assertEquals(dt, ks.getDateTime());
	}

	// ---- Decimal fields ----

	@Test
	public void decimal10NullByDefault() {
		assertNull(new KitchenSink().getDecimal10());
	}

	@Test
	public void setDecimal10StoresValue() {
		KitchenSink ks = new KitchenSink();
		Decimal10 d = new Decimal10("1.5");
		ks.setDecimal10(d);
		assertEquals(d, ks.getDecimal10());
	}

	@Test
	public void decimal2NullByDefault() {
		assertNull(new KitchenSink().getDecimal2());
	}

	@Test
	public void setDecimal2StoresValue() {
		KitchenSink ks = new KitchenSink();
		Decimal2 d = new Decimal2("2.5");
		ks.setDecimal2(d);
		assertEquals(d, ks.getDecimal2());
	}

	@Test
	public void decimal5NullByDefault() {
		assertNull(new KitchenSink().getDecimal5());
	}

	@Test
	public void setDecimal5StoresValue() {
		KitchenSink ks = new KitchenSink();
		Decimal5 d = new Decimal5("3.5");
		ks.setDecimal5(d);
		assertEquals(d, ks.getDecimal5());
	}

	// ---- Enum fields ----

	@Test
	public void comboNullByDefault() {
		assertNull(new KitchenSink().getCombo());
	}

	@Test
	public void setComboStoresValue() {
		KitchenSink ks = new KitchenSink();
		ks.setCombo(KitchenSink.Combo.one);
		assertEquals(KitchenSink.Combo.one, ks.getCombo());
	}

	@Test
	public void radioNullByDefault() {
		assertNull(new KitchenSink().getRadio());
	}

	@Test
	public void setRadioStoresValue() {
		KitchenSink ks = new KitchenSink();
		ks.setRadio(KitchenSink.Radio.one);
		assertEquals(KitchenSink.Radio.one, ks.getRadio());
	}

	// ---- Integer fields ----

	@Test
	public void normalIntegerNullByDefault() {
		assertNull(new KitchenSink().getNormalInteger());
	}

	@Test
	public void setNormalIntegerStoresValue() {
		KitchenSink ks = new KitchenSink();
		ks.setNormalInteger(Integer.valueOf(42));
		assertEquals(Integer.valueOf(42), ks.getNormalInteger());
	}

	@Test
	public void spinnerNullByDefault() {
		assertNull(new KitchenSink().getSpinner());
	}

	@Test
	public void setSpinnerStoresValue() {
		KitchenSink ks = new KitchenSink();
		ks.setSpinner(Integer.valueOf(10));
		assertEquals(Integer.valueOf(10), ks.getSpinner());
	}

	@Test
	public void sliderNullByDefault() {
		assertNull(new KitchenSink().getSlider());
	}

	@Test
	public void setSliderStoresValue() {
		KitchenSink ks = new KitchenSink();
		ks.setSlider(Integer.valueOf(5));
		assertEquals(Integer.valueOf(5), ks.getSlider());
	}

	// ---- Long field ----

	@Test
	public void longIntegerNullByDefault() {
		assertNull(new KitchenSink().getLongInteger());
	}

	@Test
	public void setLongIntegerStoresValue() {
		KitchenSink ks = new KitchenSink();
		ks.setLongInteger(Long.valueOf(100L));
		assertEquals(Long.valueOf(100L), ks.getLongInteger());
	}

	// ---- String fields ----

	@Test
	public void htmlNullByDefault() {
		assertNull(new KitchenSink().getHtml());
	}

	@Test
	public void setHtmlStoresValue() {
		KitchenSink ks = new KitchenSink();
		ks.setHtml("<p>Hello</p>");
		assertEquals("<p>Hello</p>", ks.getHtml());
	}

	@Test
	public void markupNullByDefault() {
		assertNull(new KitchenSink().getMarkup());
	}

	@Test
	public void setMarkupStoresValue() {
		KitchenSink ks = new KitchenSink();
		ks.setMarkup("# Heading");
		assertEquals("# Heading", ks.getMarkup());
	}

	@Test
	public void memoNullByDefault() {
		assertNull(new KitchenSink().getMemo());
	}

	@Test
	public void setMemoStoresValue() {
		KitchenSink ks = new KitchenSink();
		ks.setMemo("A memo");
		assertEquals("A memo", ks.getMemo());
	}

	@Test
	public void textNullByDefault() {
		assertNull(new KitchenSink().getText());
	}

	@Test
	public void setTextStoresValue() {
		KitchenSink ks = new KitchenSink();
		ks.setText("Hello World");
		assertEquals("Hello World", ks.getText());
	}

	// ---- Time and Timestamp fields ----

	@Test
	public void timeNullByDefault() {
		assertNull(new KitchenSink().getTime());
	}

	@Test
	public void setTimeStoresValue() {
		KitchenSink ks = new KitchenSink();
		TimeOnly t = new TimeOnly();
		ks.setTime(t);
		assertEquals(t, ks.getTime());
	}

	@Test
	public void timestampNullByDefault() {
		assertNull(new KitchenSink().getTimestamp());
	}

	@Test
	public void setTimestampStoresValue() {
		KitchenSink ks = new KitchenSink();
		Timestamp ts = new Timestamp();
		ks.setTimestamp(ts);
		assertEquals(ts, ks.getTimestamp());
	}

	// ---- Content fields ----

	@Test
	public void contentLinkNullByDefault() {
		assertNull(new KitchenSink().getContentLink());
	}

	@Test
	public void setContentLinkStoresValue() {
		KitchenSink ks = new KitchenSink();
		ks.setContentLink("abc-123");
		assertEquals("abc-123", ks.getContentLink());
	}

	@Test
	public void contentImageNullByDefault() {
		assertNull(new KitchenSink().getContentImage());
	}

	@Test
	public void setContentImageStoresValue() {
		KitchenSink ks = new KitchenSink();
		ks.setContentImage("img-456");
		assertEquals("img-456", ks.getContentImage());
	}

	@Test
	public void contentSignatureNullByDefault() {
		assertNull(new KitchenSink().getContentSignature());
	}

	@Test
	public void setContentSignatureStoresValue() {
		KitchenSink ks = new KitchenSink();
		ks.setContentSignature("sig-789");
		assertEquals("sig-789", ks.getContentSignature());
	}

	// ---- ID field ----

	@Test
	public void idNullByDefault() {
		assertNull(new KitchenSink().getId());
	}

	@Test
	public void setIdStoresValue() {
		KitchenSink ks = new KitchenSink();
		ks.setId("my-id");
		assertEquals("my-id", ks.getId());
	}

	// ---- Child collections ----

	@Test
	public void containerGridEmptyByDefault() {
		List<ContainerGrid> cg = new KitchenSink().getContainerGrid();
		assertNotNull(cg);
		assertTrue(cg.isEmpty());
	}

	@Test
	public void addContainerGridElementAddsToList() {
		KitchenSink ks = new KitchenSink();
		ContainerGrid cg = new ContainerGrid();
		boolean result = ks.addContainerGridElement(cg);
		assertTrue(result);
		assertEquals(1, ks.getContainerGrid().size());
	}

	@Test
	public void addContainerGridElementSetsParent() {
		KitchenSink ks = new KitchenSink();
		ContainerGrid cg = new ContainerGrid();
		ks.addContainerGridElement(cg);
		assertEquals(ks, cg.getParent());
	}

	@Test
	public void removeContainerGridElementRemovesFromList() {
		KitchenSink ks = new KitchenSink();
		ContainerGrid cg = new ContainerGrid();
		ks.addContainerGridElement(cg);
		boolean result = ks.removeContainerGridElement(cg);
		assertTrue(result);
		assertTrue(ks.getContainerGrid().isEmpty());
	}

	@Test
	public void removeContainerGridElementClearsParent() {
		KitchenSink ks = new KitchenSink();
		ContainerGrid cg = new ContainerGrid();
		ks.addContainerGridElement(cg);
		ks.removeContainerGridElement(cg);
		assertNull(cg.getParent());
	}

	@Test
	public void addContainerGridElementAtIndexInsertsAtCorrectPosition() {
		KitchenSink ks = new KitchenSink();
		ContainerGrid cg1 = new ContainerGrid();
		ContainerGrid cg2 = new ContainerGrid();
		ks.addContainerGridElement(cg1);
		ks.addContainerGridElement(0, cg2);
		assertEquals(cg2, ks.getContainerGrid().get(0));
	}

	@Test
	public void removeContainerGridByIndexRemovesElement() {
		KitchenSink ks = new KitchenSink();
		ContainerGrid cg = new ContainerGrid();
		ks.addContainerGridElement(cg);
		ContainerGrid removed = ks.removeContainerGridElement(0);
		assertEquals(cg, removed);
		assertTrue(ks.getContainerGrid().isEmpty());
	}

	@Test
	public void getContainerGridElementByIdFindsElement() {
		KitchenSink ks = new KitchenSink();
		ContainerGrid cg = new ContainerGrid();
		ks.addContainerGridElement(cg);
		ContainerGrid found = ks.getContainerGridElementById(cg.getBizId());
		assertEquals(cg, found);
	}

	@Test
	public void setContainerGridElementByIdUpdatesElement() {
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
	public void inlineGridEmptyByDefault() {
		assertTrue(new KitchenSink().getInlineGrid().isEmpty());
	}

	// ---- OrderedGrid child ----

	@Test
	public void orderedGridEmptyByDefault() {
		assertTrue(new KitchenSink().getOrderedGrid().isEmpty());
	}

	@Test
	public void addOrderedGridElementAddsToList() {
		KitchenSink ks = new KitchenSink();
		OrderedGrid og = new OrderedGrid();
		assertTrue(ks.addOrderedGridElement(og));
		assertEquals(1, ks.getOrderedGrid().size());
	}

	@Test
	public void addOrderedGridElementSetsParent() {
		KitchenSink ks = new KitchenSink();
		OrderedGrid og = new OrderedGrid();
		ks.addOrderedGridElement(og);
		assertEquals(ks, og.getParent());
	}

	@Test
	public void removeOrderedGridElementClearsParent() {
		KitchenSink ks = new KitchenSink();
		OrderedGrid og = new OrderedGrid();
		ks.addOrderedGridElement(og);
		ks.removeOrderedGridElement(og);
		assertNull(og.getParent());
	}

	@Test
	public void addOrderedGridAtIndex() {
		KitchenSink ks = new KitchenSink();
		OrderedGrid og1 = new OrderedGrid();
		OrderedGrid og2 = new OrderedGrid();
		ks.addOrderedGridElement(og1);
		ks.addOrderedGridElement(0, og2);
		assertEquals(og2, ks.getOrderedGrid().get(0));
	}

	@Test
	public void removeOrderedGridByIndex() {
		KitchenSink ks = new KitchenSink();
		OrderedGrid og = new OrderedGrid();
		ks.addOrderedGridElement(og);
		OrderedGrid removed = ks.removeOrderedGridElement(0);
		assertEquals(og, removed);
	}

	@Test
	public void getOrderedGridElementByIdFindsElement() {
		KitchenSink ks = new KitchenSink();
		OrderedGrid og = new OrderedGrid();
		ks.addOrderedGridElement(og);
		assertEquals(og, ks.getOrderedGridElementById(og.getBizId()));
	}

	@Test
	public void setOrderedGridElementById() {
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
	public void dataRepeaterEmptyByDefault() {
		assertTrue(new KitchenSink().getDataRepeater().isEmpty());
	}

	@Test
	public void addDataRepeaterElementAddsToList() {
		KitchenSink ks = new KitchenSink();
		DataRepeater dr = new DataRepeater();
		assertTrue(ks.addDataRepeaterElement(dr));
		assertEquals(1, ks.getDataRepeater().size());
	}

	@Test
	public void addDataRepeaterElementSetsParent() {
		KitchenSink ks = new KitchenSink();
		DataRepeater dr = new DataRepeater();
		ks.addDataRepeaterElement(dr);
		assertEquals(ks, dr.getParent());
	}

	@Test
	public void removeDataRepeaterElementClearsParent() {
		KitchenSink ks = new KitchenSink();
		DataRepeater dr = new DataRepeater();
		ks.addDataRepeaterElement(dr);
		ks.removeDataRepeaterElement(dr);
		assertNull(dr.getParent());
	}

	@Test
	public void addDataRepeaterAtIndex() {
		KitchenSink ks = new KitchenSink();
		DataRepeater dr1 = new DataRepeater();
		DataRepeater dr2 = new DataRepeater();
		ks.addDataRepeaterElement(dr1);
		ks.addDataRepeaterElement(0, dr2);
		assertEquals(dr2, ks.getDataRepeater().get(0));
	}

	@Test
	public void removeDataRepeaterByIndex() {
		KitchenSink ks = new KitchenSink();
		DataRepeater dr = new DataRepeater();
		ks.addDataRepeaterElement(dr);
		DataRepeater removed = ks.removeDataRepeaterElement(0);
		assertEquals(dr, removed);
	}

	@Test
	public void getDataRepeaterElementByIdFindsElement() {
		KitchenSink ks = new KitchenSink();
		DataRepeater dr = new DataRepeater();
		ks.addDataRepeaterElement(dr);
		assertEquals(dr, ks.getDataRepeaterElementById(dr.getBizId()));
	}

	@Test
	public void setDataRepeaterElementById() {
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
	public void lookupDescriptionNullByDefault() {
		assertNull(new KitchenSink().getLookupDescription());
	}

	// ---- Combo enum ----

	@Test
	public void comboEnumValues() {
		KitchenSink.Combo[] values = KitchenSink.Combo.values();
		assertTrue(values.length > 0);
	}

	@Test
	public void comboToLocalisedDescription() {
		assertNotNull(KitchenSink.Combo.one.toLocalisedDescription());
	}

	@Test
	public void comboFromLocalisedDescriptionReturnsValue() {
		KitchenSink.Combo c = KitchenSink.Combo.fromLocalisedDescription(KitchenSink.Combo.one.toLocalisedDescription());
		assertEquals(KitchenSink.Combo.one, c);
	}

	@Test
	public void comboToDomainValuesNotEmpty() {
		assertFalse(KitchenSink.Combo.toDomainValues().isEmpty());
	}

        @Test
        public void comboFromCodeReturnsValue() {
                assertEquals(KitchenSink.Combo.one, KitchenSink.Combo.fromCode("one"));
                assertNull(KitchenSink.Combo.fromCode("notexist"));
        }

        @Test
        public void comboFromLocalisedDescriptionReturnsNullForUnknown() {
                assertNull(KitchenSink.Combo.fromLocalisedDescription("notexist"));
        }


        @Test
        public void radioToLocalisedDescription() {
		assertNotNull(KitchenSink.Radio.one.toLocalisedDescription());
	}

	@Test
	public void radioFromLocalisedDescriptionReturnsValue() {
		KitchenSink.Radio r = KitchenSink.Radio.fromLocalisedDescription(KitchenSink.Radio.one.toLocalisedDescription());
		assertEquals(KitchenSink.Radio.one, r);
	}

	@Test
	public void radioToDomainValuesNotEmpty() {
		assertFalse(KitchenSink.Radio.toDomainValues().isEmpty());
	}

        @Test
        public void radioFromCodeReturnsValue() {
                assertEquals(KitchenSink.Radio.one, KitchenSink.Radio.fromCode("one"));
                assertNull(KitchenSink.Radio.fromCode("notexist"));
        }

        @Test
        public void radioFromLocalisedDescriptionReturnsNullForUnknown() {
                assertNull(KitchenSink.Radio.fromLocalisedDescription("notexist"));
        }

        @Test
        public void comboToCodeAndToDomainValue() {
                assertEquals("one", KitchenSink.Combo.one.toCode());
                assertNotNull(KitchenSink.Combo.one.toDomainValue());
                assertEquals("one", KitchenSink.Combo.one.toDomainValue().getCode());
        }

        @Test
        public void radioToCodeAndToDomainValue() {
                assertEquals("one", KitchenSink.Radio.one.toCode());
                assertNotNull(KitchenSink.Radio.one.toDomainValue());
                assertEquals("one", KitchenSink.Radio.one.toDomainValue().getCode());
        }
}

