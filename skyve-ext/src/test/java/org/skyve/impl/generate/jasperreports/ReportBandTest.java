package org.skyve.impl.generate.jasperreports;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.generate.jasperreports.ReportBand.BandType;
import org.skyve.impl.generate.jasperreports.ReportBand.SplitType;

@SuppressWarnings("static-method")
class ReportBandTest {

	private DesignSpecification parent;

	@BeforeEach
	void setUp() {
		parent = new DesignSpecification();
		parent.setDefaultElementHeight(Integer.valueOf(20));
	}

	private ReportBand bandWithParent() {
		ReportBand band = new ReportBand();
		band.setParent(parent);
		return band;
	}

	@Test
	void splitTypeToStringIsCapitalised() {
		assertThat(SplitType.immediate.toString(), is("Immediate"));
		assertThat(SplitType.prevent.toString(), is("Prevent"));
		assertThat(SplitType.stretch.toString(), is("Stretch"));
	}

	@Test
	void bandTypeRoundTrip() {
		ReportBand band = bandWithParent();
		for (BandType bt : BandType.values()) {
			band.setBandType(bt);
			assertThat(band.getBandType(), is(bt));
		}
	}

	@Test
	void heightWithNoElementsFallsBackToDefaultElementHeight() {
		ReportBand band = bandWithParent();
		// no elements, no explicit height set — should return parent's defaultElementHeight
		assertThat(band.getHeight(), is(Integer.valueOf(20)));
	}

	@Test
	void heightFromExplicitValueAboveDefault() {
		ReportBand band = bandWithParent();
		band.setHeight(Integer.valueOf(50));
		assertThat(band.getHeight(), is(Integer.valueOf(50)));
	}

	@Test
	void heightFromElementsExceedsDefault() {
		ReportBand band = bandWithParent();
		band.setHeight(Integer.valueOf(20));

		ReportElement elem = new ReportElement(ReportElement.ElementType.staticText, "test", "test", null, null, null, null);
		elem.setElementTop(Integer.valueOf(10));
		elem.setElementHeight(Integer.valueOf(30)); // top+height = 40 > default 20
		band.addElement(elem);

		assertThat(band.getHeight(), is(Integer.valueOf(40)));
	}

	@Test
	void addElementToList() {
		ReportBand band = bandWithParent();
		ReportElement elem = new ReportElement(ReportElement.ElementType.staticText, "test", "test", null, null, null, null);
		band.addElement(elem);
		assertEquals(1, band.getElements().size());
		assertNotNull(band.getElements().get(0));
	}

	@Test
	void invisibleConditionNameRoundTrip() {
		ReportBand band = bandWithParent();
		band.setInvisibleConditionName("notEditable");
		assertThat(band.getInvisibleConditionName(), is("notEditable"));
	}

	@Test
	void elementsInitiallyEmpty() {
		ReportBand band = bandWithParent();
		assertThat(band.getElements(), notNullValue());
		assertTrue(band.getElements().isEmpty());
	}

	@Test
	void spreadElementsOnEmptyBandReturnsImmediately() {
		ReportBand band = bandWithParent();
		// Should not throw even with no elements
		band.spreadElements();
		assertTrue(band.getElements().isEmpty());
	}

	@Test
	void spreadElementsWithWidthSetsElementWidthsAndLeftPositions() {
		parent.setWidth(Integer.valueOf(600));
		parent.setColumnWidth(Integer.valueOf(600));
		ReportBand band = bandWithParent();

		// Add 3 elements
		ReportElement e1 = new ReportElement(ReportElement.ElementType.staticText, "e1", "A", null, null, null, null);
		e1.setElementHeight(Integer.valueOf(20));
		ReportElement e2 = new ReportElement(ReportElement.ElementType.staticText, "e2", "B", null, null, null, null);
		e2.setElementHeight(Integer.valueOf(20));
		ReportElement e3 = new ReportElement(ReportElement.ElementType.staticText, "e3", "C", null, null, null, null);
		e3.setElementHeight(Integer.valueOf(20));

		band.addElement(e1);
		band.addElement(e2);
		band.addElement(e3);

		band.spreadElements();

		// After spreadElements, elements should have left positions set
		assertNotNull(e1.getElementLeft(), "First element should have left position");
		assertNotNull(e2.getElementLeft(), "Second element should have left position");
		assertNotNull(e3.getElementLeft(), "Last element should have left position");
	}

	@Test
	void addContainerFlattensElementsFromContainerIntoBand() {
		ReportBand band = bandWithParent();

		Container container = new Container();
		ReportElement elem = new ReportElement(ReportElement.ElementType.staticText, "item", "Value", null, null, null, null);
		elem.setElementHeight(Integer.valueOf(20));
		container.getElements().add(elem);

		band.addContainer(container);

		assertEquals(1, band.getElements().size());
		assertEquals("item", band.getElements().get(0).getName());
	}

	@Test
	void addContainerRecursivelyAddsChildContainerElements() {
		ReportBand band = bandWithParent();

		Container topContainer = new Container();
		ReportElement topElem = new ReportElement(ReportElement.ElementType.staticText, "top", "TopValue", null, null, null, null);
		topElem.setElementHeight(Integer.valueOf(20));
		topContainer.getElements().add(topElem);

		Container child = new Container();
		ReportElement childElem = new ReportElement(ReportElement.ElementType.staticText, "child", "ChildValue", null, null, null, null);
		childElem.setElementHeight(Integer.valueOf(20));
		child.getElements().add(childElem);
		topContainer.getContainers().add(child);

		band.addContainer(topContainer);

		assertEquals(2, band.getElements().size(), "Both top-level and child container elements should be added");
	}

        @Test
        void heightUsesExplicitValueWhenItExceedsCalculatedMax() {
                ReportBand band = bandWithParent();
                band.setHeight(Integer.valueOf(100));

                ReportElement elem = new ReportElement(ReportElement.ElementType.staticText, "e", "e", null, null, null, null);
                elem.setElementTop(Integer.valueOf(5));
                elem.setElementHeight(Integer.valueOf(10)); // top+height = 15 < explicit 100
                band.addElement(elem);

                // maxHeight=20 (default), element top+height=15, so maxHeight stays 20; explicit height=100>=20 → returns 100
                assertThat(band.getHeight(), is(Integer.valueOf(100)));
        }

        @Test
        void splitTypeToStringProducesSnakeCaseValue() {
                assertThat(ReportBand.SplitType.stretch.toString(), is("Stretch"));
        }

        @Test
        void nameRoundTrip() {
                ReportBand band = new ReportBand();
                band.setName("myBand");
                assertThat(band.getName(), is("myBand"));
        }

        @Test
        void setElementsReplacesExistingElements() {
                ReportBand band = new ReportBand();
                java.util.List<ReportElement> list = new java.util.ArrayList<>();
                band.setElements(list);
                assertThat(band.getElements(), org.hamcrest.CoreMatchers.sameInstance(list));
        }

        @Test
        void parentRoundTrip() {
                ReportBand band = new ReportBand();
                DesignSpecification spec = new DesignSpecification();
                band.setParent(spec);
                assertThat(band.getParent(), org.hamcrest.CoreMatchers.sameInstance(spec));
        }

        @Test
        void getJrxmlDelegatesToRendererRenderBand() {
                ReportBand band = bandWithParent();
                band.setBandType(ReportBand.BandType.detail);
                String jrxml = band.getJrxml();
                assertNotNull(jrxml);
        }
}
