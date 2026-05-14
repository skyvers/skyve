package org.skyve.impl.generate.jasperreports;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.generate.jasperreports.ReportBand.BandType;
import org.skyve.impl.generate.jasperreports.ReportBand.SplitType;

public class ReportBandTest {

	private DesignSpecification parent;

	@BeforeEach
	public void setUp() {
		parent = new DesignSpecification();
		parent.setDefaultElementHeight(Integer.valueOf(20));
	}

	private ReportBand bandWithParent() {
		ReportBand band = new ReportBand();
		band.setParent(parent);
		return band;
	}

	@Test
	public void splitTypeToStringIsCapitalised() {
		assertThat(SplitType.immediate.toString(), is("Immediate"));
		assertThat(SplitType.prevent.toString(), is("Prevent"));
		assertThat(SplitType.stretch.toString(), is("Stretch"));
	}

	@Test
	public void bandTypeRoundTrip() {
		ReportBand band = bandWithParent();
		for (BandType bt : BandType.values()) {
			band.setBandType(bt);
			assertThat(band.getBandType(), is(bt));
		}
	}

	@Test
	public void heightWithNoElementsFallsBackToDefaultElementHeight() {
		ReportBand band = bandWithParent();
		// no elements, no explicit height set — should return parent's defaultElementHeight
		assertThat(band.getHeight(), is(Integer.valueOf(20)));
	}

	@Test
	public void heightFromExplicitValueAboveDefault() {
		ReportBand band = bandWithParent();
		band.setHeight(Integer.valueOf(50));
		assertThat(band.getHeight(), is(Integer.valueOf(50)));
	}

	@Test
	public void heightFromElementsExceedsDefault() {
		ReportBand band = bandWithParent();
		band.setHeight(Integer.valueOf(20));

		ReportElement elem = new ReportElement(ReportElement.ElementType.staticText, "test", "test", null, null, null, null);
		elem.setElementTop(Integer.valueOf(10));
		elem.setElementHeight(Integer.valueOf(30)); // top+height = 40 > default 20
		band.addElement(elem);

		assertThat(band.getHeight(), is(Integer.valueOf(40)));
	}

	@Test
	public void addElementToList() {
		ReportBand band = bandWithParent();
		ReportElement elem = new ReportElement(ReportElement.ElementType.staticText, "test", "test", null, null, null, null);
		band.addElement(elem);
		assertThat(band.getElements().size(), is(1));
		assertNotNull(band.getElements().get(0));
	}

	@Test
	public void invisibleConditionNameRoundTrip() {
		ReportBand band = bandWithParent();
		band.setInvisibleConditionName("notEditable");
		assertThat(band.getInvisibleConditionName(), is("notEditable"));
	}

	@Test
	public void elementsInitiallyEmpty() {
		ReportBand band = bandWithParent();
		assertThat(band.getElements(), notNullValue());
		assertTrue(band.getElements().isEmpty());
	}
}
