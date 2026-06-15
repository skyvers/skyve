package org.skyve.impl.metadata.controller;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.view.HorizontalAlignment;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.report.ReportFormat;

class NoCustomisationsTest {

	@Test
	@SuppressWarnings("static-method")
	void determineDefaultColumnTextAlignmentForDateIsRight() {
		NoCustomisations nc = new NoCustomisations();
		assertThat(nc.determineDefaultColumnTextAlignment("desktop", AttributeType.date), is(HorizontalAlignment.right));
	}

	@Test
	@SuppressWarnings("static-method")
	void determineDefaultColumnTextAlignmentForDateTimeIsRight() {
		NoCustomisations nc = new NoCustomisations();
		assertThat(nc.determineDefaultColumnTextAlignment("desktop", AttributeType.dateTime), is(HorizontalAlignment.right));
	}

	@Test
	@SuppressWarnings("static-method")
	void determineDefaultColumnTextAlignmentForTimeIsRight() {
		NoCustomisations nc = new NoCustomisations();
		assertThat(nc.determineDefaultColumnTextAlignment("desktop", AttributeType.time), is(HorizontalAlignment.right));
	}

	@Test
	@SuppressWarnings("static-method")
	void determineDefaultColumnTextAlignmentForDecimal2IsRight() {
		NoCustomisations nc = new NoCustomisations();
		assertThat(nc.determineDefaultColumnTextAlignment("desktop", AttributeType.decimal2), is(HorizontalAlignment.right));
	}

	@Test
	@SuppressWarnings("static-method")
	void determineDefaultColumnTextAlignmentForIntegerIsRight() {
		NoCustomisations nc = new NoCustomisations();
		assertThat(nc.determineDefaultColumnTextAlignment("desktop", AttributeType.integer), is(HorizontalAlignment.right));
	}

	@Test
	@SuppressWarnings("static-method")
	void determineDefaultColumnTextAlignmentForBoolIsCentre() {
		NoCustomisations nc = new NoCustomisations();
		assertThat(nc.determineDefaultColumnTextAlignment("desktop", AttributeType.bool), is(HorizontalAlignment.centre));
	}

	@Test
	@SuppressWarnings("static-method")
	void determineDefaultColumnTextAlignmentForContentIsCentre() {
		NoCustomisations nc = new NoCustomisations();
		assertThat(nc.determineDefaultColumnTextAlignment("desktop", AttributeType.content), is(HorizontalAlignment.centre));
	}

	@Test
	@SuppressWarnings("static-method")
	void determineDefaultColumnTextAlignmentForTextIsLeft() {
		NoCustomisations nc = new NoCustomisations();
		assertThat(nc.determineDefaultColumnTextAlignment("desktop", AttributeType.text), is(HorizontalAlignment.left));
	}

	@Test
	@SuppressWarnings("static-method")
	void determineDefaultWidgetTextAlignmentDelegatesToColumn() {
		NoCustomisations nc = new NoCustomisations();
		// Widget text alignment should match column text alignment (delegates)
		assertThat(nc.determineDefaultWidgetTextAlignment("desktop", AttributeType.date),
				is(nc.determineDefaultColumnTextAlignment("desktop", AttributeType.date)));
	}

	@Test
	@SuppressWarnings("static-method")
	void determineDefaultColumnWidthForDateReturns110() {
		NoCustomisations nc = new NoCustomisations();
		assertThat(nc.determineDefaultColumnWidth("desktop", AttributeType.date), is(Integer.valueOf(110)));
	}

	@Test
	@SuppressWarnings("static-method")
	void determineDefaultColumnWidthForDateTimeReturns130() {
		NoCustomisations nc = new NoCustomisations();
		assertThat(nc.determineDefaultColumnWidth("desktop", AttributeType.dateTime), is(Integer.valueOf(130)));
	}

	@Test
	@SuppressWarnings("static-method")
	void determineDefaultColumnWidthForTimeReturns80() {
		NoCustomisations nc = new NoCustomisations();
		assertThat(nc.determineDefaultColumnWidth("desktop", AttributeType.time), is(Integer.valueOf(80)));
	}

	@Test
	@SuppressWarnings("static-method")
	void determineDefaultColumnWidthForTimestampReturns140() {
		NoCustomisations nc = new NoCustomisations();
		assertThat(nc.determineDefaultColumnWidth("desktop", AttributeType.timestamp), is(Integer.valueOf(140)));
	}

	@Test
	@SuppressWarnings("static-method")
	void determineDefaultColumnWidthForBoolReturns75() {
		NoCustomisations nc = new NoCustomisations();
		assertThat(nc.determineDefaultColumnWidth("desktop", AttributeType.bool), is(Integer.valueOf(75)));
	}

	@Test
	@SuppressWarnings("static-method")
	void determineDefaultColumnWidthForTextReturnsNull() {
		NoCustomisations nc = new NoCustomisations();
		assertNull(nc.determineDefaultColumnWidth("desktop", AttributeType.text));
	}

	@Test
	@SuppressWarnings("static-method")
	void listGridExportFormatsReturnsAllFormats() {
		NoCustomisations nc = new NoCustomisations();
		ReportFormat[] formats = nc.listGridExportFormats();
		assertNotNull(formats);
		assertEquals(ReportFormat.values().length, formats.length);
	}

	@Test
	@SuppressWarnings("static-method")
	void registerCustomExpressionsDoesNotThrow() {
		// just verify it completes without exception
		assertDoesNotThrow(() -> new NoCustomisations().registerCustomExpressions());
	}

	@Test
	@SuppressWarnings("static-method")
	void registerCustomFormattersDoesNotThrow() {
		// just verify it completes without exception
		assertDoesNotThrow(() -> new NoCustomisations().registerCustomFormatters());
	}
}
