package org.skyve.impl.report.jasperreports;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;

import java.util.List;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.report.jasperreports.ReportDesignParameters.ColumnAlignment;
import org.skyve.impl.report.jasperreports.ReportDesignParameters.ReportColumn;
import org.skyve.impl.report.jasperreports.ReportDesignParameters.ReportStyle;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.report.ReportFormat;

@SuppressWarnings("static-method")
class ReportDesignParametersTest {

	private ReportDesignParameters params;

	@BeforeEach
	void setUp() {
		params = new ReportDesignParameters();
	}

	@Test
	void reportFormatRoundTrip() {
		params.setReportFormat(ReportFormat.pdf);
		assertThat(params.getReportFormat(), is(ReportFormat.pdf));
	}

	@Test
	void reportStyleEnumValues() {
		assertThat(ReportStyle.tabular, notNullValue());
		assertThat(ReportStyle.columnar, notNullValue());
		assertNotEquals(ReportStyle.tabular, ReportStyle.columnar);
	}

	@Test
	void columnAlignmentEnumValues() {
		assertThat(ColumnAlignment.left, notNullValue());
		assertThat(ColumnAlignment.center, notNullValue());
		assertThat(ColumnAlignment.right, notNullValue());
		assertNotEquals(ColumnAlignment.left, ColumnAlignment.center);
	}

	@Test
	void reportStyleRoundTrip() {
		params.setReportStyle(ReportStyle.tabular);
		assertThat(params.getReportStyle(), is(ReportStyle.tabular));
		params.setReportStyle(ReportStyle.columnar);
		assertThat(params.getReportStyle(), is(ReportStyle.columnar));
	}

	@Test
	void columnRoundTrip() {
		ReportColumn col = new ReportColumn();
		col.setName("testName");
		col.setTitle("Test Title");
		col.setLine(2);
		col.setWidth(120);
		col.setAlignment(ColumnAlignment.center);
		col.setAttributeType(AttributeType.text);
		col.setFormatPattern("##.00");

		assertThat(col.getName(), is("testName"));
		assertThat(col.getTitle(), is("Test Title"));
		assertEquals(2, col.getLine());
		assertEquals(120, col.getWidth());
		assertThat(col.getAlignment(), is(ColumnAlignment.center));
		assertThat(col.getAttributeType(), is(AttributeType.text));
		assertThat(col.getFormatPattern(), is("##.00"));
	}

	@Test
	void addAndRetrieveColumns() {
		ReportColumn col1 = new ReportColumn();
		col1.setName("first");
		ReportColumn col2 = new ReportColumn();
		col2.setName("second");

		params.getColumns().add(col1);
		params.getColumns().add(col2);

		List<ReportColumn> cols = params.getColumns();
		assertEquals(2, cols.size());
		assertThat(cols.get(0).getName(), is("first"));
		assertThat(cols.get(1).getName(), is("second"));
	}

	@Test
	void addAndRetrieveGroupColumns() {
		params.getGroupColumns().add("groupA");
		params.getGroupColumns().add("groupB");

		List<String> groups = params.getGroupColumns();
		assertEquals(2, groups.size());
		assertThat(groups.get(0), is("groupA"));
		assertThat(groups.get(1), is("groupB"));
	}

	@Test
	void pageGeometryRoundTrip() {
		params.setPageWidth(842);
		params.setPageHeight(595);
		params.setTopMargin(20);
		params.setBottomMargin(20);
		params.setLeftMargin(20);
		params.setRightMargin(20);

		assertEquals(842, params.getPageWidth());
		assertEquals(595, params.getPageHeight());
		assertEquals(20, params.getTopMargin());
		assertEquals(20, params.getBottomMargin());
		assertEquals(20, params.getLeftMargin());
		assertEquals(20, params.getRightMargin());
	}

	@Test
	void flagsRoundTrip() {
		params.setPretty(true);
		params.setPaginated(true);
		params.setShowSummary(true);
		params.setIncludeCustomerLogo(false);

		assertTrue(params.isPretty());
		assertTrue(params.isPaginated());
		assertTrue(params.isShowSummary());
		assertFalse(params.isIncludeCustomerLogo());
	}
}
