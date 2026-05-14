package org.skyve.impl.report.jasperreports;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import java.util.List;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.report.jasperreports.ReportDesignParameters.ColumnAlignment;
import org.skyve.impl.report.jasperreports.ReportDesignParameters.ReportColumn;
import org.skyve.impl.report.jasperreports.ReportDesignParameters.ReportStyle;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.report.ReportFormat;

public class ReportDesignParametersTest {

	private ReportDesignParameters params;

	@BeforeEach
	public void setUp() {
		params = new ReportDesignParameters();
	}

	@Test
	public void reportFormatRoundTrip() {
		params.setReportFormat(ReportFormat.pdf);
		assertThat(params.getReportFormat(), is(ReportFormat.pdf));
	}

	@Test
	public void reportStyleEnumValues() {
		assertThat(ReportStyle.tabular, notNullValue());
		assertThat(ReportStyle.columnar, notNullValue());
		assertThat(ReportStyle.tabular.equals(ReportStyle.columnar), is(false));
	}

	@Test
	public void columnAlignmentEnumValues() {
		assertThat(ColumnAlignment.left, notNullValue());
		assertThat(ColumnAlignment.center, notNullValue());
		assertThat(ColumnAlignment.right, notNullValue());
		assertThat(ColumnAlignment.left.equals(ColumnAlignment.center), is(false));
	}

	@Test
	public void reportStyleRoundTrip() {
		params.setReportStyle(ReportStyle.tabular);
		assertThat(params.getReportStyle(), is(ReportStyle.tabular));
		params.setReportStyle(ReportStyle.columnar);
		assertThat(params.getReportStyle(), is(ReportStyle.columnar));
	}

	@Test
	public void columnRoundTrip() {
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
		assertThat(col.getLine(), is(2));
		assertThat(col.getWidth(), is(120));
		assertThat(col.getAlignment(), is(ColumnAlignment.center));
		assertThat(col.getAttributeType(), is(AttributeType.text));
		assertThat(col.getFormatPattern(), is("##.00"));
	}

	@Test
	public void addAndRetrieveColumns() {
		ReportColumn col1 = new ReportColumn();
		col1.setName("first");
		ReportColumn col2 = new ReportColumn();
		col2.setName("second");

		params.getColumns().add(col1);
		params.getColumns().add(col2);

		List<ReportColumn> cols = params.getColumns();
		assertThat(cols.size(), is(2));
		assertThat(cols.get(0).getName(), is("first"));
		assertThat(cols.get(1).getName(), is("second"));
	}

	@Test
	public void addAndRetrieveGroupColumns() {
		params.getGroupColumns().add("groupA");
		params.getGroupColumns().add("groupB");

		List<String> groups = params.getGroupColumns();
		assertThat(groups.size(), is(2));
		assertThat(groups.get(0), is("groupA"));
		assertThat(groups.get(1), is("groupB"));
	}

	@Test
	public void pageGeometryRoundTrip() {
		params.setPageWidth(842);
		params.setPageHeight(595);
		params.setTopMargin(20);
		params.setBottomMargin(20);
		params.setLeftMargin(20);
		params.setRightMargin(20);

		assertThat(params.getPageWidth(), is(842));
		assertThat(params.getPageHeight(), is(595));
		assertThat(params.getTopMargin(), is(20));
		assertThat(params.getBottomMargin(), is(20));
		assertThat(params.getLeftMargin(), is(20));
		assertThat(params.getRightMargin(), is(20));
	}

	@Test
	public void flagsRoundTrip() {
		params.setPretty(true);
		params.setPaginated(true);
		params.setShowSummary(true);
		params.setIncludeCustomerLogo(false);

		assertThat(params.isPretty(), is(true));
		assertThat(params.isPaginated(), is(true));
		assertThat(params.isShowSummary(), is(true));
		assertThat(params.isIncludeCustomerLogo(), is(false));
	}
}
