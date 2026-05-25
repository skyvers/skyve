package org.skyve.impl.generate.jasperreports;

import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.when;

import java.util.ArrayList;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
@SuppressWarnings("static-method")
class ViewReportDesignGeneratorTest {

	@Mock
	private ReportViewVisitor mockVisitor;

	@Test
	void defaultConstructorCreatesInstance() {
		assertThat(new ViewReportDesignGenerator(), notNullValue());
	}

	@Test
	void defaultConstructorHasNullVisitor() {
		assertThat(new ViewReportDesignGenerator().getVisitor(), nullValue());
	}

	@Test
	void constructorWithVisitorStoresVisitor() {
		ViewReportDesignGenerator gen = new ViewReportDesignGenerator(mockVisitor);
		assertThat(gen.getVisitor(), notNullValue());
	}

	@Test
	void setVisitorStoresValue() {
		ViewReportDesignGenerator gen = new ViewReportDesignGenerator();
		gen.setVisitor(mockVisitor);
		assertThat(gen.getVisitor(), notNullValue());
	}

	@Test
	void getSubreportGeneratorReturnsViewReportDesignGeneratorInstance() {
		ViewReportDesignGenerator gen = new ViewReportDesignGenerator();
		assertThat(gen.getSubreportGenerator(), instanceOf(ViewReportDesignGenerator.class));
	}

	@Test
	void addBandsWithVisitorAlreadyVisitedUsesDetailBands() {
		DesignSpecification design = new DesignSpecification();
		design.setReportType(DesignSpecification.ReportType.report);
		design.setColumnWidth(java.lang.Integer.valueOf(500));
		design.setDefaultElementHeight(java.lang.Integer.valueOf(20));

		when(mockVisitor.isVisited()).thenReturn(true);
		when(mockVisitor.getDetailBands()).thenReturn(new ArrayList<>());
		when(mockVisitor.getViewTitle()).thenReturn("My Report");

		ViewReportDesignGenerator gen = new ViewReportDesignGenerator(mockVisitor);
		gen.addBands(design);

		assertNotNull(design.getBands());
		assertTrue(design.getBands().size() > 0);
	}

	@Test
	void addBandsForSubreportTypeWithVisitor() {
		DesignSpecification design = new DesignSpecification();
		design.setReportType(DesignSpecification.ReportType.subreport);
		design.setColumnWidth(java.lang.Integer.valueOf(400));
		design.setDefaultElementHeight(java.lang.Integer.valueOf(20));

		when(mockVisitor.isVisited()).thenReturn(true);
		when(mockVisitor.getDetailBands()).thenReturn(new ArrayList<>());

		ViewReportDesignGenerator gen = new ViewReportDesignGenerator(mockVisitor);
		gen.addBands(design);

		assertTrue(design.getBands().size() > 0);
	}

	@Test
	void addBandsWithDetailBandsFromVisitorAddsThemToDesign() {
		DesignSpecification design = new DesignSpecification();
		design.setReportType(DesignSpecification.ReportType.report);
		design.setColumnWidth(java.lang.Integer.valueOf(500));
		design.setDefaultElementHeight(java.lang.Integer.valueOf(20));

		ReportBand detailBand = new ReportBand();
		detailBand.setBandType(ReportBand.BandType.detail);
		detailBand.setParent(design);
		java.util.List<ReportBand> details = new ArrayList<>();
		details.add(detailBand);

		when(mockVisitor.isVisited()).thenReturn(true);
		when(mockVisitor.getDetailBands()).thenReturn(details);
		when(mockVisitor.getViewTitle()).thenReturn("Test");

		ViewReportDesignGenerator gen = new ViewReportDesignGenerator(mockVisitor);
		gen.addBands(design);

		long detailCount = design.getBands().stream()
				.filter(b -> ReportBand.BandType.detail.equals(b.getBandType()))
				.count();
		assertTrue(detailCount >= 1);
	}
}
