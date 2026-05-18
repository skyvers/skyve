package org.skyve.impl.generate.jasperreports;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.domain.types.Decimal2;
import org.skyve.domain.types.Decimal5;
import org.skyve.impl.generate.jasperreports.DesignSpecification.DefinitionSource;
import org.skyve.impl.generate.jasperreports.DesignSpecification.Mode;
import org.skyve.impl.generate.jasperreports.DesignSpecification.Orientation;
import org.skyve.impl.generate.jasperreports.DesignSpecification.ReportType;

@SuppressWarnings({"static-method", "boxing"})
class DesignSpecificationTest {

	private DesignSpecification spec;

	@BeforeEach
	void setUp() {
		spec = new DesignSpecification();
	}

	@Test
	void modeEnumHasSqlAndBean() {
		assertThat(Mode.sql, notNullValue());
		assertThat(Mode.bean, notNullValue());
		assertFalse(Mode.sql.equals(Mode.bean));
	}

	@Test
	void definitionSourceAllValues() {
		assertThat(DefinitionSource.document, notNullValue());
		assertThat(DefinitionSource.view, notNullValue());
		assertThat(DefinitionSource.query, notNullValue());
		assertThat(DefinitionSource.list, notNullValue());
	}

	@Test
	void reportTypeAllValues() {
		assertThat(ReportType.report, notNullValue());
		assertThat(ReportType.subreport, notNullValue());
		assertFalse(ReportType.report.equals(ReportType.subreport));
	}

	@Test
	void orientationAllValues() {
		assertThat(Orientation.portrait, notNullValue());
		assertThat(Orientation.landscape, notNullValue());
		assertFalse(Orientation.portrait.equals(Orientation.landscape));
	}

	@Test
	void nameRoundTrip() {
		spec.setName("myReport");
		assertThat(spec.getName(), is("myReport"));
	}

	@Test
	void moduleAndDocumentRoundTrip() {
		spec.setModuleName("test");
		spec.setDocumentName("AllAttributesPersistent");
		assertThat(spec.getModuleName(), is("test"));
		assertThat(spec.getDocumentName(), is("AllAttributesPersistent"));
	}

	@Test
	void geometryRoundTrip() {
		spec.setWidth(Integer.valueOf(842));
		spec.setHeight(Integer.valueOf(595));
		spec.setLeftMargin(Integer.valueOf(20));
		spec.setRightMargin(Integer.valueOf(20));
		spec.setTopMargin(Integer.valueOf(20));
		spec.setBottomMargin(Integer.valueOf(20));
		spec.setColumnWidth(Integer.valueOf(802));

		assertThat(spec.getWidth(), is(Integer.valueOf(842)));
		assertThat(spec.getHeight(), is(Integer.valueOf(595)));
		assertThat(spec.getLeftMargin(), is(Integer.valueOf(20)));
		assertThat(spec.getRightMargin(), is(Integer.valueOf(20)));
		assertThat(spec.getTopMargin(), is(Integer.valueOf(20)));
		assertThat(spec.getBottomMargin(), is(Integer.valueOf(20)));
		assertThat(spec.getColumnWidth(), is(Integer.valueOf(802)));
	}

	@Test
	void flagsRoundTrip() {
		spec.setSaveToDocumentPackage(Boolean.TRUE);
		spec.setRenderLabelAsTextFields(Boolean.TRUE);
		spec.setDefaultBorder(Boolean.TRUE);
		spec.setIncludePageNumbers(Boolean.TRUE);

		assertThat(spec.getSaveToDocumentPackage(), is(Boolean.TRUE));
		assertThat(spec.getRenderLabelAsTextFields(), is(Boolean.TRUE));
		assertThat(spec.getDefaultBorder(), is(Boolean.TRUE));
		assertThat(spec.getIncludePageNumbers(), is(Boolean.TRUE));
	}

	@Test
	void listPropertiesInitiallyEmpty() {
		assertThat(spec.getFields(), notNullValue());
		assertTrue(spec.getFields().isEmpty());
		assertThat(spec.getBands(), notNullValue());
		assertTrue(spec.getBands().isEmpty());
		assertThat(spec.getParameters(), notNullValue());
		assertTrue(spec.getParameters().isEmpty());
		assertThat(spec.getVariables(), notNullValue());
		assertTrue(spec.getVariables().isEmpty());
		assertThat(spec.getSubReports(), notNullValue());
		assertTrue(spec.getSubReports().isEmpty());
	}

	@Test
	void defaultConstructorSetsPortraitOrientation() {
		assertThat(spec.getOrientation(), is(Orientation.portrait));
	}

	@Test
	void defaultConstructorSetsPortraitGeometry() {
		// default portrait: width=595, height=842
		assertThat(spec.getWidth(), is(Integer.valueOf(595)));
		assertThat(spec.getHeight(), is(Integer.valueOf(842)));
	}

	@Test
	void orientationRoundTrip() {
		spec.setOrientation(Orientation.landscape);
		assertThat(spec.getOrientation(), is(Orientation.landscape));
	}

	@Test
	void resetDesignLandscapeFlipsDimensions() {
		spec.setOrientation(Orientation.landscape);
		spec.resetDesign();
		assertThat(spec.getWidth(), is(Integer.valueOf(842)));
		assertThat(spec.getHeight(), is(Integer.valueOf(595)));
	}

	@Test
	void uxuiRoundTrip() {
		spec.setUxui("desktop");
		assertThat(spec.getUxui(), is("desktop"));
	}

	@Test
	void repositoryPathRoundTrip() {
		spec.setRepositoryPath("/some/path");
		assertThat(spec.getRepositoryPath(), is("/some/path"));
	}

	@Test
	void defaultFontNameRoundTrip() {
		spec.setDefaultFontName("Arial");
		assertThat(spec.getDefaultFontName(), is("Arial"));
	}

	@Test
	void titleFontSizeRoundTrip() {
		spec.setTitleFontSize(Integer.valueOf(18));
		assertThat(spec.getTitleFontSize(), is(Integer.valueOf(18)));
	}

	@Test
	void defaultFontSizeRoundTrip() {
		spec.setDefaultFontSize(Integer.valueOf(10));
		assertThat(spec.getDefaultFontSize(), is(Integer.valueOf(10)));
	}

	@Test
	void dynamicFlowRoundTrip() {
		spec.setDynamicFlow(Boolean.FALSE);
		assertThat(spec.getDynamicFlow(), is(Boolean.FALSE));
	}

	@Test
	void bandSplitTypeRoundTrip() {
		spec.setBandSplitType(ReportBand.SplitType.stretch);
		assertThat(spec.getBandSplitType(), is(ReportBand.SplitType.stretch));
	}

	@Test
	void boldLabelsRoundTrip() {
		spec.setBoldLabels(Boolean.FALSE);
		assertThat(spec.getBoldLabels(), is(Boolean.FALSE));
	}

	@Test
	void checkBoxFontNameRoundTrip() {
		spec.setCheckBoxFontName("Wingdings");
		assertThat(spec.getCheckBoxFontName(), is("Wingdings"));
	}

	@Test
	void checkBoxDisplayExpressionRoundTrip() {
		spec.setCheckBoxDisplayExpression("$F{active}");
		assertThat(spec.getCheckBoxDisplayExpression(), is("$F{active}"));
	}

	@Test
	void defaultLineColourRoundTrip() {
		spec.setDefaultLineColour("#000000");
		assertThat(spec.getDefaultLineColour(), is("#000000"));
	}

	@Test
	void defaultLineWidthRoundTrip() {
		spec.setDefaultLineWidth(new Decimal2(2.0));
		assertThat(spec.getDefaultLineWidth(), is(new Decimal2(2.0)));
	}

	@Test
	void defaultBorderFlagsRoundTrip() {
		spec.setDefaultBorderTop(Boolean.TRUE);
		spec.setDefaultBorderLeft(Boolean.TRUE);
		spec.setDefaultBorderBottom(Boolean.TRUE);
		spec.setDefaultBorderRight(Boolean.TRUE);

		assertThat(spec.getDefaultBorderTop(), is(Boolean.TRUE));
		assertThat(spec.getDefaultBorderLeft(), is(Boolean.TRUE));
		assertThat(spec.getDefaultBorderBottom(), is(Boolean.TRUE));
		assertThat(spec.getDefaultBorderRight(), is(Boolean.TRUE));
	}

	@Test
	void sectionBorderFlagsRoundTrip() {
		spec.setSectionBorderTop(Boolean.TRUE);
		spec.setSectionBorderLeft(Boolean.TRUE);
		spec.setSectionBorderRight(Boolean.FALSE);
		spec.setSectionBorderBottom(Boolean.TRUE);

		assertThat(spec.getSectionBorderTop(), is(Boolean.TRUE));
		assertThat(spec.getSectionBorderLeft(), is(Boolean.TRUE));
		assertThat(spec.getSectionBorderRight(), is(Boolean.FALSE));
		assertThat(spec.getSectionBorderBottom(), is(Boolean.TRUE));
	}

	@Test
	void sectionTitleBorderFlagsRoundTrip() {
		spec.setSectionTitleBorderTop(Boolean.TRUE);
		spec.setSectionTitleBorderLeft(Boolean.FALSE);
		spec.setSectionTitleBorderRight(Boolean.TRUE);
		spec.setSectionTitleBorderBottom(Boolean.FALSE);

		assertThat(spec.getSectionTitleBorderTop(), is(Boolean.TRUE));
		assertThat(spec.getSectionTitleBorderLeft(), is(Boolean.FALSE));
		assertThat(spec.getSectionTitleBorderRight(), is(Boolean.TRUE));
		assertThat(spec.getSectionTitleBorderBottom(), is(Boolean.FALSE));
	}

	@Test
	void sectionTitleColourRoundTrip() {
		spec.setSectionTitleForeground("#FFFFFF");
		spec.setSectionTitleBackground("#333333");
		assertThat(spec.getSectionTitleForeground(), is("#FFFFFF"));
		assertThat(spec.getSectionTitleBackground(), is("#333333"));
	}

	@Test
	void defaultCellPaddingRoundTrip() {
		spec.setDefaultCellTopPadding(Integer.valueOf(4));
		spec.setDefaultCellLeftPadding(Integer.valueOf(4));
		spec.setDefaultCellBottomPadding(Integer.valueOf(4));
		spec.setDefaultCellRightPadding(Integer.valueOf(4));

		assertThat(spec.getDefaultCellTopPadding(), is(Integer.valueOf(4)));
		assertThat(spec.getDefaultCellLeftPadding(), is(Integer.valueOf(4)));
		assertThat(spec.getDefaultCellBottomPadding(), is(Integer.valueOf(4)));
		assertThat(spec.getDefaultCellRightPadding(), is(Integer.valueOf(4)));
	}

	@Test
	void labelAlignmentOverrideRoundTrip() {
		spec.setLabelAlignmentOverride(ReportElement.ElementAlignment.right);
		assertThat(spec.getLabelAlignmentOverride(), is(ReportElement.ElementAlignment.right));
	}

	@Test
	void addJoinCreatesJoinMaps() {
		spec.addJoin("MyDocument", "a1", "join MyDocument a1 on a1.parent_id = a.bizId");
		assertThat(spec.getJoins(), notNullValue());
		assertFalse(spec.getJoins().isEmpty());
		assertThat(spec.getJoinAlias(), notNullValue());
		assertFalse(spec.getJoinAlias().isEmpty());
	}

	@Test
	void includeCustomerLogoRoundTrip() {
		spec.setIncludeCustomerLogo(true);
		assertTrue(spec.isIncludeCustomerLogo());
	}

	@Test
	void parentReportPersistentNameRoundTrip() {
		spec.setParentReportPersistentName("TST_AllAttributes");
		assertThat(spec.getParentReportPersistentName(), is("TST_AllAttributes"));
	}

	@Test
	void pixelToTwipRoundTrip() {
		spec.setPixelToTwip(new Decimal5(15.0));
		assertThat(spec.getPixelToTwip(), is(new Decimal5(15.0)));
	}

	@Test
	void verticaliseRoundTrip() {
		spec.setVerticalise(Boolean.TRUE);
		assertThat(spec.getVerticalise(), is(Boolean.TRUE));
	}

	@Test
	void queryNameRoundTrip() {
		spec.setQueryName("qMyQuery");
		assertThat(spec.getQueryName(), is("qMyQuery"));
	}

	@Test
	void joinsInitiallyNull() {
		assertThat(spec.getJoins(), nullValue());
		assertThat(spec.getJoinAlias(), nullValue());
	}
}
