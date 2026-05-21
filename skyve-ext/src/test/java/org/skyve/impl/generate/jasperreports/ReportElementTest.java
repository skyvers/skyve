package org.skyve.impl.generate.jasperreports;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import org.junit.jupiter.api.Test;
import org.skyve.domain.types.Decimal2;
import org.skyve.impl.generate.jasperreports.ReportElement.ElementAlignment;
import org.skyve.impl.generate.jasperreports.ReportElement.ElementType;
import org.skyve.impl.generate.jasperreports.ReportElement.EvaluationTime;
import org.skyve.impl.metadata.view.HorizontalAlignment;

@SuppressWarnings("static-method")
class ReportElementTest {

	private static ReportElement simple() {
		return new ReportElement(ElementType.staticText, "test", "val",
				Integer.valueOf(0), Integer.valueOf(0), Integer.valueOf(200), null);
	}

	@Test
	void elementTypeEnumExists() {
		// spot-check several ElementType values
		assertThat(ElementType.staticText.name(), is("staticText"));
		assertThat(ElementType.textField.name(), is("textField"));
		assertThat(ElementType.subreport.name(), is("subreport"));
		assertThat(ElementType.line.name(), is("line"));
	}

	@Test
	void alignmentToStringIsCapitalised() {
		assertThat(ElementAlignment.left.toString(), is("Left"));
		assertThat(ElementAlignment.right.toString(), is("Right"));
		assertThat(ElementAlignment.center.toString(), is("Center"));
	}

	@Test
	void fromHorizontalAlignmentCentre() {
		// Skyve spells it "centre", JasperReports spells it "center"
		ElementAlignment result = ElementAlignment.fromHorizontalAlignment(HorizontalAlignment.centre);
		assertThat(result, is(ElementAlignment.center));
	}

	@Test
	void fromHorizontalAlignmentLeft() {
		ElementAlignment result = ElementAlignment.fromHorizontalAlignment(HorizontalAlignment.left);
		assertThat(result, is(ElementAlignment.left));
	}

	@Test
	void fromHorizontalAlignmentRight() {
		ElementAlignment result = ElementAlignment.fromHorizontalAlignment(HorizontalAlignment.right);
		assertThat(result, is(ElementAlignment.right));
	}

	@Test
	void fromHorizontalAlignmentNull() {
		ElementAlignment result = ElementAlignment.fromHorizontalAlignment(null);
		assertThat(result, nullValue());
	}

	@Test
	void evaluationTimeToStringIsCapitalised() {
		assertThat(EvaluationTime.now.toString(), is("Now"));
		assertThat(EvaluationTime.report.toString(), is("Report"));
		assertThat(EvaluationTime.auto.toString(), is("Auto"));
	}

	@Test
	void geometryRoundTrip() {
		ReportElement elem = new ReportElement(ElementType.staticText, "test", "test",
				Integer.valueOf(10), Integer.valueOf(5), Integer.valueOf(200), null);
		elem.setElementHeight(Integer.valueOf(30));

		assertThat(elem.getElementTop(), is(Integer.valueOf(10)));
		assertThat(elem.getElementLeft(), is(Integer.valueOf(5)));
		assertThat(elem.getElementWidth(), is(Integer.valueOf(200)));
		assertThat(elem.getElementHeight(), is(Integer.valueOf(30)));
	}

	@Test
	void ordinalRoundTrip() {
		ReportElement e = simple();
		e.setOrdinal(Integer.valueOf(3));
		assertThat(e.getOrdinal(), is(Integer.valueOf(3)));
	}

	@Test
	void nameRoundTrip() {
		ReportElement e = simple();
		e.setName("myField");
		assertThat(e.getName(), is("myField"));
	}

	@Test
	void elementTypeRoundTrip() {
		ReportElement e = simple();
		e.setElementType(ElementType.textField);
		assertThat(e.getElementType(), is(ElementType.textField));
	}

	@Test
	void elementBorderRoundTrip() {
		ReportElement e = simple();
		e.setElementBorder(Boolean.TRUE);
		assertThat(e.getElementBorder(), is(Boolean.TRUE));
	}

	@Test
	void elementValueRoundTrip() {
		ReportElement e = simple();
		e.setElementValue("$F{text}");
		assertThat(e.getElementValue(), is("$F{text}"));
	}

	@Test
	void elementAlignmentRoundTrip() {
		ReportElement e = simple();
		e.setElementAlignment(ElementAlignment.right);
		assertThat(e.getElementAlignment(), is(ElementAlignment.right));
	}

	@Test
	void elementFontNameRoundTrip() {
		ReportElement e = simple();
		e.setElementFontName("Helvetica");
		assertThat(e.getElementFontName(), is("Helvetica"));
	}

	@Test
	void elementFontSizeRoundTrip() {
		ReportElement e = simple();
		e.setElementFontSize(Integer.valueOf(14));
		assertThat(e.getElementFontSize(), is(Integer.valueOf(14)));
	}

	@Test
	void evaluationTimeRoundTrip() {
		ReportElement e = simple();
		e.setEvaluationTime(EvaluationTime.report);
		assertThat(e.getEvaluationTime(), is(EvaluationTime.report));
	}

	@Test
	void collectionDocumentNameRoundTrip() {
		ReportElement e = simple();
		e.setCollectionDocumentName("ChildDocument");
		assertThat(e.getCollectionDocumentName(), is("ChildDocument"));
	}

	@Test
	void paddingRoundTrip() {
		ReportElement e = simple();
		e.setTopPadding(Integer.valueOf(2));
		e.setLeftPadding(Integer.valueOf(3));
		e.setBottomPadding(Integer.valueOf(4));
		e.setRightPadding(Integer.valueOf(5));

		assertThat(e.getTopPadding(), is(Integer.valueOf(2)));
		assertThat(e.getLeftPadding(), is(Integer.valueOf(3)));
		assertThat(e.getBottomPadding(), is(Integer.valueOf(4)));
		assertThat(e.getRightPadding(), is(Integer.valueOf(5)));
	}

	@Test
	void borderColourRoundTrip() {
		ReportElement e = simple();
		e.setBorderColour("#FF0000");
		assertThat(e.getBorderColour(), is("#FF0000"));
	}

	@Test
	void borderLineWidthRoundTrip() {
		ReportElement e = simple();
		e.setBorderLineWidth(new Decimal2(1.5));
		assertThat(e.getBorderLineWidth(), is(new Decimal2(1.5)));
	}

	@Test
	void borderSideFlagsRoundTrip() {
		ReportElement e = simple();
		e.setBorderTop(Boolean.TRUE);
		e.setBorderLeft(Boolean.FALSE);
		e.setBorderBottom(Boolean.TRUE);
		e.setBorderRight(Boolean.FALSE);

		assertThat(e.getBorderTop(), is(Boolean.TRUE));
		assertThat(e.getBorderLeft(), is(Boolean.FALSE));
		assertThat(e.getBorderBottom(), is(Boolean.TRUE));
		assertThat(e.getBorderRight(), is(Boolean.FALSE));
	}

	@Test
	void dynamicFlowRoundTrip() {
		ReportElement e = simple();
		e.setDynamicFlow(Boolean.TRUE);
		assertThat(e.getDynamicFlow(), is(Boolean.TRUE));
	}

	@Test
	void elementBoldRoundTrip() {
		ReportElement e = simple();
		e.setElementBold(Boolean.TRUE);
		assertThat(e.getElementBold(), is(Boolean.TRUE));
	}

	@Test
	void elementItalicRoundTrip() {
		ReportElement e = simple();
		e.setElementItalic(Boolean.TRUE);
		assertThat(e.getElementItalic(), is(Boolean.TRUE));
	}

	@Test
	void foreAndBackColourRoundTrip() {
		ReportElement e = simple();
		e.setElementForeColour("#111111");
		e.setElementBackColour("#EEEEEE");
		assertThat(e.getElementForeColour(), is("#111111"));
		assertThat(e.getElementBackColour(), is("#EEEEEE"));
	}

	@Test
	void invisibleConditionNameRoundTrip() {
		ReportElement e = simple();
		e.setInvisibleConditionName("notActive");
		assertThat(e.getInvisibleConditionName(), is("notActive"));
	}

	@Test
	void pixelAndPercentageAndResponsiveWidthRoundTrip() {
		ReportElement e = simple();
		e.setPixelWidth(Integer.valueOf(100));
		e.setPercentageWidth(Integer.valueOf(50));
		e.setResponsiveWidth(Integer.valueOf(6));
		assertThat(e.getPixelWidth(), is(Integer.valueOf(100)));
		assertThat(e.getPercentageWidth(), is(Integer.valueOf(50)));
		assertThat(e.getResponsiveWidth(), is(Integer.valueOf(6)));
	}

	@Test
	void rowRoundTrip() {
		ReportElement e = simple();
		e.setRow(Integer.valueOf(3));
		assertThat(e.getRow(), is(Integer.valueOf(3)));
	}

	@Test
	void reportFileNameRoundTrip() {
		ReportElement e = simple();
		e.setReportFileName("myReport.jasper");
		assertThat(e.getReportFileName(), is("myReport.jasper"));
	}

	@Test
	void fieldRoundTrip() {
		ReportElement e = simple();
		ReportField field = new ReportField();
		field.setName("myField");
		e.setField(field);
		assertThat(e.getField().getName(), is("myField"));
	}

        @Test
        void getJrxmlReturnsNonNullString() {
                DesignSpecification ds = new DesignSpecification();
                ds.setModuleName("test");
                ds.setDocumentName("TestDoc");
                ds.setMode(DesignSpecification.Mode.bean);

                ReportBand band = new ReportBand();
                band.setParent(ds);
                band.setBandType(ReportBand.BandType.detail);

                ReportElement e = new ReportElement(ElementType.staticText, "lbl", "Label", null, null, null, null);
                e.setParent(band);
                band.addElement(e);

                assertThat(e.getJrxml(), notNullValue());
        }

        @Test
        void constructorWithNonNullAlignmentSetsAlignment() {
                // Covers the else branch at line 557 in ReportElement.java
                ReportElement e = new ReportElement(ElementType.staticText, "lbl", "Label",
                                null, null, Integer.valueOf(0), Integer.valueOf(0),
                                Integer.valueOf(200), null, null, ElementAlignment.right, Boolean.FALSE, Boolean.FALSE, null);
                assertThat(e.getElementAlignment(), is(ElementAlignment.right));
        }
}
