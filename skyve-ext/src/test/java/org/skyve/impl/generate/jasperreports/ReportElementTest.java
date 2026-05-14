package org.skyve.impl.generate.jasperreports;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import org.junit.jupiter.api.Test;
import org.skyve.domain.types.Decimal2;
import org.skyve.impl.generate.jasperreports.ReportElement.ElementAlignment;
import org.skyve.impl.generate.jasperreports.ReportElement.ElementType;
import org.skyve.impl.generate.jasperreports.ReportElement.EvaluationTime;
import org.skyve.impl.metadata.view.HorizontalAlignment;

@SuppressWarnings("static-method")
public class ReportElementTest {

	private static ReportElement simple() {
		return new ReportElement(ElementType.staticText, "test", "val",
				Integer.valueOf(0), Integer.valueOf(0), Integer.valueOf(200), null);
	}

	@Test
	public void elementTypeEnumExists() {
		// spot-check several ElementType values
		assertThat(ElementType.staticText.name(), is("staticText"));
		assertThat(ElementType.textField.name(), is("textField"));
		assertThat(ElementType.subreport.name(), is("subreport"));
		assertThat(ElementType.line.name(), is("line"));
	}

	@Test
	public void alignmentToStringIsCapitalised() {
		assertThat(ElementAlignment.left.toString(), is("Left"));
		assertThat(ElementAlignment.right.toString(), is("Right"));
		assertThat(ElementAlignment.center.toString(), is("Center"));
	}

	@Test
	public void fromHorizontalAlignmentCentre() {
		// Skyve spells it "centre", JasperReports spells it "center"
		ElementAlignment result = ElementAlignment.fromHorizontalAlignment(HorizontalAlignment.centre);
		assertThat(result, is(ElementAlignment.center));
	}

	@Test
	public void fromHorizontalAlignmentLeft() {
		ElementAlignment result = ElementAlignment.fromHorizontalAlignment(HorizontalAlignment.left);
		assertThat(result, is(ElementAlignment.left));
	}

	@Test
	public void fromHorizontalAlignmentRight() {
		ElementAlignment result = ElementAlignment.fromHorizontalAlignment(HorizontalAlignment.right);
		assertThat(result, is(ElementAlignment.right));
	}

	@Test
	public void fromHorizontalAlignmentNull() {
		ElementAlignment result = ElementAlignment.fromHorizontalAlignment(null);
		assertThat(result, nullValue());
	}

	@Test
	public void evaluationTimeToStringIsCapitalised() {
		assertThat(EvaluationTime.now.toString(), is("Now"));
		assertThat(EvaluationTime.report.toString(), is("Report"));
		assertThat(EvaluationTime.auto.toString(), is("Auto"));
	}

	@Test
	public void geometryRoundTrip() {
		ReportElement elem = new ReportElement(ElementType.staticText, "test", "test",
				Integer.valueOf(10), Integer.valueOf(5), Integer.valueOf(200), null);
		elem.setElementHeight(Integer.valueOf(30));

		assertThat(elem.getElementTop(), is(Integer.valueOf(10)));
		assertThat(elem.getElementLeft(), is(Integer.valueOf(5)));
		assertThat(elem.getElementWidth(), is(Integer.valueOf(200)));
		assertThat(elem.getElementHeight(), is(Integer.valueOf(30)));
	}

	@Test
	public void ordinalRoundTrip() {
		ReportElement e = simple();
		e.setOrdinal(Integer.valueOf(3));
		assertThat(e.getOrdinal(), is(Integer.valueOf(3)));
	}

	@Test
	public void nameRoundTrip() {
		ReportElement e = simple();
		e.setName("myField");
		assertThat(e.getName(), is("myField"));
	}

	@Test
	public void elementTypeRoundTrip() {
		ReportElement e = simple();
		e.setElementType(ElementType.textField);
		assertThat(e.getElementType(), is(ElementType.textField));
	}

	@Test
	public void elementBorderRoundTrip() {
		ReportElement e = simple();
		e.setElementBorder(Boolean.TRUE);
		assertThat(e.getElementBorder(), is(Boolean.TRUE));
	}

	@Test
	public void elementValueRoundTrip() {
		ReportElement e = simple();
		e.setElementValue("$F{text}");
		assertThat(e.getElementValue(), is("$F{text}"));
	}

	@Test
	public void elementAlignmentRoundTrip() {
		ReportElement e = simple();
		e.setElementAlignment(ElementAlignment.right);
		assertThat(e.getElementAlignment(), is(ElementAlignment.right));
	}

	@Test
	public void elementFontNameRoundTrip() {
		ReportElement e = simple();
		e.setElementFontName("Helvetica");
		assertThat(e.getElementFontName(), is("Helvetica"));
	}

	@Test
	public void elementFontSizeRoundTrip() {
		ReportElement e = simple();
		e.setElementFontSize(Integer.valueOf(14));
		assertThat(e.getElementFontSize(), is(Integer.valueOf(14)));
	}

	@Test
	public void evaluationTimeRoundTrip() {
		ReportElement e = simple();
		e.setEvaluationTime(EvaluationTime.report);
		assertThat(e.getEvaluationTime(), is(EvaluationTime.report));
	}

	@Test
	public void collectionDocumentNameRoundTrip() {
		ReportElement e = simple();
		e.setCollectionDocumentName("ChildDocument");
		assertThat(e.getCollectionDocumentName(), is("ChildDocument"));
	}

	@Test
	public void paddingRoundTrip() {
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
	public void borderColourRoundTrip() {
		ReportElement e = simple();
		e.setBorderColour("#FF0000");
		assertThat(e.getBorderColour(), is("#FF0000"));
	}

	@Test
	public void borderLineWidthRoundTrip() {
		ReportElement e = simple();
		e.setBorderLineWidth(new Decimal2(1.5));
		assertThat(e.getBorderLineWidth(), is(new Decimal2(1.5)));
	}

	@Test
	public void borderSideFlagsRoundTrip() {
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
	public void dynamicFlowRoundTrip() {
		ReportElement e = simple();
		e.setDynamicFlow(Boolean.TRUE);
		assertThat(e.getDynamicFlow(), is(Boolean.TRUE));
	}

	@Test
	public void elementBoldRoundTrip() {
		ReportElement e = simple();
		e.setElementBold(Boolean.TRUE);
		assertThat(e.getElementBold(), is(Boolean.TRUE));
	}

	@Test
	public void elementItalicRoundTrip() {
		ReportElement e = simple();
		e.setElementItalic(Boolean.TRUE);
		assertThat(e.getElementItalic(), is(Boolean.TRUE));
	}

	@Test
	public void foreAndBackColourRoundTrip() {
		ReportElement e = simple();
		e.setElementForeColour("#111111");
		e.setElementBackColour("#EEEEEE");
		assertThat(e.getElementForeColour(), is("#111111"));
		assertThat(e.getElementBackColour(), is("#EEEEEE"));
	}
}
