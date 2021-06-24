package org.skyve.impl.generate.jasperreports;

import java.awt.Color;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;

import org.apache.commons.lang3.StringUtils;
import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.types.Decimal2;
import org.skyve.impl.generate.jasperreports.DesignSpecification.Mode;
import org.skyve.impl.generate.jasperreports.ReportBand.BandType;
import org.skyve.impl.report.jasperreports.ReportDesignParameters;
import org.skyve.impl.tools.jasperreports.SkyveDocumentExecuterFactory;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.Persistent;
import org.skyve.metadata.model.document.Collection;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.MetaDataQueryColumn;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.model.list.DocumentQueryListModel;
import org.skyve.report.ReportFormat;
import org.skyve.util.Util;

import net.sf.jasperreports.engine.DefaultJasperReportsContext;
import net.sf.jasperreports.engine.JRBand;
import net.sf.jasperreports.engine.JRElement;
import net.sf.jasperreports.engine.JRException;
import net.sf.jasperreports.engine.JRExpression;
import net.sf.jasperreports.engine.JRField;
import net.sf.jasperreports.engine.JRLineBox;
import net.sf.jasperreports.engine.JRReport;
import net.sf.jasperreports.engine.JasperCompileManager;
import net.sf.jasperreports.engine.JasperReport;
import net.sf.jasperreports.engine.JasperReportsContext;
import net.sf.jasperreports.engine.base.JRBoxPen;
import net.sf.jasperreports.engine.design.JRDesignBand;
import net.sf.jasperreports.engine.design.JRDesignElement;
import net.sf.jasperreports.engine.design.JRDesignExpression;
import net.sf.jasperreports.engine.design.JRDesignField;
import net.sf.jasperreports.engine.design.JRDesignImage;
import net.sf.jasperreports.engine.design.JRDesignLine;
import net.sf.jasperreports.engine.design.JRDesignParameter;
import net.sf.jasperreports.engine.design.JRDesignQuery;
import net.sf.jasperreports.engine.design.JRDesignRectangle;
import net.sf.jasperreports.engine.design.JRDesignSection;
import net.sf.jasperreports.engine.design.JRDesignStaticText;
import net.sf.jasperreports.engine.design.JRDesignSubreport;
import net.sf.jasperreports.engine.design.JRDesignSubreportParameter;
import net.sf.jasperreports.engine.design.JRDesignTextElement;
import net.sf.jasperreports.engine.design.JRDesignTextField;
import net.sf.jasperreports.engine.design.JRDesignVariable;
import net.sf.jasperreports.engine.design.JasperDesign;
import net.sf.jasperreports.engine.query.JRQueryExecuterFactory;
import net.sf.jasperreports.engine.type.CalculationEnum;
import net.sf.jasperreports.engine.type.EvaluationTimeEnum;
import net.sf.jasperreports.engine.type.HorizontalTextAlignEnum;
import net.sf.jasperreports.engine.type.IncrementTypeEnum;
import net.sf.jasperreports.engine.type.LineStyleEnum;
import net.sf.jasperreports.engine.type.ModeEnum;
import net.sf.jasperreports.engine.type.PositionTypeEnum;
import net.sf.jasperreports.engine.type.ResetTypeEnum;
import net.sf.jasperreports.engine.type.RotationEnum;
import net.sf.jasperreports.engine.type.ScaleImageEnum;
import net.sf.jasperreports.engine.type.SplitTypeEnum;
import net.sf.jasperreports.engine.type.StretchTypeEnum;
import net.sf.jasperreports.engine.type.VerticalTextAlignEnum;
import net.sf.jasperreports.engine.xml.JRXmlWriter;

public class JasperReportRenderer {

    public static final String DESIGN_SPEC_PARAMETER_NAME = "DESIGN_SPEC";
    protected final JasperDesign jasperDesign;
    protected final DesignSpecification designSpecification;
    protected final ReportDesignParameters reportDesignParameters;

	private static final Float FONT_TEN = Float.valueOf(10f);
	private static final Float FONT_TWELVE = Float.valueOf(12f);
	private static final Float FONT_TWENTY_SIX = Float.valueOf(26f);

    private static final Map<String, String> properties;
    private boolean rendered = false;

    private String jrxml;

    static {
        properties = new HashMap<>();
        properties.put("ireport.encoding", "UTF-8");
        properties.put("ireport.zoom", "1.0");
        properties.put("ireport.x", "0");
        properties.put("ireport.y", "0");
    }

    private static final List<String> imports;

    static {
        imports = new ArrayList<>();
        imports.add("net.sf.jasperreports.engine.*");
        imports.add("java.util.*");
        imports.add("net.sf.jasperreports.engine.data.*");
    }

    public JasperReportRenderer(DesignSpecification designSpecification) {
        // Currently lists can only be rendered using the alternative report design abstraction
        // so instead of re-implementing we convert to that design.
        if (DesignSpecification.DefinitionSource.list.equals(designSpecification.getDefinitionSource())) {
            this.designSpecification = null;
            this.reportDesignParameters = convertDesign(designSpecification);
        } else {
            this.designSpecification = designSpecification;
            this.reportDesignParameters = null;
        }
        jasperDesign = new JasperDesign();
    }

    public JasperReportRenderer(ReportDesignParameters reportDesignParameters) {
        this.reportDesignParameters = reportDesignParameters;
        this.designSpecification = null;
        jasperDesign = new JasperDesign();
    }

    public JasperReport getReport() throws Exception {
        if (!rendered) {
            renderDesign();
        }
        return JasperCompileManager.compileReport(jasperDesign);
    }

    public String getJrxml() throws Exception {
        if (!rendered) {
            renderDesign();
        }

        return jrxml;
    }

    public static JasperReport getSubReport(DesignSpecification designSpecification, String subReport) throws Exception {
        final DesignSpecification subReportSpec = designSpecification.getSubReports().stream()
                .filter(r -> r.getName().equals(subReport))
                .findFirst()
                .orElse(null);
        if (subReportSpec == null) {
            throw new IllegalArgumentException(String.format("Specified subreport %s does not exist.", subReport));
        }

        final JasperReportRenderer subReportRenderer = new JasperReportRenderer(subReportSpec);
        return subReportRenderer.getReport();
    }

    public String renderDesign() throws Exception {
        if (rendered) {
            throw new IllegalStateException("Report has already been rendered.");
        }

        /*
        * Note there are currently two different report abstractions which are converted to the JasperDesign.
        * In the future these could be merged into the one.
        */
        if (designSpecification != null) {
            jrxml = renderFromDesignSpecification();
            return jrxml;
        }
        if (reportDesignParameters != null) {
            jrxml = renderFromReportDesignParameters();
            return jrxml;
        }

        throw new IllegalStateException("No design to render.");
    }

    private String renderFromDesignSpecification() throws JRException {
        if (designSpecification.getModuleName() != null && designSpecification.getDocumentName() != null) {
            configureReportProperties(designSpecification);

            // support document queries
            JasperReportsContext jasperReportsContext = DefaultJasperReportsContext.getInstance();
            jasperReportsContext.setProperty(JRQueryExecuterFactory.QUERY_EXECUTER_FACTORY_PREFIX + "document", SkyveDocumentExecuterFactory.class.getCanonicalName());

            Optional.ofNullable(designSpecification.getLanguage()).ifPresent(jasperDesign::setLanguage);

            addProperties();
            addImports();
            addParameters(designSpecification);
            addQuery(designSpecification);
            addFields(designSpecification);
            addVariables(designSpecification);
            addBands(designSpecification);

            final String result = JRXmlWriter.writeReport(jasperDesign, "UTF-8");
            rendered = true;
            return result;
        }
        throw new IllegalArgumentException("Invalid module or document name.");
    }

    private String renderFromReportDesignParameters() throws JRException {
        int reportColumnWidth = reportDesignParameters.getPageWidth() - reportDesignParameters.getLeftMargin() - reportDesignParameters.getRightMargin();
        ReportFormat format = reportDesignParameters.getReportFormat();
        boolean wideStaticTexts = ReportFormat.csv.equals(format) ||
                ReportFormat.txt.equals(format) ||
                ReportFormat.xml.equals(format);

        // JasperDesign
        jasperDesign.setName("Export");
        jasperDesign.setLanguage(JRReport.LANGUAGE_JAVA);
        jasperDesign.setPageWidth(reportDesignParameters.getPageWidth());
        jasperDesign.setPageHeight(reportDesignParameters.getPageHeight());
        jasperDesign.setColumnWidth(reportColumnWidth);
        jasperDesign.setColumnSpacing(0);
        jasperDesign.setLeftMargin(reportDesignParameters.getLeftMargin());
        jasperDesign.setRightMargin(reportDesignParameters.getRightMargin());
        jasperDesign.setTopMargin(reportDesignParameters.getTopMargin());
        jasperDesign.setBottomMargin(reportDesignParameters.getBottomMargin());
        jasperDesign.setIgnorePagination(! reportDesignParameters.isPaginated());

        // Parameters
        JRDesignParameter parameter = new JRDesignParameter();
        parameter.setName("TITLE");
        parameter.setValueClass(String.class);
        jasperDesign.addParameter(parameter);

        parameter = new JRDesignParameter();
        parameter.setName("RESOURCE_DIR");
        parameter.setValueClass(String.class);
        jasperDesign.addParameter(parameter);

        // TODO allow grouping here

        JRDesignBand band;
        JRDesignStaticText staticText;
        JRDesignTextField textField;
        JRDesignLine line;
        JRDesignExpression expression;

        JRDesignBand columnHeaderBand = new JRDesignBand();
        if (ReportDesignParameters.ReportStyle.tabular.equals(reportDesignParameters.getReportStyle())) {
            columnHeaderBand.setHeight(18);
        }

        JRDesignBand detailBand = new JRDesignBand();
        if (ReportDesignParameters.ReportStyle.tabular.equals(reportDesignParameters.getReportStyle())) {
            detailBand.setHeight(20);
        }
        else {
            detailBand.setHeight(reportDesignParameters.getColumns().size() * 20 + 10);
        }

        final JRDesignBand columnFooterBand = new JRDesignBand();
        if (ReportDesignParameters.ReportStyle.tabular.equals(reportDesignParameters.getReportStyle())) {
            columnFooterBand.setHeight(20);
        }

        final JRDesignBand summaryBand = new JRDesignBand();

        int xPos = 0;
        int yPos = 0;
        int columnarLabelWidth = 0;

        // Determine the size of the labels (for columnar reports)
        if (ReportDesignParameters.ReportStyle.columnar.equals(reportDesignParameters.getReportStyle())) {
            for (ReportDesignParameters.ReportColumn column : reportDesignParameters.getColumns()) {
                columnarLabelWidth = Math.max(columnarLabelWidth, column.getTitle().length() * 8);
            }
        }

        JRDesignField thisField = new JRDesignField();
        thisField.setName("THIS");
        thisField.setValueClass(Bean.class);
        jasperDesign.addField(thisField);

        JRDesignField userField = new JRDesignField();
        userField.setName("USER");
        userField.setValueClass(User.class);
        jasperDesign.addField(userField);

        for (ReportDesignParameters.ReportColumn column : reportDesignParameters.getColumns()) {
            // Field
            JRDesignField designField = new JRDesignField();
            designField.setName(String.format("%s_display", column.getName()));
            designField.setDescription(column.getName());
            designField.setValueClass(String.class);
            jasperDesign.addField(designField);

            if (isAggregatableAttribute(column.getAttributeType())) {
            	// Aggregate field
            	JRDesignField aggregateField = new JRDesignField();
                aggregateField.setName(column.getName());
                aggregateField.setValueClass(isDateOrTimeAttribute(column.getAttributeType()) ? Date.class : Number.class);
                jasperDesign.addField(aggregateField);

                // Aggregation variable.
                if (isDateOrTimeAttribute((column.getAttributeType()))) {
                    final JRDesignVariable minDateVariable = new JRDesignVariable();
                    minDateVariable.setName(String.format("%s_minDate", column.getName()));
                    minDateVariable.setValueClass(Date.class);
                    minDateVariable.setCalculation(CalculationEnum.LOWEST);
                    final JRDesignExpression minDateExpression = new JRDesignExpression();
                    minDateExpression.setText("$F{" + column.getName() + "}");
                    minDateVariable.setExpression(minDateExpression);
                    jasperDesign.addVariable(minDateVariable);

                    final JRDesignVariable maxDateVariable = new JRDesignVariable();
                    maxDateVariable.setName(String.format("%s_maxDate", column.getName()));
                    maxDateVariable.setValueClass(Date.class);
                    maxDateVariable.setCalculation(CalculationEnum.HIGHEST);
                    final JRDesignExpression maxDateExpression = new JRDesignExpression();
                    maxDateExpression.setText("$F{" + column.getName() + "}");
                    maxDateVariable.setExpression(maxDateExpression);
                    jasperDesign.addVariable(maxDateVariable);
                } else {
                    final JRDesignVariable columnSummaryVariable = new JRDesignVariable();
                    columnSummaryVariable.setName(String.format("%s_summary", column.getName()));

                    columnSummaryVariable.setValueClass(Number.class);
                    columnSummaryVariable.setCalculation(CalculationEnum.SUM);
                    columnSummaryVariable.setResetType(ResetTypeEnum.REPORT);
                    final JRDesignExpression variableExpression = new JRDesignExpression();
                    variableExpression.setText(String.format("$F{%s}", column.getName()));
                    columnSummaryVariable.setExpression(variableExpression);
                    jasperDesign.addVariable(columnSummaryVariable);
                }
            }

            HorizontalTextAlignEnum alignment = null;
            switch (column.getAlignment()) {
                case left:
                    alignment = HorizontalTextAlignEnum.LEFT;
                    break;
                case center:
                    alignment = HorizontalTextAlignEnum.CENTER;
                    break;
                case right:
                    alignment = HorizontalTextAlignEnum.RIGHT;
                    break;
                default:
            }

            // Detail
            if (ReportDesignParameters.ReportStyle.tabular.equals(reportDesignParameters.getReportStyle())) {
                // Column Header
                staticText = new JRDesignStaticText();
                staticText.setMode(ModeEnum.OPAQUE);
                staticText.setX(xPos);
                staticText.setY(0);
                staticText.setWidth(wideStaticTexts ? 1000 : column.getWidth());
                staticText.setHeight(18);
                staticText.setHorizontalTextAlign(HorizontalTextAlignEnum.CENTER);
                staticText.setForecolor(Color.white);
                staticText.setBackcolor(new Color(0x99, 0x99, 0x99));
                staticText.setFontSize(FONT_TWELVE);
                staticText.setText(column.getTitle());
                columnHeaderBand.addElement(staticText);

                // Value
                textField = new JRDesignTextField();
                textField.setBlankWhenNull(true);
                textField.setX(xPos);
                textField.setY(0);
                textField.setWidth(column.getWidth());
                textField.setHeight(20);
                textField.setHorizontalTextAlign(alignment);
                textField.setFontSize(FONT_TWELVE);
                textField.setStretchWithOverflow(true);
                textField.setStretchType(StretchTypeEnum.ELEMENT_GROUP_HEIGHT);
                expression = new JRDesignExpression();
                expression.setText("$F{" + column.getName() + "_display}");
                textField.setExpression(expression);
                detailBand.addElement(textField);

                // Column totals
                if (isAggregatableAttribute(column.getAttributeType())) {
                    textField = new JRDesignTextField();
                    textField.setBlankWhenNull(true);
                    textField.setX(xPos);
                    textField.setY(0);
                    textField.setWidth(column.getWidth());
                    textField.setHeight(20);
                    textField.setHorizontalTextAlign(alignment);
                    textField.setFontSize(FONT_TWELVE);
                    textField.setStretchWithOverflow(true);
                    textField.setStretchType(StretchTypeEnum.ELEMENT_GROUP_HEIGHT);
                    expression = new JRDesignExpression();
                    if (isDateOrTimeAttribute(column.getAttributeType())) {
                        expression.setText(String.format("(($V{%s_minDate} == null) || ($V{%s_maxDate} == null)) ? \"\" : org.skyve.impl.jasperreports.SkyveDataSource.getFormattedValue($F{USER}, $F{THIS}, \"%s\", $V{%s_minDate}) + \" - \" + org.skyve.impl.jasperreports.SkyveDataSource.getFormattedValue($F{USER}, $F{THIS}, \"%s\", $V{%s_maxDate})",
                        									column.getName(),
                        									column.getName(),
                        									column.getName(),
                        									column.getName(),
                                                            column.getName(),
                        									column.getName()));
                    } else {
                        expression.setText(String.format("org.skyve.impl.jasperreports.SkyveDataSource.getFormattedValue($F{USER}, $F{THIS}, \"%s\", $V{%s_summary})", column.getName(), column.getName()));
                    }
                    textField.setExpression(expression);
                    summaryBand.addElement(textField);
                }
            }
            else {
                // Label
                staticText = new JRDesignStaticText();
                staticText.setX(0);
                staticText.setY(yPos);
                staticText.setWidth(wideStaticTexts ? 1000 : columnarLabelWidth);
                staticText.setHeight(20);
                staticText.setFontSize(FONT_TWELVE);
                staticText.setStretchType(StretchTypeEnum.ELEMENT_GROUP_HEIGHT);
                staticText.setItalic(true);
                staticText.setText(column.getTitle());
                detailBand.addElement(staticText);

                // Value
                textField = new JRDesignTextField();
                textField.setBlankWhenNull(true);
                textField.setX(150);
                textField.setY(yPos);
                textField.setWidth(reportColumnWidth - columnarLabelWidth);
                textField.setHeight(20);
                textField.setHorizontalTextAlign(HorizontalTextAlignEnum.LEFT);
                textField.setFontSize(FONT_TWELVE);
                textField.setStretchWithOverflow(true);
                textField.setStretchType(StretchTypeEnum.ELEMENT_GROUP_HEIGHT);
                expression = new JRDesignExpression();
                expression.setText("$F{" + column.getName() + "_display}");
                textField.setExpression(expression);
                detailBand.addElement(textField);
            }

            if (ReportDesignParameters.ReportStyle.tabular.equals(reportDesignParameters.getReportStyle())) {
                xPos += column.getWidth() + (reportDesignParameters.isPretty() ? 5 : 0);
            }
            else {
                yPos += 20;
            }
        }

        // Background

        band = new JRDesignBand();
        jasperDesign.setBackground(band);

        // Title
        band = new JRDesignBand();
        if (reportDesignParameters.isPretty()) {
            band.setHeight(58);
            line = new JRDesignLine();
            line.setX(0);
            line.setY(8);
            line.setWidth(reportColumnWidth);
            line.setHeight(1);
            band.addElement(line);
            textField = new JRDesignTextField();
            textField.setBlankWhenNull(true);
            textField.setX(0);
            textField.setY(13);
            textField.setWidth(reportColumnWidth);
            textField.setHeight(35);
            textField.setHorizontalTextAlign(HorizontalTextAlignEnum.CENTER);
            textField.setFontSize(FONT_TWENTY_SIX);
            textField.setBold(true);
            expression = new JRDesignExpression();
            expression.setText("$P{TITLE}");
            textField.setExpression(expression);
            band.addElement(textField);
            line = new JRDesignLine();
            line.setPositionType(PositionTypeEnum.FIX_RELATIVE_TO_BOTTOM);
            line.setX(0);
            line.setY(51);
            line.setWidth(reportColumnWidth);
            line.setHeight(1);
            band.addElement(line);

            if (reportDesignParameters.isIncludeCustomerLogo()) {
                addCustomerLogo(band);
            }
        }
        jasperDesign.setTitle(band);

        // Page header
        band = new JRDesignBand();
        jasperDesign.setPageHeader(band);

        // Column header
        jasperDesign.setColumnHeader(columnHeaderBand);

        // Detail
        ((JRDesignSection) jasperDesign.getDetailSection()).addBand(detailBand);

        // Column footer
        jasperDesign.setColumnFooter(columnFooterBand);

        // Page footer
        band = new JRDesignBand();

        if (reportDesignParameters.isPaginated()) {
            band.setHeight(26);

            // Current time
            textField = new JRDesignTextField();
            textField.setEvaluationTime(EvaluationTimeEnum.REPORT);
            textField.setPattern("");
            textField.setBlankWhenNull(false);
            textField.setX(30);
            textField.setY(6);
            textField.setWidth(209);
            textField.setHeight(19);
            textField.setForecolor(Color.black);
            textField.setBackcolor(Color.white);
            textField.setFontSize(FONT_TEN);
            expression = new JRDesignExpression();
            expression.setText("new Date()");
            textField.setExpression(expression);
            band.addElement(textField);

            // Page number of
            textField = new JRDesignTextField();
            textField.setPattern("");
            textField.setBlankWhenNull(false);
            textField.setX(reportColumnWidth - 200);
            textField.setY(6);
            textField.setWidth(155);
            textField.setHeight(19);
            textField.setForecolor(Color.black);
            textField.setBackcolor(Color.white);
            textField.setHorizontalTextAlign(HorizontalTextAlignEnum.RIGHT);
            textField.setFontSize(FONT_TEN);
            expression = new JRDesignExpression();
            expression.setText("\"Page \" + $V{PAGE_NUMBER} + \" of\"");
            textField.setExpression(expression);
            band.addElement(textField);

            // Total pages
            textField = new JRDesignTextField();
            textField.setEvaluationTime(EvaluationTimeEnum.REPORT);
            textField.setPattern("");
            textField.setBlankWhenNull(false);
            textField.setX(reportColumnWidth - 40);
            textField.setY(6);
            textField.setWidth(40);
            textField.setHeight(19);
            textField.setForecolor(Color.black);
            textField.setBackcolor(Color.white);
            textField.setFontSize(FONT_TEN);
            expression = new JRDesignExpression();
            expression.setText("$V{PAGE_NUMBER}");
            textField.setExpression(expression);
            band.addElement(textField);
        }
        jasperDesign.setPageFooter(band);

        // Summary
        summaryBand.setHeight(40);

        line = new JRDesignLine();
        line.setX(0);
        line.setY(0);
        line.setWidth(reportColumnWidth);
        line.setHeight(1);
        summaryBand.addElement(line);
        line = new JRDesignLine();
        line.setX(0);
        line.setY(20);
        line.setWidth(reportColumnWidth);
        line.setHeight(1);
        line.setPositionType(PositionTypeEnum.FLOAT);
        summaryBand.addElement(line);
        line = new JRDesignLine();
        line.setX(0);
        line.setY(39);
        line.setWidth(reportColumnWidth);
        line.setHeight(1);
        line.setPositionType(PositionTypeEnum.FLOAT);
        summaryBand.addElement(line);

        textField = new JRDesignTextField();
        textField.setBlankWhenNull(true);
        textField.setX(0);
        textField.setY(20);
        textField.setWidth(reportColumnWidth);
        textField.setHeight(20);
        textField.setHorizontalTextAlign(HorizontalTextAlignEnum.LEFT);
        textField.setFontSize(FONT_TWELVE);
        textField.setStretchWithOverflow(true);
        textField.setStretchType(StretchTypeEnum.ELEMENT_GROUP_HEIGHT);
        textField.setEvaluationTime(EvaluationTimeEnum.REPORT);
        expression = new JRDesignExpression();
        expression.setText("\"Number of records: \" + $V{REPORT_COUNT}");
        textField.setExpression(expression);
        textField.setPositionType(PositionTypeEnum.FLOAT);
        summaryBand.addElement(textField);
		if (reportDesignParameters.isShowSummary() == true) {
            jasperDesign.setSummary(summaryBand);
        }

        rendered = true;
        return JRXmlWriter.writeReport(jasperDesign, "UTF-8");
    }

    protected void addParameters(DesignSpecification design) throws JRException {
        for (ReportParameter reportParameter : design.getParameters()) {
            final JRDesignParameter parameter = new JRDesignParameter();
            parameter.setName(reportParameter.getName());
            parameter.setValueClassName(reportParameter.getTypeClass());
            parameter.setForPrompting(false);

            final JRDesignExpression expression = new JRDesignExpression();
            expression.setText(reportParameter.getDefaultValueExpression());

            parameter.setDefaultValueExpression(expression);

            jasperDesign.addParameter(parameter);
        }

        final JRDesignParameter designSpecParameter = new JRDesignParameter();
        designSpecParameter.setName(DESIGN_SPEC_PARAMETER_NAME);
        designSpecParameter.setValueClassName(DesignSpecification.class.getName());
        designSpecParameter.setForPrompting(false);

        jasperDesign.addParameter(designSpecParameter);
    }

	@SuppressWarnings("boxing")
	protected void configureReportProperties(DesignSpecification design) {
        jasperDesign.setName(design.getName());
        jasperDesign.setPageWidth(design.getWidth());
        jasperDesign.setPageHeight(design.getHeight());
        jasperDesign.setColumnWidth(design.getColumnWidth());
        jasperDesign.setLeftMargin(design.getLeftMargin());
        jasperDesign.setRightMargin(design.getRightMargin());
        jasperDesign.setTopMargin(design.getTopMargin());
        jasperDesign.setBottomMargin(design.getBottomMargin());
    }

    protected List<String> getImports() {
        return imports;
    }

    private void addImports() {
        getImports().forEach(jasperDesign::addImport);
    }

    protected Map<String, String> getProperties() {
        return properties;
    }

    private void addProperties() {
        getProperties().forEach(jasperDesign::setProperty);
    }

    protected void addQuery(DesignSpecification designSpecification) {
        final JRDesignQuery query = new JRDesignQuery();
        if (DesignSpecification.Mode.bean.equals(designSpecification.getMode())) {
            query.setLanguage("document");

            if (DesignSpecification.ReportType.report.equals(designSpecification.getReportType())) {
                query.addTextChunk(designSpecification.getModuleName().concat(".").concat(designSpecification.getDocumentName()));
            }
        } else {
            final Customer customer = CORE.getPersistence().getUser().getCustomer();
            final Module module = customer.getModule(designSpecification.getModuleName());
            final Document document = module.getDocument(customer, designSpecification.getDocumentName());

            String sqlName = null;
            if (document.getExtends() != null) {
                Document extDocument = null;
                if (document.getExtends().getDocumentName() != null && Persistent.ExtensionStrategy.joined.equals(document.getPersistent().getStrategy())) {
                    extDocument = module.getDocument(customer, document.getExtends().getDocumentName());
                    designSpecification.setAlias(designSpecification.getAlias() + 1);
                }
				if (extDocument != null) {
					sqlName = extDocument.getName();
				}
            }

            StringBuilder sql = new StringBuilder();
            for (ReportField f : designSpecification.getFields()) {
                if (!Boolean.TRUE.equals(f.getCollection())) {
                    if (sql.length() > 0) {
                        sql.append("\n ,");
                    }
                    sql.append((f.getNameSql() == null ? sqlName + "." + f.getName() : f.getNameSql()));
                }
            }

            if (DesignSpecification.ReportType.report.equals(designSpecification.getReportType())) {
                // not implemented
            } else {
                // nothing
            }

            query.addTextChunk("select ".concat(sql.toString()).concat(" from ").concat(getPersistentFromDocument(document).concat(" a")));

            // joins
            if (designSpecification.getJoins() != null) {
                for (String k : designSpecification.getJoins().keySet()) {
                    query.addTextChunk("\n".concat(designSpecification.getJoins().get(k)));
                }
            }

            if (DesignSpecification.ReportType.report.equals(designSpecification.getReportType())) {
                query.addTextChunk("\n where ".concat("a").concat(".bizId = $P{ID}"));
            } else if (DesignSpecification.ReportType.subreport.equals(designSpecification.getReportType())) {
                Util.LOGGER.info("SUBREPORT " + designSpecification.getName() + " IS " + designSpecification.getCollectionType().name());

                // join to either parent or joiner table
                if (Collection.CollectionType.child.equals(designSpecification.getCollectionType())) {
                    // child
                    query.addTextChunk("\n where ".concat("a").concat(".parent_id = $P{ID}"));
                } else {
                    // joiner
                    // TODO - join correctly for aggregated collections - probably obviated by the getJoins() above
                    query.addTextChunk("\n join ".concat(designSpecification.getParentReportPersistentName()).concat("_").concat(designSpecification.getField().getName()));
                    query.addTextChunk(" ".concat(designSpecification.getField().getName()).concat(" on ").concat(designSpecification.getField().getName()).concat(".element_id = "));
                    query.addTextChunk(" ".concat(document.getName()).concat(".bizId"));
                    query.addTextChunk("\n  where ".concat(designSpecification.getField().getName()).concat(".owner_id = $P{ID}"));
                }
            }
        }

        jasperDesign.setQuery(query);
    }

    private void addFields(DesignSpecification designSpecification) throws JRException {
        for (ReportField reportField : designSpecification.getFields()) {
            final JRField field = createField(reportField);
            if (field != null) {
                jasperDesign.addField(field);
            }
        }
    }

    protected JRField createField(ReportField reportField) {
        final JRDesignField jrField = new JRDesignField();

		if (reportField.getParent() != null) {
			if (!(Boolean.TRUE.equals(reportField.getCollection()) && Mode.sql.equals(reportField.getParent().getMode()))) {
                jrField.setName(reportField.getName());
                jrField.setValueClassName(reportField.getTypeClass());

                if (!Mode.sql.equals(reportField.getParent().getMode()) &&
                        String.class.getTypeName().equals(reportField.getTypeClass())) {
                    // Setting the field description will cause the SkyveDataSource to return the display value (String) for the field.
                    jrField.setDescription(reportField.getName());
                }

                return jrField;
            }
        }

        return null;
    }

    private void addVariables(DesignSpecification designSpecification) throws JRException {
        for (ReportVariable reportVariable : designSpecification.getVariables()) {
            jasperDesign.addVariable(createVariable(reportVariable));
        }
    }

    protected JRDesignVariable createVariable(ReportVariable reportVariable) {
        final JRDesignVariable jrVariable = new JRDesignVariable();

        jrVariable.setName(reportVariable.getName());
        jrVariable.setValueClassName(reportVariable.getTypeClass());
        jrVariable.setIncrementType(getIncrementType());
        jrVariable.setExpression(createVariableExpression(reportVariable));
        jrVariable.setInitialValueExpression(createInitialValueVariableExpression(reportVariable));

        return jrVariable;
    }

    protected IncrementTypeEnum getIncrementType() {
        return IncrementTypeEnum.COLUMN;
    }

    protected JRExpression createVariableExpression(ReportVariable reportVariable) {
        final JRDesignExpression jrExpression = new JRDesignExpression();

        jrExpression.setText(String.format("$V{%s}.add($F{%s})", reportVariable.getTypeClass(), reportVariable.getName()));

        return jrExpression;
    }

    protected JRExpression createInitialValueVariableExpression(ReportVariable reportVariable) {
        final JRDesignExpression jrExpression = new JRDesignExpression();

        jrExpression.setText(String.format("new %s(0)", reportVariable.getTypeClass()));

        return jrExpression;
    }

    private void addBands(DesignSpecification designSpecification) {
        getBandByType(designSpecification, BandType.background).ifPresent(jasperDesign::setBackground);
        getBandByType(designSpecification, BandType.title).ifPresent(titleBand -> {
            if (designSpecification.isIncludeCustomerLogo()) {
                addCustomerLogo(titleBand);
            }
            jasperDesign.setTitle(titleBand);
        });
        getBandByType(designSpecification, BandType.pageHeader).ifPresent(jasperDesign::setPageHeader);
        getBandByType(designSpecification, BandType.columnHeader).ifPresent(jasperDesign::setColumnHeader);
        getDetailBands(designSpecification).forEach(this::addDetailBand);
        getBandByType(designSpecification, BandType.columnFooter).ifPresent(jasperDesign::setColumnFooter);
        getBandByType(designSpecification, BandType.pageFooter).ifPresent(jasperDesign::setPageFooter);
        getBandByType(designSpecification, BandType.lastPageFooter).ifPresent(jasperDesign::setLastPageFooter);
        getBandByType(designSpecification, BandType.summary).ifPresent(jasperDesign::setSummary);
        getBandByType(designSpecification, BandType.noData).ifPresent(jasperDesign::setNoData);
    }

    protected void addCustomerLogo(JRBand titleBand) {
        final int logoWidth = 200;
        final JRDesignImage logoImage = new JRDesignImage(null);
        logoImage.setWidth(logoWidth);
        logoImage.setHeight(titleBand.getHeight());
        logoImage.setScaleImage(ScaleImageEnum.RETAIN_SHAPE);
        final JRDesignExpression expression = new JRDesignExpression();
        expression.setText(String.format("org.skyve.impl.generate.jasperreports.ContentImageForReport.customerLogo(%d, %d)", logoWidth, titleBand.getHeight()));
        logoImage.setExpression(expression);
        ((JRDesignBand) titleBand).addElement(logoImage);
    }

    private Optional<JRBand> getBandByType(DesignSpecification designSpecification, BandType bandType) {
        if (BandType.detail.equals(bandType)) {
            throw new IllegalArgumentException("Invalid use of method, please use getDetailBands().");
        }

        return designSpecification.getBands().stream()
                .filter(b -> bandType.equals(b.getBandType()))
                .findAny()
                .map(this::createBand);
    }

    private List<JRBand> getDetailBands(DesignSpecification designSpecification) {
        return designSpecification.getBands().stream()
                .filter(b -> BandType.detail.equals(b.getBandType()))
                .map(this::createBand)
                .collect(Collectors.toList());
    }

    protected void addDetailBand(JRBand band) {
        ((JRDesignSection) jasperDesign.getDetailSection()).addBand(band);
    }

    protected JRBand createBand(ReportBand reportBand) {
        if (reportBand.getElements().isEmpty()) {
            return null;
        }

        final JRDesignBand jrBand = new JRDesignBand();

        jrBand.setHeight(Optional.ofNullable(reportBand.getHeight()).orElse(0));

        Optional.ofNullable(reportBand.getSplitType())
                .map(this::getSplitType)
                .ifPresent(jrBand::setSplitType);

        Optional.ofNullable(reportBand.getInvisibleConditionName())
                .map(this::getPrintWhenExpressionFromInvisibleCondition)
                .ifPresent(jrBand::setPrintWhenExpression);

        reportBand.getElements().stream()
                .map(this::createElement)
                .filter(Objects::nonNull)
                .forEach(jrBand::addElement);

        return jrBand;
    }

    private SplitTypeEnum getSplitType(ReportBand.SplitType splitType) {
        return SplitTypeEnum.getByName(splitType.toString());
    }

    private JRExpression getPrintWhenExpressionFromInvisibleCondition(String invisibleConditionName) {
        final JRDesignExpression jrExpression = new JRDesignExpression();

        final String expressionText;
        if (StringUtils.isBlank(invisibleConditionName)) {
            expressionText = "";
        } else {
            if (Mode.bean.equals(designSpecification.getMode())) {
                expressionText = String.format("$F{THIS}.%s()", flipCondition(invisibleConditionName));
            } else {
                final String subExpression =
                        String.format("org.skyve.impl.generate.jasperreports.BeanForReport.evaluateCondition(\"%s\", \"%s\", $P{ID}, \"%s\")",
                                designSpecification.getModuleName(),
                                designSpecification.getDocumentName(),
                                rawConditionName(invisibleConditionName));
                if (!invisibleConditionName.startsWith("not")) {
                    expressionText = "!".concat(subExpression);
                } else {
                    expressionText = subExpression;
                }
            }
        }

        jrExpression.setText(expressionText);

        return jrExpression;
    }

    private JRElement createElement(ReportElement reportElement) {
        switch (reportElement.getElementType()) {
            case staticText:
                final JRDesignStaticText staticTextElement = new JRDesignStaticText();

                configureCommonTextFieldProperties(staticTextElement, reportElement);

                staticTextElement.setText(Optional.ofNullable(reportElement.getElementValue()).orElse("\"\""));

                wrapInBox(reportElement, staticTextElement.getLineBox());

                return staticTextElement;
            case textField:
                final JRDesignTextField textElement = new JRDesignTextField();

                configureCommonTextFieldProperties(textElement, reportElement);

                Optional.ofNullable(reportElement.getDynamicFlow())
                        .ifPresent(textElement::setStretchWithOverflow);
                Optional.ofNullable(reportElement.getEvaluationTime())
                        .map(e -> EvaluationTimeEnum.getByName(e.toString()))
                        .ifPresent(textElement::setEvaluationTime);
                textElement.setBlankWhenNull(true);
                textElement.setExpression(createTextElementExpression(reportElement));

                wrapInBox(reportElement, textElement.getLineBox());

                return textElement;
            case checkBox:
                // Not implemented.
                return null;
            case combo:
                // Not implemented.
                return null;
            case colourPicker:
                // Not implemented.
                return null;
            case contentImage:
            case staticImage:
            case dynamicImage:
                final JRDesignImage jrDynamicImage = new JRDesignImage(null);

                configureDimensions(jrDynamicImage, reportElement);
                jrDynamicImage.setExpression(createImageElementExpression(reportElement));

                wrapInBox(reportElement, jrDynamicImage.getLineBox());

                return jrDynamicImage;
            case geometry:
                // Not implemented.
                return null;
            case line:
                final JRDesignLine jrLine = new JRDesignLine();

                jrLine.setKey(reportElement.getElementType().toString() + reportElement.getOrdinal());
                configureDimensions(jrLine, reportElement);
                jrLine.setForecolor(Color.getColor("#404040"));
                jrLine.getLinePen().setLineStyle(LineStyleEnum.SOLID);
                jrLine.getLinePen().setLineWidth(Optional.ofNullable(designSpecification.getDefaultLineWidth()).map(Decimal2::floatValue).orElse(1.0f));

                return jrLine;
            case radio:
                // Not implemented.
                return null;
            case richTextField:
                // Not implemented.
                return null;
            case html:
                // Not implemented.
                return null;
            case subreport:
                final JRDesignSubreport jrSubreport = new JRDesignSubreport(null);

                configureDimensions(jrSubreport, reportElement);

                if (Mode.bean.equals(designSpecification.getMode())) {
                    final JRDesignExpression dataSourceExpression = new JRDesignExpression();
                    dataSourceExpression.setText(String.format("new net.sf.jasperreports.engine.data.JRBeanCollectionDataSource($F{%s})", reportElement.getName()));
                    jrSubreport.setDataSourceExpression(dataSourceExpression);
                } else {
                    final JRDesignSubreportParameter subReportParameter = new JRDesignSubreportParameter();
                    subReportParameter.setName("ID");

                    final JRDesignExpression parameterExpression = new JRDesignExpression();
                    parameterExpression.setText("$P{ID}");
                    subReportParameter.setExpression(parameterExpression);
                    try {
                        jrSubreport.addParameter(subReportParameter);
                    } catch (JRException e) {
                        e.printStackTrace();
                    }

                    final JRDesignExpression connectionExpression = new JRDesignExpression();
                    connectionExpression.setText("$P{REPORT_CONNECTION}");
                    jrSubreport.setConnectionExpression(connectionExpression);
                }

                final JRDesignExpression subReportExpression = new JRDesignExpression();
                subReportExpression.setText(String.format("org.skyve.impl.generate.jasperreports.JasperReportRenderer.getSubReport($P{%s}, \"%s\")", DESIGN_SPEC_PARAMETER_NAME, reportElement.getReportFileName()));
                jrSubreport.setExpression(subReportExpression);

                return jrSubreport;
            case slider:
                // Not implemented.
                return null;
            case spinner:
                // Not implemented.
                return null;
            case border:
                final JRDesignRectangle jrRectangle = new JRDesignRectangle();

                jrRectangle.setStretchType(StretchTypeEnum.CONTAINER_HEIGHT);
                configureDimensions(jrRectangle, reportElement);
                Optional.ofNullable(designSpecification.getDefaultLineColour()).map(Color::getColor).ifPresent(jrRectangle::setForecolor);
                jrRectangle.getLinePen().setLineStyle(LineStyleEnum.SOLID);
                jrRectangle.getLinePen().setLineWidth(Optional.ofNullable(designSpecification.getDefaultLineWidth()).map(Decimal2::floatValue).orElse(1.0f));

                return jrRectangle;
            default:
                return null;
        }
    }

    private void wrapInBox(ReportElement reportElement, JRLineBox box) {
        box.setTopPadding(reportElement.getTopPadding());
        box.setBottomPadding(reportElement.getBottomPadding());
        box.setLeftPadding(reportElement.getLeftPadding());
        box.setRightPadding(reportElement.getRightPadding());

        final JRBoxPen boxPen = box.getPen();
        if (Boolean.TRUE.equals(reportElement.getElementBorder())) {
            final Float lineWidth = Optional.ofNullable(reportElement.getBorderLineWidth()).map(Decimal2::floatValue).orElse(1f);
            boxPen.setLineWidth(lineWidth);
            boxPen.setLineColor(Color.getColor(reportElement.getBorderColour()));
            if (Boolean.TRUE.equals(reportElement.getBorderTop())) {
                box.getTopPen().setLineStyle(LineStyleEnum.SOLID);
                box.getTopPen().setLineWidth(lineWidth);
                box.getTopPen().setLineColor(Color.getColor(reportElement.getBorderColour()));
            }
            if (Boolean.TRUE.equals(reportElement.getBorderLeft())) {
                box.getLeftPen().setLineStyle(LineStyleEnum.SOLID);
                box.getLeftPen().setLineWidth(lineWidth);
                box.getLeftPen().setLineColor(Color.getColor(reportElement.getBorderColour()));
            }
            if (Boolean.TRUE.equals(reportElement.getBorderBottom())) {
                box.getBottomPen().setLineStyle(LineStyleEnum.SOLID);
                box.getBottomPen().setLineWidth(lineWidth);
                box.getBottomPen().setLineColor(Color.getColor(reportElement.getBorderColour()));
            }
            if (Boolean.TRUE.equals(reportElement.getBorderRight())) {
                box.getRightPen().setLineStyle(LineStyleEnum.SOLID);
                box.getRightPen().setLineWidth(lineWidth);
                box.getRightPen().setLineColor(Color.getColor(reportElement.getBorderColour()));
            }
        } else {
            box.getTopPen().setLineStyle(LineStyleEnum.SOLID);
            box.getLeftPen().setLineStyle(LineStyleEnum.SOLID);
            box.getBottomPen().setLineStyle(LineStyleEnum.SOLID);
            box.getRightPen().setLineStyle(LineStyleEnum.SOLID);
        }
    }

    protected void configureCommonTextFieldProperties(JRDesignTextElement textElement, ReportElement reportElement) {
        textElement.setKey(String.format("%s_%s", reportElement.getElementType().toString(),
                (reportElement.getOrdinal() == null ? "1" : reportElement.getOrdinal())));
        textElement.setStretchType(StretchTypeEnum.ELEMENT_GROUP_HEIGHT);
        textElement.setForecolor(Color.getColor(Optional.ofNullable(reportElement.getElementForeColour()).orElse("#404040")));

        if (reportElement.getElementBackColour() != null) {
            textElement.setMode(ModeEnum.OPAQUE);
            textElement.setBackcolor(Color.getColor(reportElement.getElementBackColour()));
        } else {
            textElement.setMode(ModeEnum.TRANSPARENT);
            textElement.setBackcolor(Color.getColor("#FFFFFF"));
        }

        configureDimensions(textElement, reportElement);

        Optional.ofNullable(reportElement.getInvisibleConditionName())
                .map(this::getPrintWhenExpressionFromInvisibleCondition)
                .ifPresent(textElement::setPrintWhenExpression);

        Optional.ofNullable(reportElement.getElementAlignment())
                .map(a -> HorizontalTextAlignEnum.getByName(a.toString()))
                .ifPresent(textElement::setHorizontalTextAlign);
        textElement.setVerticalTextAlign(VerticalTextAlignEnum.TOP);
        textElement.setRotation(RotationEnum.NONE);

        textElement.setFontName(reportElement.getElementFontName());

        if (BandType.title.equals(reportElement.getParent().getBandType())) {
            if (designSpecification.getTitleFontSize() != null) {
                textElement.setFontSize(designSpecification.getTitleFontSize());
            } else {
                textElement.setFontSize(16f);
            }
        } else {
            if (reportElement.getElementFontSize() != null) {
                textElement.setFontSize(reportElement.getElementFontSize());
            } else {
                textElement.setFontSize(12f);
            }
        }
		textElement.setBold(Optional.ofNullable(reportElement.getElementBold()).orElse(Boolean.FALSE));
		textElement.setItalic(Optional.ofNullable(reportElement.getElementItalic()).orElse(Boolean.FALSE));
        textElement.setUnderline(false);
        textElement.setStrikeThrough(false);
    }

	@SuppressWarnings("boxing")
	protected void configureDimensions(JRDesignElement jrDesignElement, ReportElement reportElement) {
        jrDesignElement.setX(Optional.ofNullable(reportElement.getElementLeft()).orElse(0));
        jrDesignElement.setY(Optional.ofNullable(reportElement.getElementTop()).orElse(0));
        jrDesignElement.setHeight(Optional.ofNullable(reportElement.getElementHeight()).orElse(0));
        jrDesignElement.setWidth(Optional.ofNullable(reportElement.getElementWidth()).orElse(0));
    }

    protected JRExpression createTextElementExpression(ReportElement reportElement) {
        final JRDesignExpression jrExpression = new JRDesignExpression();

        if (reportElement.getElementValue() == null) {
            jrExpression.setText("\"\"");
        } else {
            jrExpression.setText(reportElement.getElementValue());
        }

        return jrExpression;
    }

    protected JRExpression createImageElementExpression(ReportElement reportElement) {
        final JRDesignExpression jrExpression = new JRDesignExpression();

        final String expression;
        switch (reportElement.getElementType()) {
            case contentImage:
                final StringBuilder contentExpressionBuilder = new StringBuilder();
                contentExpressionBuilder.append("org.skyve.impl.generate.jasperreports.ContentImageForReport.image(");
                contentExpressionBuilder.append("$F{").append(reportElement.getElementValue()).append("}, ");

                if (reportElement.getElementWidth() != null) {
                    contentExpressionBuilder.append(reportElement.getElementWidth().toString()).append(", ");
                    contentExpressionBuilder.append(reportElement.getElementWidth().toString());
                }
                contentExpressionBuilder.append(")");
                expression = contentExpressionBuilder.toString();
                break;
            case dynamicImage:
                final StringBuilder dynamicExpressionBuilder = new StringBuilder();
                dynamicExpressionBuilder.append("new modules").append('.');
                dynamicExpressionBuilder.append(reportElement.getParent().getParent().getModuleName()).append('.');
                dynamicExpressionBuilder.append(reportElement.getParent().getParent().getDocumentName()).append('.');
                dynamicExpressionBuilder.append("images").append('.');
                dynamicExpressionBuilder.append(reportElement.getElementValue()).append("().getImage(");
                if (Mode.bean.equals(reportElement.getParent().getParent().getMode())) {
                    dynamicExpressionBuilder.append("$F{THIS}, ");
                } else {
                    // sql
                    dynamicExpressionBuilder.append("org.skyve.impl.generate.jasperreports.BeanForReport.getBean(");
                    dynamicExpressionBuilder.append(reportElement.getParent().getParent().getModuleName()).append(", ");
                    dynamicExpressionBuilder.append(reportElement.getParent().getParent().getDocumentName()).append(", ");
                    dynamicExpressionBuilder.append("$P{ID})");
                }
                if (reportElement.getElementWidth() != null) {
                    dynamicExpressionBuilder.append(reportElement.getElementWidth().toString()).append(", ");
                    dynamicExpressionBuilder.append(reportElement.getElementWidth().toString()).append(", ");
                }
                // TODO vertical sizing - for now assume square based on pixelWidth
                dynamicExpressionBuilder.append(" (org.skyve.impl.generate.jasperreports.BeanForReport.getUser())");
                expression = dynamicExpressionBuilder.toString();
                break;
            case staticImage:
                expression = reportElement.getElementValue();
                break;
            default:
                throw new IllegalArgumentException(String.format("Invalid element type %s.",
                        reportElement.getElementType()));
        }

        jrExpression.setText(expression);

        return jrExpression;
    }

    // TODO: Move to interface
    public static String getPersistentFromDocument(Document document) {

        StringBuilder sb = new StringBuilder();

        if (document.getPersistent() != null) {
            sb.append(document.getPersistent().getCatalog() == null ? "" : document.getPersistent().getCatalog() + ".");
            sb.append(document.getPersistent().getSchema() == null ? "" : document.getPersistent().getSchema() + ".");
            sb.append(document.getPersistent().getName());
        }

        return sb.toString();
    }

    public static String flipCondition(String conditionName) {
        String result = null;
        if (conditionName != null) {
            if (conditionName.startsWith("not")) {
                result = "is" + rawConditionName(conditionName).substring(0, 1).toUpperCase() + rawConditionName(conditionName).substring(1);
            } else {
                result = "not" + conditionName.substring(0, 1).toUpperCase() + conditionName.substring(1);
            }
        }
        return result;
    }

    public static String rawConditionName(String conditionName) {
        if (conditionName.startsWith("not")) {
            return conditionName.substring(3, 4).toLowerCase() + conditionName.substring(4);
        } else {
            return conditionName.substring(0, 1).toLowerCase() + conditionName.substring(1);
        }
    }

    private ReportDesignParameters convertDesign(DesignSpecification designSpecification) {
        final ReportDesignParameters reportDesignParameters = new ReportDesignParameters();

        reportDesignParameters.setReportFormat(ReportFormat.pdf);
        reportDesignParameters.setReportStyle(ReportDesignParameters.ReportStyle.tabular);
		reportDesignParameters.setPageWidth(designSpecification.getWidth().intValue());
		reportDesignParameters.setPageHeight(designSpecification.getHeight().intValue());
        reportDesignParameters.setPaginated(true);
        reportDesignParameters.setPretty(true);
        reportDesignParameters.setTopMargin(20);
        reportDesignParameters.setBottomMargin(20);
        reportDesignParameters.setLeftMargin(20);
        reportDesignParameters.setRightMargin(20);

        final DocumentQueryListModel<Bean> listModel = getListModel(designSpecification);
        final Document drivingDocument = listModel.getDrivingDocument();
        for (MetaDataQueryColumn queryColumn : listModel.getColumns()) {
            ReportDesignParameters.ReportColumn reportColumn = new ReportDesignParameters.ReportColumn();
            reportColumn.setLine(1);
            reportColumn.setName(queryColumn.getBinding());
            reportColumn.setTitle(queryColumn.getBinding());
            reportColumn.setType("text");
			reportColumn.setWidth(queryColumn.getPixelWidth() != null ? queryColumn.getPixelWidth().intValue() : 100);
            String align = queryColumn.getAlignment() != null ? queryColumn.getAlignment().toAlignmentString() : null;
            if (align != null) {
                reportColumn.setAlignment(ReportDesignParameters.ColumnAlignment.valueOf(align));
            }
            else {
                reportColumn.setAlignment(ReportDesignParameters.ColumnAlignment.left);
            }
            if (drivingDocument != null && reportColumn.getName() != null) {
                final Attribute attribute = drivingDocument.getAttribute(reportColumn.getName());
                if (attribute != null) {
                    reportColumn.setAttributeType(attribute.getAttributeType());
                }
            }

            reportDesignParameters.getColumns().add(reportColumn);
        }

        return reportDesignParameters;
    }

    protected DocumentQueryListModel<Bean> getListModel(DesignSpecification designSpecification) {
        final Customer customer = CORE.getCustomer();
        final Module module = designSpecification.getModule();

        MetaDataQueryDefinition query = null;
        if (designSpecification.getQueryName() != null) {
            query = module.getMetaDataQuery(designSpecification.getQueryName());
        }
        if (query == null) {
            query = module.getDocumentDefaultQuery(customer, designSpecification.getDocumentName());
        }
        if (query == null) {
            throw new IllegalArgumentException("Design does not reference a valid query " + designSpecification.getQueryName());
        }

        final DocumentQueryListModel<Bean> queryModel = new DocumentQueryListModel<>();
        queryModel.setQuery(query);

        return queryModel;
    }

    protected boolean isAggregatableAttribute(AttributeType attributeType) {
        return attributeType == AttributeType.integer ||
                attributeType == AttributeType.longInteger ||
                attributeType == AttributeType.decimal2 ||
                attributeType == AttributeType.decimal5 ||
                attributeType == AttributeType.decimal10 ||
                attributeType == AttributeType.date ||
                attributeType == AttributeType.dateTime ||
                attributeType == AttributeType.time ||
                attributeType == AttributeType.timestamp;
    }

    protected boolean isDateOrTimeAttribute(AttributeType attributeType) {
        return attributeType == AttributeType.date ||
                attributeType == AttributeType.dateTime ||
                attributeType == AttributeType.time ||
                attributeType == AttributeType.timestamp;
    }
}
