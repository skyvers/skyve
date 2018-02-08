package org.skyve.impl.generate.jasperreports;

import java.awt.*;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;

import org.apache.commons.lang3.StringUtils;
import org.skyve.CORE;
import org.skyve.domain.types.Decimal2;
import org.skyve.impl.tools.jasperreports.SkyveDocumentExecuterFactory;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Persistent;
import org.skyve.metadata.model.document.Collection;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.util.Util;
import org.skyve.impl.generate.jasperreports.DesignSpecification.Mode;
import org.skyve.impl.generate.jasperreports.ReportBand.BandType;

import net.sf.jasperreports.engine.DefaultJasperReportsContext;
import net.sf.jasperreports.engine.JRBand;
import net.sf.jasperreports.engine.JRElement;
import net.sf.jasperreports.engine.JRException;
import net.sf.jasperreports.engine.JRExpression;
import net.sf.jasperreports.engine.JRField;
import net.sf.jasperreports.engine.JRLineBox;
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
import net.sf.jasperreports.engine.type.EvaluationTimeEnum;
import net.sf.jasperreports.engine.type.HorizontalTextAlignEnum;
import net.sf.jasperreports.engine.type.IncrementTypeEnum;
import net.sf.jasperreports.engine.type.LineStyleEnum;
import net.sf.jasperreports.engine.type.ModeEnum;
import net.sf.jasperreports.engine.type.RotationEnum;
import net.sf.jasperreports.engine.type.SplitTypeEnum;
import net.sf.jasperreports.engine.type.StretchTypeEnum;
import net.sf.jasperreports.engine.type.VerticalTextAlignEnum;
import net.sf.jasperreports.engine.xml.JRXmlWriter;

public class JasperReportRenderer {

    protected final JasperDesign jasperDesign;
    protected final DesignSpecification designSpecification;

    private static final Map<String, String> properties;
    private boolean rendered = false;

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
        this.designSpecification = designSpecification;
        jasperDesign = new JasperDesign();
    }

    public JasperReport getReport() throws Exception {
        if (!rendered) {
            renderDesign();
        }
        return JasperCompileManager.compileReport(jasperDesign);
    }

    public String renderDesign() throws Exception {
        if (rendered) {
            throw new IllegalStateException("Report has already been rendered.");
        }

        if (designSpecification.getModuleName() != null && designSpecification.getDocumentName() != null) {
            configureReportProperties(designSpecification);

            // support document queries
            JasperReportsContext jasperReportsContext = DefaultJasperReportsContext.getInstance();
            jasperReportsContext.setProperty(JRQueryExecuterFactory.QUERY_EXECUTER_FACTORY_PREFIX + "document", SkyveDocumentExecuterFactory.class.getCanonicalName());

            addProperties();
            addImports();
            addParameters(designSpecification);
            addQuery(designSpecification);
            addFields(designSpecification);
            addVariables(designSpecification);
            addBands(designSpecification);

            final String jrxml = JRXmlWriter.writeReport(jasperDesign, "UTF-8");
            rendered = true;
            return jrxml;
        }

        throw new IllegalArgumentException("Invalid module or document name.");
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
    }

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
                sqlName = extDocument.getName();
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

        if (!(Boolean.TRUE.equals(reportField.getCollection()) && Mode.sql.equals(reportField.getParent().getMode()))) {
            if (reportField.getParent() != null) {
                jrField.setName(reportField.getName());
                jrField.setValueClassName(reportField.getTypeClass());

                if (!Mode.sql.equals(reportField.getParent().getMode())) {
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
        getBandByType(designSpecification, BandType.title).ifPresent(jasperDesign::setTitle);
        getBandByType(designSpecification, BandType.pageHeader).ifPresent(jasperDesign::setPageHeader);
        getBandByType(designSpecification, BandType.columnHeader).ifPresent(jasperDesign::setColumnHeader);
        getDetailBands(designSpecification).forEach(this::addDetailBand);
        getBandByType(designSpecification, BandType.columnFooter).ifPresent(jasperDesign::setColumnFooter);
        getBandByType(designSpecification, BandType.pageFooter).ifPresent(jasperDesign::setPageFooter);
        getBandByType(designSpecification, BandType.lastPageFooter).ifPresent(jasperDesign::setLastPageFooter);
        getBandByType(designSpecification, BandType.summary).ifPresent(jasperDesign::setSummary);
        getBandByType(designSpecification, BandType.noData).ifPresent(jasperDesign::setNoData);
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
                        String.format("modules.design.reportgeneration.BeanForReport.evaluateCondition(\"%s\", \"%s\", $P{ID}, \"%s\")",
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
                subReportExpression.setText(String.format("$P{SUBREPORT_DIR} + \"%s.jasper\"", reportElement.getReportFileName()));
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
        textElement.setBold(Optional.ofNullable(reportElement.getElementBold()).orElse(false));
        textElement.setItalic(Optional.ofNullable(reportElement.getElementItalic()).orElse(false));
        textElement.setUnderline(false);
        textElement.setStrikeThrough(false);
    }

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
                contentExpressionBuilder.append("modules.design.reportgeneration.ContentImageForReport.image(");
                contentExpressionBuilder.append("$F{").append(reportElement.getElementValue()).append("}, ");

                if (reportElement.getElementWidth() != null) {
                    contentExpressionBuilder.append(reportElement.getElementWidth().toString()).append(", ");
                    contentExpressionBuilder.append(reportElement.getElementWidth().toString());
                }
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
                    dynamicExpressionBuilder.append("modules.design.reportgeneration.BeanForReport.getBean(");
                    dynamicExpressionBuilder.append(reportElement.getParent().getParent().getModuleName()).append(", ");
                    dynamicExpressionBuilder.append(reportElement.getParent().getParent().getDocumentName()).append(", ");
                    dynamicExpressionBuilder.append("$P{ID})");
                }
                if (reportElement.getElementWidth() != null) {
                    dynamicExpressionBuilder.append(reportElement.getElementWidth().toString()).append(", ");
                    dynamicExpressionBuilder.append(reportElement.getElementWidth().toString()).append(", ");
                }
                // TODO vertical sizing - for now assume square based on pixelWidth
                dynamicExpressionBuilder.append(" (org.skyve.metadata.user.User) modules.design.reportgeneration.BeanForReport.getUser())");
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
}
