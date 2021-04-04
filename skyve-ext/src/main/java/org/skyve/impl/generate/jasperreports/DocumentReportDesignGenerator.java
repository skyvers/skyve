package org.skyve.impl.generate.jasperreports;

import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.document.Document;

public class DocumentReportDesignGenerator extends ReportDesignGenerator {

    @Override
    protected DocumentReportDesignGenerator getSubreportGenerator() {
        return new DocumentReportDesignGenerator();
    }

    @Override
    protected void addFields(DesignSpecification design) {
        super.addFields(design);

        final Customer customer = design.getCustomer();
        final Document document = design.getDocument();
        for (Attribute a : document.getAttributes()) {
            if (!a.isDeprecated() && (a.isPersistent() || DesignSpecification.Mode.bean.equals(design.getMode()))) {

                ReportField fld = fieldFromAttribute(design, customer, document, a, new StringBuilder(), new StringBuilder());
                design.getFields().add(fld);
            }
        }
    }

    @Override
    protected void addBands(DesignSpecification design) {
        super.addBands(design);

        ReportBand columnHeader = new ReportBand();
        columnHeader.setBandType(ReportBand.BandType.columnHeader);
        columnHeader.setName(ReportBand.BandType.columnHeader.toString());
        columnHeader.setParent(design);

        // grid layout headers
        if (DesignSpecification.ReportType.subreport.equals(design.getReportType())) {

            for (ReportField a : design.getFields()) {

                if (!Attribute.AttributeType.collection.name().equals(a.getSkyveType()) && !Boolean.TRUE.equals(a.getImplicit())) {

                    ReportElement.ElementType type = ReportElement.ElementType.staticText;
                    String valueExpression = a.getDisplayName();
                    ReportElement.ElementAlignment alignment = ReportElement.ElementAlignment.left;

                    if (Boolean.TRUE.equals(design.getRenderLabelAsTextFields()) || Boolean.TRUE.equals(design.getDynamicFlow())) {
                        type = ReportElement.ElementType.textField;
                        valueExpression = "\"" + a.getDisplayName() + "\"";
                    }
                    if (Boolean.TRUE.equals(a.getIncludeTotal())) {
                        alignment = ReportElement.ElementAlignment.right;
                    }
                    columnHeader = Renderer.addElement(columnHeader, type, a.getName(), valueExpression, null, null, Integer.valueOf(0), null, null, null, null, alignment,
                            design.getBoldLabels(), null, null, null, null);
                }
            }
            columnHeader.spreadElements();
        }
        design.getBands().add(columnHeader);

        // add a detail band for every non collection attribute
        switch (design.getReportType()) {
            case subreport:

                ReportBand subReportDetail = new ReportBand();
                subReportDetail.setBandType(ReportBand.BandType.detail);
                subReportDetail.setParent(design);

                // grid layout data
                for (ReportField srf : design.getFields()) {
                    if (!Boolean.TRUE.equals(srf.getImplicit())) {
                        ReportElement.ElementAlignment alignment = ReportElement.ElementAlignment.left;
                        String valueExpression = "$F{" + srf.getName() + "}";

                        subReportDetail = Renderer.addElement(subReportDetail, ReportElement.ElementType.textField, srf.getName(), valueExpression, null, null, Integer.valueOf(0),
                                Integer.valueOf(0), design.getColumnWidth(), null, null, alignment, null, null, null, null, null);
                    }
                }
                subReportDetail.spreadElements();

                design.getBands().add(subReportDetail);

                break;

            case report:
                for (ReportField a : design.getFields()) {
                    if (!Attribute.AttributeType.collection.name().equals(a.getSkyveType()) && !Boolean.TRUE.equals(a.getImplicit())) {
                        ReportBand detail = new ReportBand();
                        detail.setBandType(ReportBand.BandType.detail);
                        detail.setParent(design);
                        detail.setName(a.getName());

                        ReportElement.ElementType type = ReportElement.ElementType.staticText;
                        String valueExpression = a.getDisplayName();
                        ReportElement.ElementAlignment alignment = ReportElement.ElementAlignment.left;
                        Integer labelWidth = Integer.valueOf(design.getDefaultElementHeight().intValue() * 5);
                        Integer top = Integer.valueOf(0);
                        Integer labelLeft = Integer.valueOf(0);

                        // add label
                        if (Boolean.TRUE.equals(design.getRenderLabelAsTextFields()) || Boolean.TRUE.equals(design.getDynamicFlow())) {
                            type = ReportElement.ElementType.textField;
                            valueExpression = "\"" + a.getDisplayName() + "\"";
                        }
                        if (Boolean.TRUE.equals(a.getIncludeTotal())) {
                            alignment = ReportElement.ElementAlignment.right;
                        }

                        detail = Renderer.addElement(detail, type, a.getName(), valueExpression, null, null, top, labelLeft, labelWidth, null, null, alignment,
                                design.getBoldLabels(), null, null, null, null);

                        // add value
                        type = ReportElement.ElementType.textField;
                        valueExpression = "$F{" + a.getName() + "}";
                        int valueWidth = design.getColumnWidth().intValue() - labelWidth;

                        detail = Renderer.addElement(detail, type, a.getName(), valueExpression, null, null, top, labelWidth, valueWidth, null, null, alignment, null,
                                null, null, null, null);

                        design.getBands().add(detail);
                    }
                }

                // add detail bands for subReports
                for (DesignSpecification sub : design.getSubReports()) {

                    ReportBand subBand = new ReportBand();
                    subBand.setBandType(ReportBand.BandType.detail);
                    subBand.setParent(design);
                    subBand.setName(sub.getName());

                    ReportElement eS = new ReportElement(ReportElement.ElementType.subreport,
                            sub.getField().getName(),
                            null,
                            0,
                            0,
                            design.getColumnWidth(),
                            null);
                    eS.setReportFileName(sub.getName());
                    eS.setParent(subBand);

                    eS.setCollectionDocumentName(sub.getDocumentName());

                    eS.setElementHeight(design.getDefaultElementHeight());
                    eS.setDynamicFlow(design.getDynamicFlow());

                    eS.setParent(subBand);
                    subBand.getElements().add(eS);
                    design.getBands().add(subBand);
                }

                break;
        }

        ReportBand columnFooter = new ReportBand();
        columnFooter.setBandType(ReportBand.BandType.columnFooter);
        columnFooter.setParent(design);

        // grid footers - totals expression
        if (DesignSpecification.ReportType.subreport.equals(design.getReportType())) {
            for (ReportField a : design.getFields()) {
                if (!Attribute.AttributeType.collection.name().equals(a.getSkyveType()) && !Boolean.TRUE.equals(a.getImplicit())) {

                    String valueExpression = "";
                    ReportElement.ElementAlignment alignment = null;
                    if (Boolean.TRUE.equals(a.getIncludeTotal())) {
                        valueExpression = "$V{" + a.getName() + "}";
                        alignment = ReportElement.ElementAlignment.right;
                    }

                    columnFooter = Renderer.addElement(columnFooter, ReportElement.ElementType.textField, a.getName(), valueExpression, null, null, 0, 0,
                            0, null, null, alignment, design.getBoldLabels(), null, null, null, null);
                }
            }
            columnFooter.spreadElements();
        }
        design.getBands().add(columnFooter);
    }

    @Override
    protected ReportBand createTitleBand(DesignSpecification design) {
        ReportBand title = super.createTitleBand(design);

        if (DesignSpecification.ReportType.report.equals(design.getReportType())) {
            title = Renderer.addElement(title, ReportElement.ElementType.textField, "bizKey", Renderer.renderBoundMessage(design, "{bizKey}"), null, null, 0,
                    0, design.getColumnWidth(),
                    design.getDefaultElementHeight() * 2, null, ReportElement.ElementAlignment.center, Boolean.TRUE, null, null, null, null);

            if (Boolean.TRUE.equals(design.getSectionBorderTop())
                    || Boolean.TRUE.equals(design.getSectionBorderLeft())
                    || Boolean.TRUE.equals(design.getSectionBorderBottom())
                    || Boolean.TRUE.equals(design.getSectionBorderRight())) {

                // TODO implement borders as lines
                title = Renderer.addElement(title, ReportElement.ElementType.border, null, null, null, null, 0, 0, design.getColumnWidth(),
                        design.getDefaultElementHeight() * 2, null, null, null, null, null, null, null);
            }
        }

        return title;
    }
}
