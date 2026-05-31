package org.skyve.impl.generate.jasperreports;

import java.util.List;

import org.skyve.CORE;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.module.query.MetaDataQueryColumn;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;

/**
 * Generates a JasperReports design from a Skyve named document query,
 * mapping query columns to report fields.
 */
public class QueryReportDesignGenerator extends ReportDesignGenerator {
    /**
     * Indicates that query reports do not support collection subreport generation.
     *
     * @return Never returns normally.
     * @throws UnsupportedOperationException Always.
     */
    @Override
    protected QueryReportDesignGenerator getSubreportGenerator() {
        throw new UnsupportedOperationException("Subreports are not supported in query reports.");
    }

    /**
     * Adds report fields for query columns that map to attributes on the driving document.
     *
     * @param design The design being populated.
     */
    @Override
    protected void addFields(DesignSpecification design) {
        super.addFields(design);

        final MetaDataQueryDefinition queryDefinition = design.getModule().getNullSafeMetaDataQuery(design.getQueryName());

        final List<? extends Attribute> documentAttributes = design.getDocument().getAttributes();
        for (MetaDataQueryColumn queryColumn : queryDefinition.getColumns()) {
            final Attribute attribute = documentAttributes.stream()
                    .filter(a -> a.getName().equals(queryColumn.getBinding()))
                    .findFirst()
                    .orElse(null);
            if (attribute != null) {
                final ReportField fld = fieldFromAttribute(design, CORE.getCustomer(), design.getDocument(), attribute, new StringBuilder(), new StringBuilder());
                design.getFields().add(fld);
            }
        }
    }

    /**
     * Creates one detail band per non-collection query field with a label/value pair.
     *
     * @param design The design being populated.
     */
    @Override
    protected void addBands(DesignSpecification design) {
        super.addBands(design);

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
                int valueWidth = design.getColumnWidth().intValue() - labelWidth.intValue();

                detail = Renderer.addElement(detail, type, a.getName(), valueExpression, null, null, top, labelWidth, Integer.valueOf(valueWidth),
                		null, null, alignment, null, null, null, null, null);

                design.getBands().add(detail);
            }
        }
    }
}
