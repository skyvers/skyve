package org.skyve.impl.generate.jasperreports;

import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.metadata.module.query.MetaDataQueryColumn;

import java.util.List;

public class QueryReportDesignGenerator extends ReportDesignGenerator {

    @Override
    protected QueryReportDesignGenerator getSubreportGenerator() {
        throw new UnsupportedOperationException("Subreports are not supported in query reports.");
    }

    @Override
    protected void addFields(DesignSpecification design) {
        super.addFields(design);

        final MetaDataQueryDefinition queryDefinition = design.getModule().getMetaDataQuery(design.getQueryName());

        final List<? extends Attribute> documentAttributes = design.getDocument().getAttributes();
        for (MetaDataQueryColumn queryColumn : queryDefinition.getColumns()) {
            final Attribute attribute = documentAttributes.stream()
                    .filter(a -> a.getName().equals(queryColumn.getBinding()))
                    .findFirst()
                    .orElse(null);
            if (attribute != null) {
                final ReportField fld = fieldFromAttribute(design, design.getCustomer(), design.getDocument(), attribute, new StringBuilder(), new StringBuilder());
                design.getFields().add(fld);
            }
        }
    }

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
                Integer labelWidth = design.getDefaultElementHeight() * 5;
                Integer top = 0;
                Integer labelLeft = 0;

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
                int valueWidth = design.getColumnWidth() - labelWidth;

                detail = Renderer.addElement(detail, type, a.getName(), valueExpression, null, null, top, labelWidth, valueWidth, null, null, alignment, null,
                        null, null, null, null);

                design.getBands().add(detail);
            }
        }
    }
}
