package org.skyve.impl.generate.jasperreports;

import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.module.ModuleImpl;
import org.skyve.impl.metadata.view.ViewImpl;
import org.skyve.metadata.view.View;

public class ViewReportDesignGenerator extends ReportDesignGenerator {

    private ReportViewVisitor visitor;

    public ViewReportDesignGenerator() { }

    public ViewReportDesignGenerator(ReportViewVisitor visitor) {
        this.visitor = visitor;
    }

    public ReportViewVisitor getVisitor() {
        return visitor;
    }

    public void setVisitor(ReportViewVisitor visitor) {
        this.visitor = visitor;
    }

    @Override
    protected ViewReportDesignGenerator getSubreportGenerator() {
        return new ViewReportDesignGenerator();
    }

    @Override
    protected void addFields(DesignSpecification design) {
        super.addFields(design);

        initialiseVisitorIfNull(design);

        // Our visitor is responsible for adding fields to the design.
        visitor.setDesign(design);
        visitor.visit();
    }

    @Override
    protected void addBands(DesignSpecification design) {
        super.addBands(design);

        initialiseVisitorIfNull(design);

        if (!visitor.isVisited()) {
            visitor.visit();
        }

        // construct title band
        if (DesignSpecification.ReportType.report.equals(design.getReportType())) {
            ReportBand title = new ReportBand();
            title.setBandType(ReportBand.BandType.title);
            title.setName("Title");
            title.setParent(design);

            title = Renderer.addElement(title, ReportElement.ElementType.textField, "title", Renderer.renderBoundMessage(design, visitor.getViewTitle()), null, null,
                    0, 0, design.getColumnWidth(),
                    design.getDefaultElementHeight() * 2, null, null, Boolean.TRUE, null, null, null, null);

            if (Boolean.TRUE.equals(design.getSectionBorderTop())
                    || Boolean.TRUE.equals(design.getSectionBorderLeft())
                    || Boolean.TRUE.equals(design.getSectionBorderBottom())
                    || Boolean.TRUE.equals(design.getSectionBorderRight())) {

                // TODO implement borders as lines
                title = Renderer.addElement(title, ReportElement.ElementType.border, null, null, null, null, 0, 0, design.getColumnWidth(),
                        design.getDefaultElementHeight() * 2, null, null, null, null, design.getSectionTitleForeground(), design.getSectionTitleBackground(), null);
            }

            design.getBands().add(title);
        }

        design.getBands().addAll(visitor.getDetailBands());
    }

    private void initialiseVisitorIfNull(DesignSpecification design) {
        if (visitor == null) {
            final View view = design.getDocument().getView(design.getUxui(), design.getCustomer(), View.ViewType.edit.toString());
            visitor = new ReportViewVisitor(this, (CustomerImpl) design.getCustomer(), (ModuleImpl) design.getModule(), (DocumentImpl) design.getDocument(), (ViewImpl) view);
        }
    }
}
