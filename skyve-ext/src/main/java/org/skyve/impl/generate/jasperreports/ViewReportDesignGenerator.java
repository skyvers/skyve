package org.skyve.impl.generate.jasperreports;

import org.skyve.CORE;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.module.ModuleImpl;
import org.skyve.impl.metadata.view.ViewImpl;
import org.skyve.metadata.view.View;

/**
 * Generates a JasperReports design from a Skyve view definition, producing a
 * layout that mirrors the view's sections and widget arrangement.
 */
public class ViewReportDesignGenerator extends ReportDesignGenerator {
    private ReportViewVisitor visitor;

    /**
     * Creates a generator that lazily creates its own view visitor.
     */
    public ViewReportDesignGenerator() { }

    /**
     * Creates a generator using a caller-supplied visitor.
     *
     * @param visitor The visitor used to extract fields and layout bands.
     */
    public ViewReportDesignGenerator(ReportViewVisitor visitor) {
        this.visitor = visitor;
    }

    /**
     * Returns the current visitor instance.
     *
     * @return The configured visitor, or {@code null} when not initialised.
     */
    public ReportViewVisitor getVisitor() {
        return visitor;
    }

    /**
     * Replaces the visitor used to traverse the source view.
     *
     * @param visitor The visitor to use for subsequent generation calls.
     */
    public void setVisitor(ReportViewVisitor visitor) {
        this.visitor = visitor;
    }

    /**
     * Creates a sibling generator for collection subreports.
     *
     * @return A new view-based generator instance.
     */
    @Override
    protected ViewReportDesignGenerator getSubreportGenerator() {
        return new ViewReportDesignGenerator();
    }

    /**
     * Adds fields discovered by visiting the configured view definition.
     *
     * @param design The design to populate.
     */
    @Override
    protected void addFields(DesignSpecification design) {
        super.addFields(design);

        initialiseVisitorIfNull(design);

        // Our visitor is responsible for adding fields to the design.
        visitor.setDesign(design);
        visitor.visit();
    }

    /**
     * Adds detail bands produced by the view visitor layout pass.
     *
     * @param design The design to populate.
     */
    @Override
    protected void addBands(DesignSpecification design) {
        super.addBands(design);

        initialiseVisitorIfNull(design);

        if (!visitor.isVisited()) {
            visitor.visit();
        }

        design.getBands().addAll(visitor.getDetailBands());
    }

    /**
     * Builds a title band for view-defined reports.
     *
     * @param design The design being generated.
     * @return The title band.
     */
    @Override
    protected ReportBand createTitleBand(DesignSpecification design) {
        ReportBand title = super.createTitleBand(design);

        // construct title band
        if (DesignSpecification.ReportType.report.equals(design.getReportType())) {
            title = Renderer.addElement(title, ReportElement.ElementType.textField, "title", Renderer.renderBoundMessage(design, visitor.getViewTitle()), null, null,
            		Integer.valueOf(0), Integer.valueOf(0), design.getColumnWidth(), Integer.valueOf(design.getDefaultElementHeight().intValue() * 2),
                    null, ReportElement.ElementAlignment.center, Boolean.TRUE, null, null, null, null);

            if (Boolean.TRUE.equals(design.getSectionBorderTop())
                    || Boolean.TRUE.equals(design.getSectionBorderLeft())
                    || Boolean.TRUE.equals(design.getSectionBorderBottom())
                    || Boolean.TRUE.equals(design.getSectionBorderRight())) {

                // TODO implement borders as lines
                title = Renderer.addElement(title, ReportElement.ElementType.border, null, null, null, null,
                		Integer.valueOf(0), Integer.valueOf(0), design.getColumnWidth(), Integer.valueOf(design.getDefaultElementHeight().intValue() * 2),
                        null, null, null, null, design.getSectionTitleForeground(), design.getSectionTitleBackground(), null);
            }
        }

        return title;
    }

    private void initialiseVisitorIfNull(DesignSpecification design) {
        if (visitor == null) {
            final View view = design.getDocument().getView(design.getUxui(), CORE.getCustomer(), View.ViewType.edit.toString());
            visitor = new ReportViewVisitor(this, (CustomerImpl) CORE.getCustomer(), (ModuleImpl) design.getModule(), (DocumentImpl) design.getDocument(), (ViewImpl) view, design.getUxui());
        }
    }
}
