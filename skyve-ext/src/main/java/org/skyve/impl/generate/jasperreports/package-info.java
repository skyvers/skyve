/**
 * JasperReports design generators for Skyve document, list, query, and view reports.
 *
 * <p>The abstract {@code ReportDesignGenerator} base class is extended by document,
 * list, query, and view specialisations. Supporting classes
 * ({@code DesignSpecification}, {@code ReportBand}, {@code ReportElement},
 * {@code ReportField}, {@code ReportParameter}, {@code ReportVariable}) hold the
 * intermediate design state used to emit a JasperReports {@code .jrxml} file.
 *
 * <p>{@code JasperReportRenderer} / {@code Renderer} drive the compile-fill-export
 * pipeline. {@code ReportViewVisitor} extracts field and parameter descriptors from
 * view metadata.
 */
package org.skyve.impl.generate.jasperreports;
