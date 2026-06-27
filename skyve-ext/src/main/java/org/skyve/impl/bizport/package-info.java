/**
 * Data file import and export (BizPort) adapters for Skyve applications.
 *
 * <p>This package provides the infrastructure for reading data from external
 * files (CSV, Excel) and writing Skyve domain objects, and for generating
 * Excel reports from domain queries:
 * <ul>
 *   <li>{@code AbstractDataFileLoader} — base class for all data-file loaders,
 *       supplying binding resolution, validation, and error-collection logic.
 *   <li>{@code CSVLoader} — reads CSV files via SuperCSV and delegates to
 *       {@code AbstractDataFileLoader} for domain object population.
 *   <li>{@code POISheetLoader} — reads Apache POI {@code Sheet} instances.
 *   <li>{@code POISheetGenerator} — generates styled Excel
 *       ({@code .xlsx}) worksheets from a document's query results.
 *   <li>{@code POIWorkbook} / {@code POISheet} — POI-backed workbook and sheet
 *       wrappers used by {@code StandardGenerator} / {@code StandardLoader}.
 *   <li>{@code StandardGenerator} / {@code StandardLoader} — high-level
 *       BizPort generator and loader that drive the full import/export cycle.
 *   <li>{@code DataFileField} / {@code DataFileExportField} — column-level
 *       configuration describing how a binding maps to a file column.
 * </ul>
 */
package org.skyve.impl.bizport;
