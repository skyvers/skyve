/**
 * Action contracts, interceptor hooks, observer callbacks, and controller infrastructure
 * for Skyve server-side behaviour.
 *
 * <h2>Action types</h2>
 * <ul>
 *   <li>{@link org.skyve.metadata.controller.ServerSideAction} — the primary action contract;
 *       executed when a button or action widget is activated in the browser. Returns a
 *       {@link org.skyve.metadata.controller.ServerSideActionResult} that carries the
 *       (potentially replaced) bean back to the view.</li>
 *   <li>{@link org.skyve.metadata.controller.DownloadAction} — a two-phase download contract:
 *       {@code prepare} validates and primes the download in one request;
 *       {@code download} streams the file content in a subsequent request.</li>
 *   <li>{@link org.skyve.metadata.controller.UploadAction} — processes an uploaded file
 *       after the browser POSTs it to Skyve.</li>
 *   <li>{@link org.skyve.metadata.controller.BizExportAction} — produces a
 *       {@link org.skyve.bizport.BizPortWorkbook} for spreadsheet-based data export.</li>
 *   <li>{@link org.skyve.metadata.controller.BizImportAction} — consumes a
 *       {@link org.skyve.bizport.BizPortWorkbook} and collects validation problems into an
 *       {@link org.skyve.domain.messages.UploadException}.</li>
 * </ul>
 *
 * <h2>Transfer objects</h2>
 * <ul>
 *   <li>{@link org.skyve.metadata.controller.Download} — wraps the file name, MIME type,
 *       content disposition, and content source (byte array, {@link java.io.File}, or
 *       {@link org.skyve.metadata.controller.WebFileInputStream}).</li>
 *   <li>{@link org.skyve.metadata.controller.Upload} — carries the uploaded file name,
 *       MIME type, and stream to the action method.</li>
 *   <li>{@link org.skyve.metadata.controller.WebFileInputStream} — a lifecycle-managed
 *       {@link java.io.InputStream} that Skyve closes only after the content has been
 *       written to the HTTP response.</li>
 * </ul>
 *
 * <h2>Extension points</h2>
 * <ul>
 *   <li>{@link org.skyve.metadata.controller.Interceptor} — an abstract class providing
 *       before/after hooks for every Skyve lifecycle event (save, delete, validate,
 *       action, render, etc.). Extend and register via the customer XML to intercept
 *       framework calls without modifying Bizlets.</li>
 *   <li>{@link org.skyve.metadata.controller.Observer} — an interface for receiving
 *       application-lifecycle events (startup, shutdown, login, logout, backup, restore).</li>
 *   <li>{@link org.skyve.metadata.controller.Customisations} — configures UI defaults such
 *       as widget text alignment, column widths, and export formats.</li>
 * </ul>
 *
 * <h2>Threading model</h2>
 * Action implementations must be stateless or thread-confined. Skyve instantiates a new
 * action class per request, so instance fields are safe within a single invocation but
 * must not be shared across requests. {@code Interceptor} and {@code Observer} instances
 * are long-lived singletons; guard any mutable state they carry with appropriate synchronisation.
 */
package org.skyve.metadata.controller;
