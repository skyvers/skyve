/**
 * Navigation reference types that resolve to Skyve views, reports, and external URLs.
 *
 * <p>References are used in view metadata to specify navigation targets for buttons,
 * links, and zoom-in widgets. The hierarchy is:
 * <ul>
 *   <li>{@code Reference} — base class for all reference types; carries the optional
 *       target window/frame identifier.
 *   <li>{@code EditViewReference} — navigates to the edit view of a document instance.
 *   <li>{@code DefaultListViewReference} — navigates to a document's default list view.
 *   <li>{@code QueryListViewReference} — navigates to a list view driven by a named query.
 *   <li>{@code ActionReference} — invokes a named action class on the current bean.
 *   <li>{@code ImplicitActionReference} — invokes one of the framework implicit actions
 *       (save, delete, cancel, etc.).
 *   <li>{@code ReportReference} — opens a Jasper report.
 *   <li>{@code ContentReference} — opens a stored content item.
 *   <li>{@code ExternalReference} — navigates to a static external URL.
 *   <li>{@code ResourceReference} — navigates to a framework resource URL.
 *   <li>{@code ReferenceTarget} — enumeration of window/frame open targets.
 *   <li>{@code ReferenceProcessor} — resolves a reference at runtime to a URL or action.
 * </ul>
 *
 * @see org.skyve.metadata.view
 */
package org.skyve.impl.metadata.view.reference;
