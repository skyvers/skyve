/**
 * Internal implementation classes for the Skyve view metadata model.
 *
 * <p>This package is the root of the view implementation hierarchy. The view model
 * describes the layout and behaviour of a document's edit and list views as declared
 * in {@code .xml} view metadata files. Key responsibilities of this package and its
 * sub-packages:
 * <ul>
 *   <li>JAXB-annotated implementation classes that are populated when an XML view file
 *       is unmarshalled.
 *   <li>Runtime view representations used by renderers to produce the user-interface
 *       output (Faces, REST, Flutter, etc.).
 *   <li>View visitor infrastructure for traversing the view element tree.
 * </ul>
 *
 * <p>The sub-package hierarchy mirrors the logical structure of a view:
 * <ul>
 *   <li>{@code component} — named, reusable view fragments.
 *   <li>{@code container} — layout containers (box, tab pane, collapsible, sidebar).
 *   <li>{@code container.form} — form-specific layout (form, row, column, item).
 *   <li>{@code event} — client-side event sources and server-side event actions.
 *   <li>{@code model} — model metadata for chart and other data-driven widgets.
 *   <li>{@code reference} — navigation targets and action references.
 *   <li>{@code widget} — unbound display widgets (blurb, button, image, link, map).
 *   <li>{@code widget.bound} — bound widgets (label, progress bar, zoom-in).
 *   <li>{@code widget.bound.input} — bound input widgets (text field, combo, checkbox, etc.).
 *   <li>{@code widget.bound.tabular} — tabular bound widgets (data grid, list grid, repeaters).
 * </ul>
 *
 * @see org.skyve.metadata.view
 */
package org.skyve.impl.metadata.view;
