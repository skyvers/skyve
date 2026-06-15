/**
 * Unbound display-only view widget implementations.
 *
 * <p>Widgets in this package render content that is not bound to a document attribute:
 * <ul>
 *   <li>{@code Blurb} — a rich-text or expression-evaluated HTML fragment.
 *   <li>{@code Button} — a push button that triggers an action or navigation reference.
 *   <li>{@code DialogButton} — a button variant that triggers a confirmation dialog.
 *   <li>{@code Chart} — embeds a chart widget driven by a {@code ModelMetaData} definition.
 *   <li>{@code DynamicImage} — renders an image whose source is computed at runtime.
 *   <li>{@code StaticImage} — renders a static image resource.
 *   <li>{@code Link} — a hyperlink navigating to a reference target.
 *   <li>{@code MapDisplay} — renders an interactive map view.
 *   <li>{@code Spacer} — inserts whitespace to control layout alignment.
 *   <li>{@code FilterParameterImpl} — carries a list-filter parameter for a widget.
 * </ul>
 *
 * <p>Bound widgets (those tied to a document attribute) live in the
 * {@code widget.bound} and {@code widget.bound.input} sub-packages.
 *
 * @see org.skyve.impl.metadata.view.widget.bound
 * @see org.skyve.metadata.view.widget
 */
package org.skyve.impl.metadata.view.widget;
