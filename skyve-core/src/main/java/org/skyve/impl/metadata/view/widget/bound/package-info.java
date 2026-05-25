/**
 * View widgets bound to a document attribute, excluding input and tabular specialisations.
 *
 * <p>This package contains widgets that display or link to a single binding value:
 * <ul>
 *   <li>{@code AbstractBound} — base class for all bound widgets; holds the binding
 *       expression and visibility/disabled conditions.
 *   <li>{@code Label} — displays the label text associated with a binding.
 *   <li>{@code ProgressBar} — renders a numeric binding as a progress bar.
 *   <li>{@code ZoomIn} — renders a link that navigates to the detail view of an
 *       association binding.
 *   <li>{@code ParameterImpl} — a name–value parameter passed to a widget.
 * </ul>
 *
 * <p>Input widgets (widgets that accept user input) are in the {@code input} sub-package.
 * Tabular widgets (data grids, list grids) are in the {@code tabular} sub-package.
 *
 * @see org.skyve.impl.metadata.view.widget.bound.input
 * @see org.skyve.impl.metadata.view.widget.bound.tabular
 * @see org.skyve.metadata.view.widget.bound
 */
package org.skyve.impl.metadata.view.widget.bound;
