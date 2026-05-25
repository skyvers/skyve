/**
 * Defines the view metadata model for Skyve edit and list screens.
 *
 * <p>The central type is {@link org.skyve.metadata.view.View}, which is the top-level
 * container for all UI metadata associated with a document screen. Views are typed by
 * {@link org.skyve.metadata.view.View.ViewType} (list, create, edit, pick, params).
 *
 * <p>This package also contains mixin interfaces that are composed throughout the view
 * widget hierarchy:
 * <ul>
 *   <li>{@link org.skyve.metadata.view.Disableable} &mdash; conditional disable/enable via
 *       named document conditions.</li>
 *   <li>{@link org.skyve.metadata.view.Invisible} &mdash; conditional show/hide via
 *       named document conditions.</li>
 *   <li>{@link org.skyve.metadata.view.Editable} &mdash; explicit editable override.</li>
 *   <li>{@link org.skyve.metadata.view.Filterable} &mdash; declarative pre-filter parameters
 *       for list/lookup widgets.</li>
 *   <li>{@link org.skyve.metadata.view.Parameterizable} &mdash; binding parameters passed
 *       to child views or actions.</li>
 *   <li>{@link org.skyve.metadata.view.FormattedText} &mdash; built-in or custom formatter
 *       for displayed values.</li>
 *   <li>{@link org.skyve.metadata.view.TextOutput} &mdash; HTML escape and sanitisation
 *       settings for text rendering.</li>
 * </ul>
 *
 * <p>{@link org.skyve.metadata.view.Action} represents a button or toolbar action on a
 * view, which may be a framework implicit action or a custom
 * {@link org.skyve.metadata.controller.ServerSideAction} implementation.
 *
 * @see org.skyve.metadata.view.View
 * @see org.skyve.metadata.view.Action
 */
package org.skyve.metadata.view;
