/**
 * Custom list view model SPI for supplying data to Skyve list widgets.
 *
 * <p>{@link org.skyve.metadata.view.model.list.ListModel} is the primary SPI: subclass
 * it (or one of the concrete bases) and reference the class from a view's
 * {@code modelName} attribute to drive a {@code listGrid} or {@code listRepeater} widget
 * with application-defined data.
 *
 * <p>Key types:
 * <ul>
 *   <li>{@link org.skyve.metadata.view.model.list.ListModel} — abstract base; implement
 *       {@code getColumns()} and {@code fetch()}.</li>
 *   <li>{@link org.skyve.metadata.view.model.list.InMemoryListModel} — in-memory variant;
 *       implement {@code getRows()} instead.</li>
 *   <li>{@link org.skyve.metadata.view.model.list.DocumentQueryListModel} — wraps a
 *       {@link org.skyve.persistence.DocumentQuery}.</li>
 *   <li>{@link org.skyve.metadata.view.model.list.Filter} — composable filter predicate.</li>
 *   <li>{@link org.skyve.metadata.view.model.list.Page} — paged result set.</li>
 * </ul>
 */
package org.skyve.metadata.view.model.list;
