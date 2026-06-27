/**
 * JAXB-annotated metadata classes for deserialising view XML files — root and
 * action list.
 *
 * <p>This package contains the top-level view-binding classes:
 * <ul>
 *   <li>{@code ViewMetaData} — JAXB root element ({@code <view>}); holds the view
 *       type, title, widget tree, actions section, and access declarations.
 *   <li>{@code Actions} — the {@code <actions>} container within a view; holds
 *       the ordered list of action metadata objects.
 * </ul>
 *
 * <p>Widget and container element bindings are in
 * {@link org.skyve.impl.metadata.view} and its sub-packages.
 * Action element bindings are in
 * {@link org.skyve.impl.metadata.repository.view.actions}.
 * Access element bindings are in
 * {@link org.skyve.impl.metadata.repository.view.access}.
 *
 * @see org.skyve.metadata.view
 */
package org.skyve.impl.metadata.repository.view;
