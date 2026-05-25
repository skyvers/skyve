/**
 * SPI base classes for client-side code generators (Flutter, React).
 *
 * <p>This package defines the renderer abstractions that each client-specific generator
 * sub-package implements to traverse the Skyve view metadata and emit client-specific
 * source code:
 * <ul>
 *   <li>{@link org.skyve.impl.generate.client.ClientViewRenderer} — extends
 *       {@link org.skyve.impl.generate.ViewRenderer} with client-code generation
 *       helpers: binding resolution, converter lookup, and the recursive component
 *       rendering pipeline.
 *   <li>{@link org.skyve.impl.generate.client.ComponentRenderer} — SPI for rendering
 *       individual view components (widgets) to a {@link RenderedComponent} tree.
 *   <li>{@link org.skyve.impl.generate.client.LayoutRenderer} — SPI for rendering
 *       layout containers (VBox, HBox, Tab, etc.).
 *   <li>{@link org.skyve.impl.generate.client.RenderedComponent} — mutable tree node
 *       that accumulates the generated text fragments for one UI component, supporting
 *       indentation, child nesting, and after-text.
 * </ul>
 *
 * <p>Concrete implementations live in the {@code flutter} and {@code react}
 * sub-packages.
 *
 * @see org.skyve.impl.generate.client.flutter
 * @see org.skyve.impl.generate.client.react
 */
package org.skyve.impl.generate.client;
