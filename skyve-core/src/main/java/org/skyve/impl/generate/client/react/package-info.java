/**
 * React and React Native client-code generators for Skyve views.
 *
 * <p>This package produces React/TypeScript (PrimeReact) and React Native source code
 * from Skyve view metadata. It implements the client renderer SPIs defined in
 * {@link org.skyve.impl.generate.client}:
 * <ul>
 *   <li>{@link org.skyve.impl.generate.client.react.ReactGenerator} — entry point;
 *       orchestrates generation across all modules, documents, and view types for both
 *       React and React Native targets.
 *   <li>{@link org.skyve.impl.generate.client.react.ReactViewRenderer},
 *       {@link org.skyve.impl.generate.client.react.PrimeReactViewRenderer},
 *       {@link org.skyve.impl.generate.client.react.ReactNativeViewRenderer} — base
 *       view renderers for each target platform.
 *   <li>{@link org.skyve.impl.generate.client.react.ReactEditView},
 *       {@link org.skyve.impl.generate.client.react.ReactListView},
 *       {@link org.skyve.impl.generate.client.react.ReactCalendarView},
 *       {@link org.skyve.impl.generate.client.react.ReactMapView},
 *       {@link org.skyve.impl.generate.client.react.ReactTreeView} — view-type specific
 *       renderers for web React.
 *   <li>{@link org.skyve.impl.generate.client.react.ReactNativeEditView} — edit view
 *       renderer for React Native.
 *   <li>{@link org.skyve.impl.generate.client.react.PrimeReactLayoutRenderer},
 *       {@link org.skyve.impl.generate.client.react.PrimeReactComponentRenderer},
 *       {@link org.skyve.impl.generate.client.react.ReactNativeLayoutRenderer},
 *       {@link org.skyve.impl.generate.client.react.ReactNativeComponentRenderer} —
 *       platform-specific layout and widget renderers.
 *   <li>{@link org.skyve.impl.generate.client.react.ReactRouter},
 *       {@link org.skyve.impl.generate.client.react.ReactNativeRouter} — generate
 *       React Router and React Navigation routing code.
 *   <li>{@link org.skyve.impl.generate.client.react.ReactComponent} — value type
 *       carrying the generated component code for a single widget.
 * </ul>
 *
 * @see org.skyve.impl.generate.client
 */
package org.skyve.impl.generate.client.react;
