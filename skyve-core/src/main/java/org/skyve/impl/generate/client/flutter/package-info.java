/**
 * Flutter/Dart client-code generator for Skyve views.
 *
 * <p>This package produces Flutter/Dart widget code from Skyve view metadata. It
 * implements the client renderer SPIs defined in
 * {@link org.skyve.impl.generate.client}:
 * <ul>
 *   <li>{@link org.skyve.impl.generate.client.flutter.FlutterGenerator} — entry point;
 *       orchestrates generation across all modules, documents, and view types.
 *   <li>{@link org.skyve.impl.generate.client.flutter.FlutterViewRenderer} — base
 *       renderer that wires together layout and component renderers.
 *   <li>{@link org.skyve.impl.generate.client.flutter.FlutterEditView},
 *       {@link org.skyve.impl.generate.client.flutter.FlutterListView},
 *       {@link org.skyve.impl.generate.client.flutter.FlutterCalendarView},
 *       {@link org.skyve.impl.generate.client.flutter.FlutterMapView},
 *       {@link org.skyve.impl.generate.client.flutter.FlutterTreeView} — view-type
 *       specific renderers.
 *   <li>{@link org.skyve.impl.generate.client.flutter.FlutterLayoutRenderer},
 *       {@link org.skyve.impl.generate.client.flutter.FlutterComponentRenderer} — handle
 *       containers and individual widgets respectively.
 *   <li>{@link org.skyve.impl.generate.client.flutter.FlutterRouting} — generates
 *       Flutter navigation/routing boilerplate.
 *   <li>{@link org.skyve.impl.generate.client.flutter.FlutterGeneratorException} —
 *       unchecked exception thrown when Flutter code generation cannot proceed.
 * </ul>
 *
 * @see org.skyve.impl.generate.client
 */
package org.skyve.impl.generate.client.flutter;
