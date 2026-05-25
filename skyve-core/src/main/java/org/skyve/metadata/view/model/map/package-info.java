/**
 * Custom map view model SPI for supplying geographic features to Skyve map widgets.
 *
 * <p>{@link org.skyve.metadata.view.model.map.MapModel} is the primary SPI: subclass it
 * and implement {@code getResult(Geometry)} to return features for the current viewport.
 *
 * <p>Key types:
 * <ul>
 *   <li>{@link org.skyve.metadata.view.model.map.MapModel} — abstract base; implement
 *       {@code getResult(Geometry)} to return map items for the viewport.</li>
 *   <li>{@link org.skyve.metadata.view.model.map.MapResult} — container of map items
 *       and optional bounding geometry returned by a model.</li>
 *   <li>{@link org.skyve.metadata.view.model.map.MapItem} — a single placeable item
 *       on the map with geometry, colour, and optional hyperlink.</li>
 *   <li>{@link org.skyve.metadata.view.model.map.MapFeature} — a GeoJSON-style feature
 *       with properties for marker display.</li>
 * </ul>
 */
package org.skyve.metadata.view.model.map;
