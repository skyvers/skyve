package org.skyve.web;

import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlType;

/**
 * URL action codes that identify which Skyve view mode a request targets.
 *
 * <p>These single-character values appear in the {@code a} query parameter
 * of Skyve URLs and determine which rendered view type the web tier selects:
 * <ul>
 *   <li>{@code l} — list view</li>
 *   <li>{@code c} — calendar view</li>
 *   <li>{@code t} — tree view</li>
 *   <li>{@code m} — map view</li>
 *   <li>{@code e} — edit (form) view</li>
 * </ul>
 */
@XmlType(namespace = XMLMetaData.COMMON_NAMESPACE)
public enum WebAction {
	l, // list
	c, // calendar
	t, // tree
	m, // map
	e // edit
}
