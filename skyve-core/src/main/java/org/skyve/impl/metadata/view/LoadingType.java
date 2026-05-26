package org.skyve.impl.metadata.view;

import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlType;

/**
 * JAXB-annotated enumeration of lazy-loading strategies for sub-view
 * widgets (panels, grids, etc.).
 *
 * <p>Controls when content is fetched: eagerly on initial page load, lazily
 * on first expansion, or lazily on every activation.
 */
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE)
public enum LoadingType {
	eager, lazy
}
