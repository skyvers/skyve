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
@SuppressWarnings("java:S115") // Suppress "Constant names should comply with a naming convention" as these are not constants but enum values
public enum LoadingType {
	eager, lazy
}
