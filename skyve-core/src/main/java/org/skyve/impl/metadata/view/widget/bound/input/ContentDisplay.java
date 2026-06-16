package org.skyve.impl.metadata.view.widget.bound.input;

import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlType;

/**
 * Presentation mode for a managed-content view widget.
 *
 * <p>Each value maps directly to the XML {@code display} attribute on
 * {@link ContentUpload}. Runtime renderers may treat {@link #auto} as a
 * request to choose an appropriate presentation from stored content metadata.
 */
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE)
public enum ContentDisplay {
	/**
	 * Requests renderer-selected presentation based on the stored content kind.
	 */
	auto,

	/**
	 * Presents content as a downloadable or navigable link.
	 */
	link,

	/**
	 * Presents content as an image.
	 */
	image,

	/**
	 * Presents content as a video.
	 */
	video
}
