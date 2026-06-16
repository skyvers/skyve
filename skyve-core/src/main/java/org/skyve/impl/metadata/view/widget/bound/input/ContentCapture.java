package org.skyve.impl.metadata.view.widget.bound.input;

import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlType;

/**
 * Upload/capture affordance requested for managed content and upload actions.
 *
 * <p>Each value maps directly to the XML {@code capture} attribute on
 * {@link ContentUpload} and upload actions. Browser support is a progressive
 * enhancement; unsupported values are still valid metadata contracts.
 */
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE)
public enum ContentCapture {
	/**
	 * Requests the default file picker without a capture hint.
	 */
	none,

	/**
	 * Requests still-image camera capture where the client supports it.
	 */
	camera,

	/**
	 * Requests video capture where the client supports it.
	 */
	video,

	/**
	 * Allows either normal file selection or supported capture sources.
	 */
	all
}
