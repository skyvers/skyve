package org.skyve.impl.metadata.view;

import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlType;

/**
 * JAXB-annotated enumeration of display modes for a content download widget.
 *
 * <p>Controls whether a binary content field is presented as an inline thumbnail
 * image or as a file-download link.
 */
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE)
@SuppressWarnings("java:S115") // Suppress "Constant names should comply with a naming convention" as these are not constants but enum values
public enum DownloadAreaType {
	resources, content;
}
