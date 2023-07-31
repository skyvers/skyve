package org.skyve.metadata.view;

import org.skyve.metadata.FormatterName;

/**
 * MetaData interface for elements that support declarative Skyve formatting.
 */
public interface FormattedText {
	FormatterName getFormatterName();
	String getCustomFormatterName();
}
