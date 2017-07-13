package org.skyve.metadata;

import java.util.Map;

/**
 * Allows properties in key/value form to decorate existing meta-data for extension purposes.
 */
public interface DecoratedMetaData {
	public Map<String, String> getProperties();
}
