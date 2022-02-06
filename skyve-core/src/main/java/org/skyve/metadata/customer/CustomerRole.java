package org.skyve.metadata.customer;

import org.skyve.metadata.NamedMetaData;
import org.skyve.metadata.SerializableMetaData;

public interface CustomerRole extends NamedMetaData, SerializableMetaData {
	public String getDescription();
	public String getDocumentation();
}
