package org.skyve.metadata.customer;

import org.skyve.metadata.NamedMetaData;

public interface CustomerRole extends NamedMetaData {
	public String getDescription();
	public String getDocumentation();
}
