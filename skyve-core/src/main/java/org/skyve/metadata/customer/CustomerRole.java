package org.skyve.metadata.customer;

import org.skyve.metadata.DecoratedMetaData;
import org.skyve.metadata.NamedMetaData;

public interface CustomerRole extends NamedMetaData, DecoratedMetaData {
	public String getDescription();
	public String getDocumentation();
}
