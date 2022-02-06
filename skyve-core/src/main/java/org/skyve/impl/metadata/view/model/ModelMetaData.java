package org.skyve.impl.metadata.view.model;

import org.skyve.metadata.SerializableMetaData;

public interface ModelMetaData extends SerializableMetaData {
	public int getModelIndex();
	public void setModelIndex(int modelIndex);
}
