package org.skyve.metadata;

public interface PersistentMetaData extends SerializableMetaData {
	public long getLastModifiedMillis();
}
