package org.skyve.metadata;

import java.io.Serializable;

/**
 * Marker interface for metadata that can be serialized.
 *
 * <p>All runtime Skyve metadata must be serializable so that instances can be
 * distributed across cluster nodes, stored in session state, and transferred
 * in background-job payloads.
 *
 * @see MetaData
 */
public interface SerializableMetaData extends MetaData, Serializable {
	// Nothing to see here
}
