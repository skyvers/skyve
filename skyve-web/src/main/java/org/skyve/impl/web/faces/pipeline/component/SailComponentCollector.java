package org.skyve.impl.web.faces.pipeline.component;

/**
 * Marks a {@link ComponentBuilder} that records the components of a view for SAIL, rather than producing them.
 * <p>
 * SAIL rebuilds views in the test JVM to find the client ids of widgets. A list grid that names its own
 * component builder is built by a fresh builder outside the view's chain, so the view renderer chains the
 * view's builders after it, but only when the chain contains a collector like this one.
 */
public interface SailComponentCollector {
	// marker
}
