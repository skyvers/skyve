package org.skyve.impl.sail.execution;

import org.skyve.metadata.sail.language.step.context.PushEditContext;

/**
 * Marker interface for edit context generation in the UI test framework.
 * <p>
 * Implementations encapsulate parameters required to perform generation on different UI technologies.
 */
public interface GenerateEditContext {

	PushEditContext pushEditContext();
}
