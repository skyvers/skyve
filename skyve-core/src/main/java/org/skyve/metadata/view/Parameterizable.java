package org.skyve.metadata.view;

import java.util.List;

import org.skyve.metadata.SerializableMetaData;
import org.skyve.metadata.view.widget.bound.Parameter;

/**
 * Mixin interface for view elements that accept declarative binding parameters.
 *
 * <p>Parameters allow a parent view to pass binding values into a child view,
 * action, or widget at the point of invocation. Each {@link org.skyve.metadata.view.widget.bound.Parameter}
 * maps a source binding expression to a destination binding in the target context.
 *
 * @see Filterable
 */
public interface Parameterizable extends SerializableMetaData {
	/**
	 * Returns the list of binding parameters for this element.
	 *
	 * @return a non-{@code null} list of parameters; may be empty
	 */
	public List<Parameter> getParameters();
}
