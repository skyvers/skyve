package org.skyve.metadata.view.widget.bound;

import org.skyve.impl.metadata.view.event.EventSource;

/**
 * Mixin interface for view widgets that are bound to a document attribute.
 *
 * <p>The binding is a dot-separated Skyve binding expression (e.g.
 * {@code "contact.name"}) that is resolved against the current document bean at
 * render time to read or write the widget value.
 *
 * <p>Extends {@link EventSource} to associate client-side or server-side event
 * handlers with the widget.
 *
 * @see Parameter
 */
public interface Bound extends EventSource {
	/**
	 * Returns the Skyve binding expression for this widget.
	 *
	 * @return the binding path; may be {@code null} for unbound widgets
	 */
	public String getBinding();
	
	/**
	 * Sets the Skyve binding expression for this widget.
	 *
	 * @param binding  the binding path to set; may be {@code null}
	 */
	public void setBinding(String binding);
}
