/**
 * Records what a user does in the browser as a SAIL script: the gate that decides whether a session may record, the
 * session's recording, the endpoint the browser recorder sends its steps to, and the builder that turns the steps into
 * an {@link org.skyve.metadata.sail.language.Automation} and readable XML. See docs/sail.md, "Recording".
 *
 * <p>Types in this package are internal implementation details for the web module.
 */
package org.skyve.impl.sail.recording;
