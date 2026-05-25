/**
 * Core web-context abstractions used by Skyve's web layer.
 *
 * <p>{@code AbstractWebContext} is the base class for the thread-local web context
 * that tracks the current conversation state, cached beans, and web-session binding
 * during request processing. {@code SortParameterImpl} is a value type representing
 * a column sort parameter (binding + sort direction) extracted from an HTTP request.
 *
 * @see org.skyve.web.WebContext
 */
package org.skyve.impl.web;
