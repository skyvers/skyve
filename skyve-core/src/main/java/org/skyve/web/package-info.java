/**
 * Web-tier contracts used by Skyve views, actions, and background processing.
 *
 * <p>The key types here are:
 * <ul>
 *   <li>{@link org.skyve.web.WebContext} — the per-conversation state holder, giving
 *       server-side actions access to the current bean, message display, and background
 *       task launching.</li>
 *   <li>{@link org.skyve.web.BackgroundTask} — the interface for short-lived view-bound
 *       background operations launched from a {@code WebContext}.</li>
 *   <li>{@link org.skyve.web.WebAction} — enum of the URL action codes that correspond
 *       to Skyve's standard view modes (list, edit, calendar, tree, map).</li>
 *   <li>{@link org.skyve.web.UserAgentType} — enum distinguishing phone, tablet, and
 *       desktop agents, driving UX/UI selection and responsive layout.</li>
 *   <li>{@link org.skyve.web.SortParameter} — the sort column + direction pair used in
 *       list and query requests.</li>
 * </ul>
 */
package org.skyve.web;
