/**
 * SAIL steps for managing the user session lifecycle.
 *
 * <p>Session steps control authentication state during automation execution:
 * <ul>
 *   <li>{@link org.skyve.metadata.sail.language.step.interaction.session.Login} — authenticates
 *       with the application using the specified username and password, optionally specifying
 *       a customer name for multi-tenant deployments.
 *   <li>{@link org.skyve.metadata.sail.language.step.interaction.session.Logout} — invalidates
 *       the current session and returns the executor to an unauthenticated state.
 * </ul>
 *
 * <p>A well-formed SAIL script begins with a {@code Login} step and ends with a
 * {@code Logout} step. Omitting {@code Logout} may leave residual session state
 * in test runtimes that reuse browser sessions.
 *
 * @see org.skyve.metadata.sail.language.step.interaction
 */
package org.skyve.metadata.sail.language.step.interaction.session;
